#include <sys/types.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>

#include <zdoor.h>

#include "erl_nif.h"

struct erl_zdoor_session {
	struct erl_zdoor_session *next;

	int busy;
	char *zonename;
	char *service;
	ErlNifPid owner;

	ErlNifMutex *lock;
	ErlNifCond *repcond;
	/* allocate rep with plain malloc -- libzdoor will use free */
	uint64_t repsid;
	char *rep;
	int replen;
};

typedef enum { ACT_OPEN, ACT_CLOSE, ACT_QUIT } jobaction_t;
struct erl_zdoor_job {
	struct erl_zdoor_job *next;
	jobaction_t action;
	struct erl_zdoor_session *sess;
	ErlNifPid owner;
};

static struct {
	struct erl_zdoor_session *slist;
	ErlNifRWLock *slock;

	struct erl_zdoor_job *jlist;
	ErlNifMutex *jlock;
	ErlNifCond *jcond;
	ErlNifTid jobThread;

	ErlNifMutex *sidlock;
	uint64_t sid;
} erl_zdoor;

void
session_free(struct erl_zdoor_session *s)
{
	if (s->zonename)
		enif_free(s->zonename);
	if (s->service);
		enif_free(s->service);
	if (s->rep)
		free(s->rep);
	enif_cond_destroy(s->repcond);

	enif_mutex_unlock(s->lock);
	enif_mutex_destroy(s->lock);
	enif_free(s);
}

struct erl_zdoor_session *
session_alloc(void)
{
	struct erl_zdoor_session *s;
	s = enif_alloc(sizeof(struct erl_zdoor_session));
	memset(s, 0, sizeof(struct erl_zdoor_session));
	s->lock = enif_mutex_create("session->lock");
	s->repcond = enif_cond_create("session->repcond");

	return s;
}

static struct zdoor_result *
erl_zdoor_callback(struct zdoor_cookie *cookie, char *argp, size_t argp_sz)
{
	struct erl_zdoor_session *sess;
	int sid = 0;
	ErlNifEnv *env = enif_alloc_env();

	sess = (struct erl_zdoor_session *)cookie->zdc_biscuit;

	enif_mutex_lock(sess->lock);
	while (sess->busy) {
		enif_cond_wait(sess->repcond, sess->lock);
	}
	sess->busy++;

	ErlNifBinary bin;
	enif_alloc_binary(argp_sz, &bin);
	memcpy(bin.data, argp, argp_sz);
	ERL_NIF_TERM binTerm = enif_make_binary(env, &bin);

	enif_mutex_lock(erl_zdoor.sidlock);
	/* never allocate sid 0 (that means no reply) */
	while (!sid)
		sid = erl_zdoor.sid++;
	enif_mutex_unlock(erl_zdoor.sidlock);
	sess->repsid = sid;

	enif_send(NULL, &sess->owner, env,
		enif_make_tuple3(env,
			enif_make_atom(env, "zdoor"),
			enif_make_uint64(env, sid),
			binTerm));

	enif_cond_wait(sess->repcond, sess->lock);

	struct zdoor_result *res = malloc(sizeof(struct zdoor_result));
	res->zdr_size = sess->replen;
	res->zdr_data = sess->rep;
	sess->rep = NULL;
	sess->repsid = 0;

	sess->busy--;
	enif_mutex_unlock(sess->lock);

	enif_free_env(env);

	return res;
}

void *
job_thread(void *arg)
{
	struct zdoor_handle *zhandle;
	int cont = 1;
	int res;

	/* first init the handle */
	zhandle = zdoor_handle_init();

	enif_mutex_lock(erl_zdoor.jlock);

	while (cont) {
		struct erl_zdoor_job *j, *oj;
		while (!erl_zdoor.jlist)
			enif_cond_wait(erl_zdoor.jcond, erl_zdoor.jlock);

		j = erl_zdoor.jlist;
		while (j) {
			if (j->action == ACT_OPEN) {
				enif_rwlock_rwlock(erl_zdoor.slock);
				enif_mutex_lock(j->sess->lock);
				j->sess->next = NULL;
				if (erl_zdoor.slist != NULL)
					j->sess->next = erl_zdoor.slist;
				erl_zdoor.slist = j->sess;
				enif_rwlock_rwunlock(erl_zdoor.slock);

				res = zdoor_open(zhandle, j->sess->zonename, j->sess->service, j->sess, erl_zdoor_callback);

				enif_mutex_unlock(j->sess->lock);

				ErlNifEnv *env = enif_alloc_env();
				ERL_NIF_TERM ret = enif_make_atom(env, "ok");
				switch (res) {
					case ZDOOR_ERROR:
						ret = enif_make_atom(env, "error");
						break;
					case ZDOOR_NOT_GLOBAL_ZONE:
						ret = enif_make_atom(env, "not_global");
						break;
					case ZDOOR_ZONE_NOT_RUNNING:
						ret = enif_make_atom(env, "not_running");
						break;
					case ZDOOR_ZONE_FORBIDDEN:
						ret = enif_make_atom(env, "eperm");
						break;
					case ZDOOR_ARGS_ERROR:
						ret = enif_make_atom(env, "badarg");
						break;
					case ZDOOR_OUT_OF_MEMORY:
						ret = enif_make_atom(env, "enomem");
						break;
				}
				enif_send(NULL, &j->owner, env,
					enif_make_tuple3(env,
						enif_make_atom(env, "zdoor_job"),
						enif_make_atom(env, "open"),
						ret));
				enif_free_env(env);
			} else if (j->action == ACT_CLOSE) {
				enif_rwlock_rwlock(erl_zdoor.slock);
				enif_mutex_lock(j->sess->lock);

				if (j->sess->busy) {
					enif_mutex_unlock(j->sess->lock);
					enif_rwlock_rwunlock(erl_zdoor.slock);

					ErlNifEnv *env = enif_alloc_env();
					enif_send(NULL, &j->owner, env,
						enif_make_tuple3(env,
							enif_make_atom(env, "zdoor_job"),
							enif_make_atom(env, "close"),
							enif_make_atom(env, "busy")));
					enif_free_env(env);
				} else {
					struct erl_zdoor_session *s = erl_zdoor.slist;
					for (; s; s = s->next) {
						if (s->next == j->sess) break;
					}
					if (s)
						s->next = j->sess->next;
					enif_rwlock_rwunlock(erl_zdoor.slock);

					zdoor_close(zhandle, j->sess->zonename, j->sess->service);
					session_free(j->sess);

					ErlNifEnv *env = enif_alloc_env();
					enif_send(NULL, &j->owner, env,
						enif_make_tuple3(env,
							enif_make_atom(env, "zdoor_job"),
							enif_make_atom(env, "close"),
							enif_make_atom(env, "ok")));
					enif_free_env(env);
				}
			} else if (j->action == ACT_QUIT) {
				cont = 0;
			}

			oj = j;
			j = j->next;
			enif_free(oj);
		}

		erl_zdoor.jlist = NULL;
	}

	enif_mutex_unlock(erl_zdoor.jlock);

	zdoor_handle_destroy(zhandle);

	return NULL;
}

static int
load_cb(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
	erl_zdoor.slock = enif_rwlock_create("sessions");
	erl_zdoor.jlock = enif_mutex_create("jobs");
	erl_zdoor.jcond = enif_cond_create("job_ready");
	erl_zdoor.slist = NULL;
	erl_zdoor.jlist = NULL;

	erl_zdoor.sidlock = enif_mutex_create("session request id");
	erl_zdoor.sid = 1;

	enif_thread_create("zdoor_job_th", &erl_zdoor.jobThread, job_thread, NULL, NULL);

	return 0;
}

static void
unload_cb(ErlNifEnv *env, void *priv_data)
{
	struct erl_zdoor_job *j;
	j = enif_alloc(sizeof(*j));
	memset(j, 0, sizeof(*j));
	j->action = ACT_QUIT;

	enif_mutex_lock(erl_zdoor.jlock);
	if (erl_zdoor.jlist)
		j->next = erl_zdoor.jlist;
	erl_zdoor.jlist = j;
	enif_mutex_unlock(erl_zdoor.jlock);
	enif_cond_signal(erl_zdoor.jcond);
}

static ERL_NIF_TERM
sess_reply(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct erl_zdoor_session *s;
	uint64_t sid;
	ErlNifBinary repbin;

	if (!enif_get_uint64(env, argv[0], &sid))
		return enif_make_badarg(env);
	if (!enif_inspect_binary(env, argv[1], &repbin))
		return enif_make_badarg(env);

	enif_rwlock_rlock(erl_zdoor.slock);
	for (s = erl_zdoor.slist; s; s = s->next) {
		enif_mutex_lock(s->lock);
		if (s->repsid == sid)
			break;
		enif_mutex_unlock(s->lock);
	}

	if (s && s->repsid == sid) {
		s->rep = malloc(repbin.size);
		memcpy(s->rep, repbin.data, repbin.size);
		s->replen = repbin.size;

		enif_mutex_unlock(s->lock);
		enif_cond_signal(s->repcond);
		enif_rwlock_runlock(erl_zdoor.slock);

		return enif_make_atom(env, "ok");

	} else {
		enif_rwlock_runlock(erl_zdoor.slock);

		return enif_make_tuple2(env,
			enif_make_atom(env, "error"),
			enif_make_atom(env, "not_found"));
	}

}

static ERL_NIF_TERM
job_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct erl_zdoor_job *j;
	j = enif_alloc(sizeof(*j));
	memset(j, 0, sizeof(*j));
	j->action = ACT_OPEN;
	j->sess = session_alloc();
	enif_self(env, &j->owner);

	unsigned int len;
	if (!enif_get_list_length(env, argv[0], &len))
		return enif_make_badarg(env);
	j->sess->zonename = enif_alloc(len + 1);
	if (enif_get_string(env, argv[0], j->sess->zonename, len+1, ERL_NIF_LATIN1) <= 0)
		return enif_make_badarg(env);
	if (!enif_get_list_length(env, argv[1], &len))
		return enif_make_badarg(env);
	j->sess->service = enif_alloc(len + 1);
	if (enif_get_string(env, argv[1], j->sess->service, len+1, ERL_NIF_LATIN1) <= 0)
		return enif_make_badarg(env);

	enif_self(env, &j->sess->owner);

	enif_mutex_lock(erl_zdoor.jlock);
	if (erl_zdoor.jlist)
		j->next = erl_zdoor.jlist;
	erl_zdoor.jlist = j;
	enif_mutex_unlock(erl_zdoor.jlock);
	enif_cond_signal(erl_zdoor.jcond);

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
job_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char *zonename;
	char *service;
	struct erl_zdoor_session *s;

	unsigned int len;
	if (!enif_get_list_length(env, argv[0], &len))
		return enif_make_badarg(env);
	zonename = enif_alloc(len + 1);
	if (enif_get_string(env, argv[0], zonename, len+1, ERL_NIF_LATIN1) <= 0)
		return enif_make_badarg(env);
	if (!enif_get_list_length(env, argv[1], &len))
		return enif_make_badarg(env);
	service = enif_alloc(len + 1);
	if (enif_get_string(env, argv[1], service, len+1, ERL_NIF_LATIN1) <= 0)
		return enif_make_badarg(env);

	int found = 0;
	enif_rwlock_rlock(erl_zdoor.slock);
	for (s = erl_zdoor.slist; s; s = s->next) {
		enif_mutex_lock(s->lock);
		if (!strcmp(s->zonename, zonename) && !strcmp(s->service, service)) {
			found = 1;
			break;
		}
		enif_mutex_unlock(s->lock);
	}

	if (s && found) {
		struct erl_zdoor_job *j;
		j = enif_alloc(sizeof(*j));
		memset(j, 0, sizeof(*j));
		j->action = ACT_CLOSE;
		j->sess = s;
		enif_self(env, &j->owner);

		enif_mutex_unlock(s->lock);
		enif_rwlock_runlock(erl_zdoor.slock);

		enif_mutex_lock(erl_zdoor.jlock);
		if (erl_zdoor.jlist)
			j->next = erl_zdoor.jlist;
		erl_zdoor.jlist = j;
		enif_mutex_unlock(erl_zdoor.jlock);
		enif_cond_signal(erl_zdoor.jcond);

		return enif_make_atom(env, "ok");

	} else {
		enif_rwlock_runlock(erl_zdoor.slock);
		return enif_make_tuple2(env,
			enif_make_atom(env, "error"),
			enif_make_atom(env, "not_found"));
	}
}

static ErlNifFunc nif_funcs[] =
{
	{"sess_reply", 2, sess_reply},
	{"job_open", 2, job_open},
	{"job_close", 2, job_close}
	/*{"enc_varbind", 1, enc_varbind}*/
};

ERL_NIF_INIT(zdoor, nif_funcs, load_cb, NULL, NULL, unload_cb)
