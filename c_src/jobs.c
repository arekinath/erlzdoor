/*
%%
%% libzdoor erlang binding
%%
%% Copyright (c) 2013, The University of Queensland
%% Author: Alex Wilson <alex@uq.edu.au>
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%  * Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright notice,
%%    this list of conditions and the following disclaimer in the documentation
%%    and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED  TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR  BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%
*/

#include "globals.h"

struct job *
job_alloc(jobaction_t act)
{
	struct job *j = enif_alloc(sizeof(struct job));
	memset(j, 0, sizeof(*j));
	j->action = act;
	if (act == ACT_OPEN)
		j->door = door_alloc();
	return j;
}

void
job_free(struct job *j)
{
	if (j->door)
		door_free(j->door);
	j->door = NULL;
	enif_free(j);
}

void
job_insert(struct job *j)
{
	enif_mutex_lock(gbl.jlock);
	if (gbl.jlist)
		j->next = gbl.jlist;
	gbl.jlist = j;
	enif_mutex_unlock(gbl.jlock);
	enif_cond_signal(gbl.jcond);
}

static struct zdoor_result *
zdoor_cb(struct zdoor_cookie *cookie, char *argp, size_t argp_sz)
{
	struct door *d;
	struct req *r;
	ErlNifEnv *env = enif_alloc_env();

	/* we kept the struct door in the biscuit */
	d = (struct door *)cookie->zdc_biscuit;

	/* this request */
	r = req_alloc();

	/* take the rlist lock first, then the req lock */
	enif_rwlock_rwlock(d->rlock);
	enif_mutex_lock(r->lock);

	req_insert(d, r);

	enif_rwlock_rwunlock(d->rlock);

	/* make the request into a binary term to put it into enif_send() */
	ErlNifBinary bin;
	enif_alloc_binary(argp_sz, &bin);
	memcpy(bin.data, argp, argp_sz);
	ERL_NIF_TERM binTerm = enif_make_binary(env, &bin);

	/* send a message back to the session owner */
	enif_send(NULL, &d->owner, env,
		enif_make_tuple3(env,
			enif_make_atom(env, "zdoor"),
			enif_make_resource(env, r),
			binTerm));

	/* now wait until the request has been replied to */
	enif_cond_wait(r->cond, r->lock);

	/* convert the reply into a zdoor_result */
	/* we have to use naked malloc() since libzdoor will use free() */
	struct zdoor_result *res = malloc(sizeof(struct zdoor_result));
	res->zdr_size = r->replen;
	res->zdr_data = r->rep;

	r->rep = NULL;
	r->replen = 0;

	/* yes, we have to unlock and re-lock to avoid lock inversion here */
	enif_mutex_unlock(r->lock);

	/* remove and free the struct req */
	enif_rwlock_rwlock(d->rlock);
	enif_mutex_lock(r->lock);
	req_remove(d, r);
	enif_rwlock_rwunlock(d->rlock);
	req_free(r);

	enif_free_env(env);

	return res;
}

/* the async job thread that handles opening/closing of doors */
void *
job_thread(void *arg)
{
	struct zdoor_handle *zhandle;
	int cont = 1;
	int res;

	/* first init the handle */
	zhandle = zdoor_handle_init();

	enif_mutex_lock(gbl.jlock);

	while (cont) {
		struct job *j, *oj;
		while (!gbl.jlist)
			enif_cond_wait(gbl.jcond, gbl.jlock);

		j = gbl.jlist;
		while (j) {
			if (j->action == ACT_OPEN) {
				enif_rwlock_rwlock(gbl.dlock);
				j->door->next = NULL;
				if (gbl.dlist != NULL)
					j->door->next = gbl.dlist;
				gbl.dlist = j->door;
				enif_rwlock_rwunlock(gbl.dlock);

				res = zdoor_open(zhandle, j->door->zonename, j->door->service, j->door, zdoor_cb);

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
				enif_rwlock_rwlock(gbl.dlock);
				enif_rwlock_rwlock(j->door->rlock);

				if (j->door->rlist) {
					enif_rwlock_rwunlock(j->door->rlock);
					enif_rwlock_rwunlock(gbl.dlock);

					ErlNifEnv *env = enif_alloc_env();
					enif_send(NULL, &j->owner, env,
						enif_make_tuple3(env,
							enif_make_atom(env, "zdoor_job"),
							enif_make_atom(env, "close"),
							enif_make_atom(env, "busy")));
					enif_free_env(env);
				} else {
					struct door *d = gbl.dlist;
					for (; d; d = d->next) {
						if (d->next == j->door) break;
					}
					if (d)
						d->next = j->door->next;
					enif_rwlock_rwunlock(gbl.dlock);

					zdoor_close(zhandle, j->door->zonename, j->door->service);
					door_free(j->door);

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

		gbl.jlist = NULL;
	}

	enif_mutex_unlock(gbl.jlock);

	zdoor_handle_destroy(zhandle);

	return NULL;
}
