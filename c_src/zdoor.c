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

struct gbl gbl;

static int
load_cb(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
	gbl.dlock = enif_rwlock_create("doors");
	gbl.jlock = enif_mutex_create("jobs");
	gbl.jcond = enif_cond_create("job_ready");
	gbl.dlist = NULL;
	gbl.jlist = NULL;

	ErlNifResourceFlags tried;
	gbl.rtype = enif_open_resource_type(env, NULL, "zdoor_req", NULL, ERL_NIF_RT_CREATE, &tried);

	enif_thread_create("zdoor_job_th", &gbl.jthread, job_thread, NULL, NULL);

	return 0;
}

static void
unload_cb(ErlNifEnv *env, void *priv_data)
{
	struct job *j = job_alloc(ACT_QUIT);
	job_insert(j);
}

static ERL_NIF_TERM
req_reply(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct req *req;
	ErlNifBinary repbin;

	if (!enif_get_resource(env, argv[0], gbl.rtype, (void **)&req))
		return enif_make_badarg(env);
	if (!enif_inspect_binary(env, argv[1], &repbin))
		return enif_make_badarg(env);

	enif_mutex_lock(req->lock);

	req->rep = malloc(repbin.size);
	memcpy(req->rep, repbin.data, repbin.size);
	req->replen = repbin.size;

	enif_mutex_unlock(req->lock);
	enif_cond_signal(req->cond);

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
req_info(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct req *req;

	if (!enif_get_resource(env, argv[0], gbl.rtype, (void **)&req))
		return enif_make_badarg(env);

	enif_rwlock_rlock(req->door->rlock);
	enif_mutex_lock(req->lock);

	ERL_NIF_TERM zname = enif_make_string(env, req->door->zonename, ERL_NIF_LATIN1);
	ERL_NIF_TERM service = enif_make_string(env, req->door->service, ERL_NIF_LATIN1);
	ERL_NIF_TERM owner = enif_make_pid(env, &req->door->owner);

	enif_mutex_unlock(req->lock);
	enif_rwlock_runlock(req->door->rlock);

	return enif_make_tuple4(env,
		enif_make_atom(env, "zdoor_req"), zname, service, owner);
}

static ERL_NIF_TERM
job_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct job *j = job_alloc(ACT_OPEN);
	enif_self(env, &j->owner);

	unsigned int len;
	if (!enif_get_list_length(env, argv[0], &len))
		return enif_make_badarg(env);
	j->door->zonename = enif_alloc(len + 1);
	if (enif_get_string(env, argv[0], j->door->zonename, len+1, ERL_NIF_LATIN1) <= 0)
		return enif_make_badarg(env);
	if (!enif_get_list_length(env, argv[1], &len))
		return enif_make_badarg(env);
	j->door->service = enif_alloc(len + 1);
	if (enif_get_string(env, argv[1], j->door->service, len+1, ERL_NIF_LATIN1) <= 0)
		return enif_make_badarg(env);

	enif_self(env, &j->door->owner);

	job_insert(j);

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
job_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char *zonename;
	char *service;
	struct door *d;

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
	enif_rwlock_rlock(gbl.dlock);
	for (d = gbl.dlist; d; d = d->next) {
		if (!strcmp(d->zonename, zonename) && !strcmp(d->service, service)) {
			found = 1;
			break;
		}
	}

	if (d && found) {
		struct job *j = job_alloc(ACT_CLOSE);
		j->door = d;
		enif_self(env, &j->owner);

		enif_rwlock_runlock(gbl.dlock);

		job_insert(j);

		return enif_make_atom(env, "ok");

	} else {
		enif_rwlock_runlock(gbl.dlock);
		return enif_make_tuple2(env,
			enif_make_atom(env, "error"),
			enif_make_atom(env, "not_found"));
	}
}

static ErlNifFunc nif_funcs[] =
{
	{"req_reply", 2, req_reply},
	{"req_info", 1, req_info},
	{"job_open", 2, job_open},
	{"job_close", 2, job_close}
};

ERL_NIF_INIT(zdoor, nif_funcs, load_cb, NULL, NULL, unload_cb)
