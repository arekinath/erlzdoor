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

#if !defined(_GLOBALS_H)
#define _GLOBALS_H

#include <sys/types.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>

#include <zdoor.h>
#include "erl_nif.h"

/* a request within a door */
struct req {
	struct req *next;
	struct door *door;
	ErlNifMutex *lock;
	ErlNifCond *cond;
	char *rep;
	int replen;
};

/* an open door that can receive requests */
struct door {
	struct door *next;
	char *zonename;
	char *service;
	ErlNifPid owner;
	struct req *rlist;
	ErlNifRWLock *rlock;
};

/* a job to be performed in the job thread */
typedef enum { ACT_OPEN, ACT_CLOSE, ACT_QUIT } jobaction_t;
struct job {
	struct job *next;
	jobaction_t action;
	struct door *door;
	ErlNifPid owner;
};

/* globals */
struct gbl {
	struct door *dlist;
	ErlNifRWLock *dlock;

	struct job *jlist;
	ErlNifMutex *jlock;
	ErlNifCond *jcond;
	ErlNifTid jthread;

	ErlNifResourceType *rtype;
};

/* stuff in other c files */
extern struct gbl gbl;

struct job *job_alloc(jobaction_t);
void job_free(struct job *);
void job_insert(struct job *);
void *job_thread(void *);

struct door *door_alloc(void);
void door_free(struct door *);

struct req *req_alloc(void);
void req_free(struct req *);
void req_insert(struct door *, struct req *);
void req_remove(struct door *, struct req *);


#endif
