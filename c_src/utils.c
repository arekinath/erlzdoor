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

struct door *
door_alloc(void)
{
	struct door *d = enif_alloc(sizeof(struct door));
	memset(d, 0, sizeof(*d));
	d->rlock = enif_rwlock_create("door->rlock");
	return d;
}

void
door_free(struct door *d)
{
	if (d->zonename)
		enif_free(d->zonename);
	if (d->service)
		enif_free(d->service);
	enif_rwlock_destroy(d->rlock);
	enif_free(d);
}

struct req *
req_alloc(void)
{
	struct req *r = enif_alloc_resource(gbl.rtype, sizeof(struct req));
	memset(r, 0, sizeof(*r));
	r->lock = enif_mutex_create("req->lock");
	r->cond = enif_cond_create("req->cond");
	return r;
}

void
req_free(struct req *r)
{
	if (r->rep)
		free(r->rep);
	r->rep = 0;
	r->next = 0;
	r->door = 0;
	enif_mutex_unlock(r->lock);
	enif_mutex_destroy(r->lock);
	enif_cond_destroy(r->cond);
	enif_release_resource(r);
}

/* needs d->rlock, r->lock */
void
req_insert(struct door *d, struct req *r)
{
	r->door = d;
	if (d->rlist)
		r->next = d->rlist;
	d->rlist = r;
}

/* needs d->rlock, r->lock */
void
req_remove(struct door *d, struct req *r)
{
	struct req *rb = d->rlist;
	for (; rb; rb = rb->next) {
		if (rb->next == r) break;
	}
	if (rb)
		rb->next = r->next;
	enif_rwlock_rwunlock(d->rlock);
}
