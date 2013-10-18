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

-module(zdoor).

-export_type([req_ref/0]).
-opaque req_ref() :: -1.

-export([open/2, close/2, reply/2, req_info/1]).
-export([req_reply/2, job_open/2, job_close/2]).
-on_load(init/0).

-include("zdoor.hrl").

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

%% @doc Get information about a pending request
-spec req_info(Req :: req_ref()) -> #zdoor_req{}.
req_info(_Req) ->
	{error, badnif}.

%% @doc Reply to a zdoor request
-spec reply(Req :: req_ref(), ReplyData :: string() | binary()) -> ok | {error, term()}.
reply(Req, Bin) when is_list(Bin) ->
    req_reply(Req, iolist_to_binary(Bin));
reply(Req, Bin) ->
    req_reply(Req, Bin).

req_reply(_Req, _Bin) ->
	{error, badnif}.

%% @internal
job_open(_Zone, _Service) ->
	{error, badnif}.

%% @doc Open a new zone door
-spec open(Zone :: string()|binary(), Service :: string()|binary()) -> ok | {error, term()}.
open(Zone, Service) when is_binary(Zone) ->
    open(binary_to_list(Zone), Service);

open(Zone, Service) when is_binary(Service) ->
    open(Zone, binary_to_list(Service));

open(Zone, Service) ->
	case ?MODULE:job_open(Zone, Service) of
		ok ->
			receive
				{zdoor_job, open, ok} -> ok;
				{zdoor_job, open, Other} -> {error, Other}
			end;
		Other -> Other
	end.

%% @internal
job_close(_Zone, _Service) ->
	{error, badnif}.

%% @doc Close a zone door
-spec close(Zone :: string()|binary(), Service :: string()|binary()) -> ok | {error, term()}.
close(Zone, Service) when is_binary(Zone) ->
    close(binary_to_list(Zone), Service);

close(Zone, Service) when is_binary(Service) ->
    close(Zone, binary_to_list(Service));

close(Zone, Service) ->
	case ?MODULE:job_close(Zone, Service) of
		ok ->
			receive
				{zdoor_job, close, ok} -> ok;
				{zdoor_job, close, Other} -> {error, Other}
			end;
		Other -> Other
	end.
