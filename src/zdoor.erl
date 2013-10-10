%%
%% libzdoor erlang binding
%%
%% Copyright (c) 2013, The University of Queensland
%% Author: Alex Wilson <alex@uq.edu.au>
%%

-module(zdoor).

-export([open/2, close/2, reply/2]).
-export([sess_reply/2, job_open/2, job_close/2]).
-on_load(init/0).

init() ->
	SoName = case code:priv_dir(erlzdoor) of
	    {error, bad_name} ->
	        case filelib:is_dir(filename:join(["..", priv])) of
	        true ->
	            filename:join(["..", priv, ?MODULE]);
	        false ->
	            filename:join([priv, ?MODULE])
	        end;
	    Dir ->
	        filename:join(Dir, ?MODULE)
    end,
	ok = erlang:load_nif(SoName, 0).

sess_reply(_Sid, _Bin) ->
	{error, badnif}.

reply(Sid, Bin) ->
	?MODULE:sess_reply(Sid, Bin).

job_open(_Zone, _Service) ->
	{error, badnif}.

open(Zone, Service) ->
	case ?MODULE:job_open(Zone, Service) of
		ok ->
			receive
				{zdoor_job, open, ok} -> ok;
				{zdoor_job, open, Other} -> {error, Other}
			end;
		Other -> Other
	end.

job_close(_Zone, _Service) ->
	{error, badnif}.

close(Zone, Service) ->
	case ?MODULE:job_close(Zone, Service) of
		ok ->
			receive
				{zdoor_job, close, ok} -> ok;
				{zdoor_job, close, Other} -> {error, Other}
			end;
		Other -> Other
	end.
