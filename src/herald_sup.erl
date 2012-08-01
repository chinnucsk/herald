%%%-------------------------------------------------------------------
%%% @author satyamshekhar <>
%%% @copyright (C) 2012, satyamshekhar
%%% @doc
%%%
%%% @end
%%% Created :  1 Aug 2012 by satyamshekhar <>
%%%-------------------------------------------------------------------
-module(herald_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_android/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(APIKey) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [APIKey]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([APIKey]) ->
    lager:debug("Starting Herald"),
    {ok, {{simple_one_for_one, 1000, 3600},
         [{herald_android, 
           {herald_android, start_link, [APIKey]},
           temporary, 2000, worker, []}
         ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_android(RegistrationId, LogId) ->
    lager:debug("Starting herald-android: ~s", [RegistrationId]),
    supervisor:start_child(?MODULE, [RegistrationId, LogId]).
