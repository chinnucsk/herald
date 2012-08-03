%%%-------------------------------------------------------------------
%%% @author satyamshekhar <>
%%% @copyright (C) 2012, satyamshekhar
%%% @doc
%%%
%%% @end
%%% Created :  1 Aug 2012 by satyamshekhar <>
%%%-------------------------------------------------------------------
-module(herald_android).

-behaviour(gen_server).

%% API
-export([start_link/3, push/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(GCM, "https://android.googleapis.com/gcm/send").

-record(state, {
          api_key = undefined, 
          registeration_id = undefined,
          log_id = undefined
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(APIKey, RegistrationId, LogId) ->
    gen_server:start_link(?MODULE, [APIKey, RegistrationId, LogId], []).
push(Device, Message) ->
    gen_server:cast(Device, {push, Message}).
stop(Device) ->
    gen_server:cast(Device, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([APIKey, RegistrationId, LogId]) ->
    lager:debug("~s started herald android: ~s", 
                [LogId, RegistrationId]),
    {ok, #state{
       api_key = APIKey,
       registeration_id = RegistrationId,
       log_id = LogId
      }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    lager:error("Invalid call"),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({push, Message}, State) ->
    send_push(State, Message),
    {noreply, State};
handle_cast(stop, #state{log_id = LogId} = State) ->
    lager:info("~s stopping", [LogId]),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_push(#state{registeration_id = RegistrationId, 
                 api_key = APIKey, log_id = LogId}, Message) ->
    lager:debug("~s sending push: ~p", [LogId, Message]),
    Headers = [{"Authorization", "key=" ++ APIKey}],
    CollapseKey = proplists:get_value(collapse_key, Message),
    Body = "registration_id=" ++ RegistrationId ++ "&collapse_key=" ++
        CollapseKey,
    Request = {?GCM, Headers,
               "application/x-www-form-urlencoded;charset=UTF-8", Body},
    lager:debug("~s Making Request: ~p", [LogId, Request]),
    case httpc:request(post, Request, [], []) of
        {ok, {{_, StatusCode, _}, _ResHeaders, ResBody}} ->
            lager:debug("~s push_response: ~p ~p", 
                        [LogId, StatusCode, ResBody]);
        {error, Reason} ->
            lager:error("~s push_error: ~p", [LogId, Reason])
    end.
