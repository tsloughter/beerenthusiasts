%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan@beerenthusiasts.org>
%%% @doc
%%% @end
%%% @copyright 2009 Tristan Sloughter
%%%----------------------------------------------------------------
-module(beerenthusiasts_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, login/2, start_user_server/0]).

%% Supervisor callbacks
-export([init/1]).

-include("be_config.hrl").

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
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
login(UserName, Password) ->
    case db_interface:validate_user(UserName, Password) of
        {ok, valid} ->
            case supervisor:start_child(?SERVER, {{user_sup, UserName}, {be_user_sup, start_link, []}, transient, 2000, supervisor, [be_user_sup]}) of
                {ok, Pid} ->
                    be_db_interface:update_last_logged_in(UserName),
                    be_user_server:start(Pid, UserName);
                _ ->
                    ?ERROR_MSG("Unable to start ~p supervisor", [UserName]),
                    {error, "Unable to login"}
            end;
        {error, Reason} ->
            ?INFO_MSG("Incorrect login for ~p", [UserName]),
            {error, Reason}
    end.

start_user_server() ->
    case supervisor:start_child(?SERVER, {erlang:make_ref(), {be_user_sup, start_link, []}, transient, 2000, supervisor, [be_user_sup]}) of
        {ok, Pid} ->
            be_user_server:start(Pid);
        _ ->
            ?ERROR_MSG("Unable to start user server supervisor", []),
            {error, "Unable to start supervisor"}
    end.
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
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
 
    Children = [{beerenthusiasts, {beerenthusiasts, start_link, []},
                 Restart, Shutdown, Type, [beerenthusiasts]},
                {erls3, {erls3, start_link, ["profiles.beerenthusiasts.org"]},
                 Restart, Shutdown, Type, [erls3]},
                {be_user_utils_server, {be_user_utils_server, start_link, []},
                 Restart, Shutdown, Type, [be_user_utils_server]}],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


