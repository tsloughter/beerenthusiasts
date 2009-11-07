%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created :  5 Nov 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(be_user_server).

-behaviour(gen_server).

%% API
-export([start/2, upload_profile_image/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {username, connection, database, email_address, profile_image}).

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
start(Super, UserName) ->
    supervisor:start_child (Super, [UserName]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
upload_profile_image(Pid, LocalFileName) ->
    gen_server:call(Pid, {upload_profile_image, LocalFileName}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([UserName]) ->
    Connection = couchbeam_server:start_connection_link(),   
    Database = couchbeam_db:create(Connection, UserName),

    {ok, EmailAddress} = db_interface:get_email_address(UserName),
    ProfileImage = digest2str(erlang:md5(wf:clean_lower(EmailAddress))),
    
    {ok, #state{username=UserName, connection=Connection, database=Database, email_address=EmailAddress, profile_image=ProfileImage}}.

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
handle_call({upload_profile_image, LocalFileName}, _From, State) ->
    erls3:put_file(LocalFileName, State#state.profile_image, "public-read"),
    {reply, ok, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
digest2str(Digest) ->
    [[nibble2hex(X bsr 4), nibble2hex(X band 15)] ||
    X <- binary_to_list(Digest)].

-define(IN(X,Min,Max), X >= Min, X =< Max).
nibble2hex(X) when ?IN(X, 0, 9)   -> X + $0;
nibble2hex(X) when ?IN(X, 10, 15) -> X - 10 + $a.
