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
-export([start_link/1, start/2, upload_profile_image/2, update_profile/2, logout/1,
         update_profile/3, get_profile/1, get_profile_image_url/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("be_config.hrl").

-define(SERVER, ?MODULE). 

-record(state, {username, couch_connection, couch_database,
                email_address, profile_image_name, profile_image_url,
                profile}).

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
start_link(UserName) ->
    gen_server:start_link(?MODULE, [UserName], []).

start(Super, UserName) ->
    supervisor:start_child (Super, [UserName]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
upload_profile_image(Pid, LocalFileName) ->
    gen_server:call(Pid, {upload_profile_image, LocalFileName}).

update_profile(Pid, KeyValue) when is_list(KeyValue) ->
    gen_server:call(Pid, {update_profile, KeyValue}).

update_profile(Pid, Key, Value) when is_list(Key), is_list(Value) ->
    update_profile(Pid, list_to_binary(Key), list_to_binary(Value));
update_profile(Pid, Key, Value) when is_binary(Key), is_binary(Value) ->
    gen_server:call(Pid, {update_profile, Key, Value}).

get_profile(Pid) ->
    gen_server:call(Pid, get_profile).

get_profile_image_url(Pid) ->
    gen_server:call(Pid, get_profile_image_url).

get_comments(Pid) ->
    gen_server:call(Pid, get_comments).

get_queue(Pid) ->
    gen_server:call(Pid, get_queue).

get_favorites(Pid) ->
    gen_server:call(Pid, get_favorites).

get_ratings(Pid) ->
    gen_server:call(Pid, get_ratings).

add_to_queue(Pid, Recipe) ->
    gen_server:call(Pid, {add_to_queue, Recipe}).

add_to_favorites(Pid, Recipe) ->
    gen_server:call(Pid, {add_to_favorites, Recipe}).

add_to_favorites(Pid, Recipe, Rating) ->
    gen_server:call(Pid, {add_rating, Recipe, Rating}).

logout(Pid) ->
    gen_server:call(Pid, logout).

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
    Database = couchbeam_db:create(Connection, "profiles"),
    
    {ok, EmailAddress} = db_interface:get_email_address(UserName),
    ProfileImageName = digest2str(erlang:md5(wf:clean_lower(EmailAddress))),

    Profile = couchbeam_db:open_doc(Database, EmailAddress),
    
    {ok, #state{username=UserName, couch_connection=Connection, couch_database=Database, email_address=EmailAddress, profile_image_name=ProfileImageName, profile_image_url=?PROFILE_IMAGE_URL++ProfileImageName, profile=Profile}}.

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
    erls3:put_file(LocalFileName, State#state.profile_image_name, "public-read"),
    {reply, ok, State};
handle_call({update_profile, KeyValue}, _From, State) ->
    UpdatedProfile = couchbeam_doc:extend(KeyValue, State#state.profile),
    couchbeam_db:save_doc(State#state.couch_database, UpdatedProfile),
    {reply, ok, State#state{profile=UpdatedProfile}};
handle_call({update_profile, Key, Value}, _From, State) ->
    UpdatedProfile = couchbeam_doc:extend(Key, Value, State#state.profile),
    couchbeam_db:save_doc(State#state.couch_database, UpdatedProfile),
    {reply, ok, State#state{profile=UpdatedProfile}};
handle_call(get_profile, _From, State) ->
    {reply, {ok, State#state.profile}, State};
handle_call(get_profile_image_url, _From, State) ->
    {reply, {ok, State#state.profile_image_url}, State};
handle_call(logout, _From, State) ->
    {stop, logout, State}.

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
terminate(_Reason, State) ->
    couchbeam_server:close(State#state.couch_connection),
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
