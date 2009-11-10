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
         update_profile/3, get_profile/1, get_profile_image_url/1, add_to_queue/2,
         add_to_favorites/2, add_rating/3, get_brews/2, get_brews/4,
         get_ratings/2,  get_ratings/4,
         get_favorites/2, get_favorites/4, get_queue/2, get_queue/4,
         get_comments/2, get_comments/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("be_config.hrl").

-define(SERVER, ?MODULE). 

-record(state, {username, couch_connection, profiles_database, recipes_database,
                user_id, email_address, profile_image_name, profile_image_url,
                profile, comments_database}).

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

get_comments(Pid, Rows) ->
    gen_server:call(Pid, {get_comments, Rows}).

get_comments(Pid, StartKey, StartDocId, Rows) ->
    gen_server:call(Pid, {get_comments, StartKey, StartDocId, Rows}).

get_brews(Pid, Rows) ->
    gen_server:call(Pid, {get_brews, Rows}).

get_brews(Pid, StartKey, StartDocId, Rows) ->
    gen_server:call(Pid, {get_brews, StartKey, StartDocId, Rows}).

get_queue(Pid, Rows) ->
    gen_server:call(Pid, {get_queue, Rows}).

get_queue(Pid, StartKey, StartDocId, Rows) ->
    gen_server:call(Pid, {get_queue, StartKey, StartDocId, Rows}).

get_favorites(Pid, Rows) ->
    gen_server:call(Pid, {get_favorites, Rows}).

get_favorites(Pid, StartKey, StartDocId, Rows) ->
    gen_server:call(Pid, {get_favorites, StartKey, StartDocId, Rows}).

get_ratings(Pid, Rows) ->
    gen_server:call(Pid, {get_ratings, Rows}).

get_ratings(Pid, StartKey, StartDocId, Rows) ->
    gen_server:call(Pid, {get_ratings, StartKey, StartDocId, Rows}).

add_to_queue(Pid, RecipeId) ->
    gen_server:call(Pid, {add_to_queue, RecipeId}).

add_to_favorites(Pid, RecipeId) ->
    gen_server:call(Pid, {add_to_favorites, RecipeId}).

add_rating(Pid, Recipe, Rating) ->
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
    ProfilesDatabase = couchbeam_db:open_or_create(Connection, "be_profiles"),
    RecipesDatabase = couchbeam_db:open_or_create(Connection, "be_recipes"),
    CommentsDatabase = couchbeam_db:open_or_create(Connection, "be_comments"),
    
    {ok, EmailAddress} = db_interface:get_email_address(UserName),
    ProfileImageName = digest2str(erlang:md5(wf:clean_lower(EmailAddress))),

    Profile = couchbeam_db:open_doc(ProfilesDatabase, EmailAddress),
    
    {ok, #state{username=UserName, couch_connection=Connection, profiles_database=ProfilesDatabase, recipes_database=RecipesDatabase, comments_database=CommentsDatabase, user_id=EmailAddress, email_address=EmailAddress, profile_image_name=ProfileImageName, profile_image_url=?PROFILE_IMAGE_URL++ProfileImageName, profile=Profile}}.

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
    couchbeam_db:save_doc(State#state.profiles_database, UpdatedProfile),
    {reply, ok, State#state{profile=UpdatedProfile}};
handle_call({update_profile, Key, Value}, _From, State) ->
    UpdatedProfile = couchbeam_doc:extend(Key, Value, State#state.profile),
    couchbeam_db:save_doc(State#state.profiles_database, UpdatedProfile),
    {reply, ok, State#state{profile=UpdatedProfile}};
handle_call(get_profile, _From, State) ->
    {reply, {ok, State#state.profile}, State};
handle_call(get_profile_image_url, _From, State) ->
    {reply, {ok, State#state.profile_image_url}, State};
handle_call({get_comments, Rows}, _From, State) -> 
    {ok, Results} = get_view_results(State#state.comments_database, "comments", "personal", [{"key", State#state.user_id}, {"limit", Rows}]),
    {reply, Results, State};
handle_call({get_comments, StartKey, StartDocId, Rows}, _From, State) -> 
    {ok, Results} = get_view_results(State#state.comments_database, "comments", "personal", [{"key", State#state.user_id}, {"startkey", StartKey}, {"startkey_docid", StartDocId}, {"limit", Rows}]),
    {reply, Results, State};
handle_call({get_brews, Rows}, _From, State) ->
    {ok, Results} = get_view_results(State#state.recipes_database, "recipes", "personal", [{"key", State#state.user_id}, {"limit", Rows}]),
    {reply, Results, State};
handle_call({get_brews, StartKey, StartDocId, Rows}, _From, State) ->
    {ok, Results} = get_view_results(State#state.recipes_database, "recipes", "personal", [{"key", State#state.user_id}, {"startkey", StartKey}, {"startkey_docid", StartDocId}, {"limit", Rows}]),
    {reply, Results, State};
handle_call({get_queue, Rows}, _From, State) ->
    {ok, Results} = get_view_results(State#state.recipes_database, "recipes", "queue", [{"key", State#state.user_id}, {"limit", Rows}]),
    {reply, Results, State};
handle_call({get_queue, StartKey, StartDocId, Rows}, _From, State) ->
    {ok, Results} = get_view_results(State#state.recipes_database, "recipes", "queue", [{"key", State#state.user_id}, {"startkey", StartKey}, {"startkey_docid", StartDocId}, {"limit", Rows}]),
    {reply, Results, State};
handle_call({get_favorites, Rows}, _From, State) ->
    {ok, Results} = get_view_results(State#state.recipes_database, "recipes", "favorites", [{"key", State#state.user_id}, {"limit", Rows}]),
    {reply, Results, State};
handle_call({get_favorites, StartKey, StartDocId, Rows}, _From, State) ->
    {ok, Results} = get_view_results(State#state.recipes_database, "recipes", "favorites", [{"key", State#state.user_id}, {"startkey", StartKey}, {"startkey_docid", StartDocId}, {"limit", Rows}]),
    {reply, Results, State};
handle_call({get_ratings, Rows}, _From, State) ->
    {ok, Results} = get_view_results(State#state.recipes_database, "recipes", "ratings", [{"key", State#state.user_id}, {"limit", Rows}]),
    {reply, Results, State};
handle_call({get_ratings, StartKey, StartDocId, Rows}, _From, State) ->
    {ok, Results} = get_view_results(State#state.recipes_database, "recipes", "ratings", [{"key", State#state.user_id}, {"startkey", StartKey}, {"startkey_docid", StartDocId}, {"limit", Rows}]),
    {reply, Results, State};
handle_call({add_to_queue, RecipeId}, _From, State) ->
    Recipe = couchbeam_db:open_doc(State#state.recipes_database, RecipeId),
    
    NewRecipe = case couchbeam_doc:get_value(<<"queue">>, Recipe) of
                    undefined ->
                        couchbeam_doc:set_value(<<"queue">>, [State#state.user_id], Recipe);
                    Queue ->
                        couchbeam_doc:set_value(<<"queue">>, [State#state.user_id | Queue], Recipe)
                end,
    
    couchbeam_db:save_doc(State#state.recipes_database, NewRecipe),
    
    {reply, ok, State};
handle_call({add_to_favorites, RecipeId}, _From, State) ->
    Recipe = couchbeam_db:open_doc(State#state.recipes_database, RecipeId),
    
    NewRecipe = case couchbeam_doc:get_value(<<"favorites">>, Recipe) of
                    undefined ->
                        couchbeam_doc:set_value(<<"favorites">>, [State#state.user_id], Recipe);
                    Favorites ->
                        couchbeam_doc:set_value(<<"favorites">>, [State#state.user_id | Favorites], Recipe)
                end,
    
    couchbeam_db:save_doc(State#state.recipes_database, NewRecipe),
    
    {reply, ok, State};
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

get_view_results(Database, DesignDoc, Name, Attributes) ->
    ViewPid = couchbeam_db:query_view(Database, {DesignDoc, Name}, Attributes),    
    ParsedResults = couchbeam_view:parse_view(ViewPid),
    couchbeam_view:close_view(ViewPid),
    {ok, ParsedResults}.
