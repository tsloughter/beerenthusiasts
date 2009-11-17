%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(beerenthusiasts).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("couchbeam.hrl").

-define(SERVER, ?MODULE). 

-record(state, {couch_connection}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
init([]) ->
    %Connection = couchbeam_server:start_connection_link(#couchdb_params{host="beerenthusiasts.cloudant.com", username="beerenthusiasts", password="temp4now"}),
    Connection = couchbeam_server:start_connection_link(),    
    
    %create_couch_dbs_and_views(Connection),

    {ok, #state{couch_connection=Connection}}.

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
    Reply = ok,
    {reply, Reply, State}.

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
create_couch_dbs_and_views(Connection) ->
    Recipes = couchbeam_db:open_or_create(Connection, "be_recipes"),
    PersonalRecipesDesignDoc = {[
                         {<<"_id">>, <<"_design/recipes">>},
                         {<<"language">>,<<"javascript">>},
                         {<<"views">>,
                          {[{<<"personal">>,
                             {[{<<"map">>,
                                <<"function (doc) {\n if (doc.type == \"recipe\") {\n emit(doc.email_address, doc);\n}\n}">>
                               }                                 
                              ]}
                            },
                            {<<"favorites">>,
                             {[{<<"map">>,
                                <<"function (doc) {\n if (doc.type == \"recipe\") {\n for (var i in doc.favorites) {\nemit(doc.favorites[i],null);\n}\n}\n}">>
                               }                                 
                              ]}
                            },
                            {<<"queue">>,
                             {[{<<"map">>,
                                <<"function(doc) {\n if (doc.type == \"recipe\") {\n for (var i in doc.queue) {\nemit(doc.queue[i],null);\n}\n}\n}">>
                               }]} 
                            },
                            {<<"ratings">>,
                             {[{<<"map">>,
                                <<"function(doc) {\n if (doc.type == \"rating\") {\n emit (doc.user_id, doc);\n}\n}">>
                               }]} 
                            }
                           ]}
                         }]},    
    couchbeam_db:save_doc(Recipes, PersonalRecipesDesignDoc),

    RatingsDesignDoc = {[
                          {<<"_id">>, <<"_design/ratings">>},
                          {<<"language">>,<<"javascript">>},
                          {<<"views">>,
                          {[{<<"average">>,
                             {[{<<"map">>,
                                <<"function(doc) { if (doc.type == \"rating\") emit(doc.recipe_id, doc.rating); }">>
                               },
                               {<<"reduce">>,
                                <<"function(keys, values) { return (sum(values) / values.length) }">>
                               }]}
                            }]}
                          }]},    
    couchbeam_db:save_doc(Recipes, RatingsDesignDoc),
    
    _Profiles = couchbeam_db:open_or_create(Connection, "be_profiles"),    
    
    Comments = couchbeam_db:open_or_create(Connection, "be_comments"),
    CommentsDesignDoc = {[
                          {<<"_id">>, <<"_design/comments">>},
                          {<<"language">>,<<"javascript">>},
                          {<<"views">>,
                          {[{<<"user">>,
                             {[{<<"map">>,
                                <<"function (doc) {\n emit(doc.email_address, doc);\n}">>
                               }]}
                            },
                            {<<"recipe">>,
                             {[{<<"map">>,
                                <<"function (doc) {\n emit(doc.recipe_id, doc);\n}">>
                               }]}
                            }]}
                          }]},    
    couchbeam_db:save_doc(Comments, CommentsDesignDoc).
