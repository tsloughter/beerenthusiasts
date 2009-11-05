%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created :  5 Nov 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(be_recipes_utils).

-compile(export_all).

get_recipe(Db, Id) ->
    couchbeam_db:open_doc(Db, Id).

save_recipe(Db, Recipe) ->
    couchbeam_db:save_doc(Db, Recipe).
