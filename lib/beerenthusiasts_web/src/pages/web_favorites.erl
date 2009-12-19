-module (web_favorites).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() ->
    UserName = wf:get_path_info(),
    case be_user_server:does_user_exist(wf:session(be_user_server), UserName) of
        true ->
            #template {file="/opt/beerenthusiasts/wwwroot/template.html"};
        false ->
            "not a user"
    end.

title() ->
    "User Queue".

body() ->
    UserName = wf:get_path_info(),
    [StartDocId] = wf:q(start_docid),
    Rows = case wf:q(rows) of
               [] ->
                   "10";
               [R] ->
                   R
           end,

    Favorites = case StartDocId of
                [] ->
                    be_user_server:get_favorites(wf:session(be_user_server), UserName, Rows);
                _ ->
                    be_user_server:get_favorites(wf:session(be_user_server), UserName, StartDocId, Rows)
            end,

    io_lib:format("~p", [Favorites]).
    
event(_) -> ok.
