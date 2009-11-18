-module (web_ratings).
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
    "User Comments".

body() ->
    UserName = wf:get_path_info(),
    StartDocId = wf:q(start_docid),
    Rows = wf:q(rows),

    Queue = case StartDocId of
                [] ->
                    be_user_server:get_ratings(wf:session(be_user_server), UserName, 2);
                _ ->
                    be_user_server:get_ratings(wf:session(be_user_server), UserName, StartDocId, Rows)
            end,

    io_lib:format("~p", [Queue]).
    
event(_) -> ok.
