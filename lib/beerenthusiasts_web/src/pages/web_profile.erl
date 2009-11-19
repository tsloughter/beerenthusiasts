-module (web_profile).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() ->
    UserName = wf:get_path_info(),
    case be_user_server:does_user_exist(wf:session(be_user_server), UserName) of
        true ->
            #template {file="/opt/beerenthusiasts/wwwroot/be_user_profile.html"};
        false ->
            "not a user"
    end.

title() ->
    "User Profile".

username() -> 
    wf:get_path_info().

fullname() ->
    UserName = wf:get_path_info(),
    {ok, Profile} = be_user_server:get_profile(wf:session(be_user_server), UserName),    
    couchbeam_doc:get_value("fullname", Profile).

last_logged_in() ->
    UserName = wf:get_path_info(),
    {ok, Seconds} = be_user_server:get_last_logged_in(UserName),                        
    {Days, {H, M, _S}} = calendar:seconds_to_daystime(Seconds),
    R = lists:flatmap(fun({Type, Amount}) ->
                            if
                                Amount == 0 -> "";                            
                                true -> lists:flatten(io_lib:format("~p ~s ", [Amount, Type]))
                            end
                    end, [{"days", Days}, {"hours", H}, {"minutes",
                                                         if
                                                             Days ==0 ; H == 0 -> M+1;
                                                             true -> 0
                                                         end}]),
    R++"ago".
                        
user_comments() ->
    UserName = wf:get_path_info(),
    {_, _, _, Comments} = be_user_server:get_comments(wf:session(be_user_server), UserName, 4),
    [{FirstDocId, _, _} | _] = Comments,
    lists:flatmap(fun({_, _, Comment}) ->
                          Value = couchbeam_doc:get_value("body", Comment),
                          [#p{class="comment", body=[Value, #br{}]}]
                  end, Comments)++[#panel{class="right", body=[#link{text="View All", url="/web/comments/"++UserName++"?start_docid="++binary_to_list(FirstDocId)++"&rows=10"}]}].

ratings() ->
    UserName = wf:get_path_info(),
    {_, _, _, Ratings} = be_user_server:get_ratings(wf:session(be_user_server), UserName, 4),
    [{FirstDocId, _, _} | _] = Ratings,
    [#p{body=[
             #table{id="ratings", rows=
                    [#tablerow { cells=lists:map(fun({_, _, Rating}) ->
                                                         #tablecell { text=couchbeam_doc:get_value("recipe_name", Rating)}
                                                 end, Ratings)},
                     #tablerow { cells=lists:map(fun({_, _, Rating}) ->
                                                         #tablecell { body=[
                                                                            case couchbeam_doc:get_value("color", Rating) of
                                                                                <<"light">> ->
                                                                                    #image{image="/beer_rating_big.png"};
                                                                                <<"medium">> ->
                                                                                    #image{image="/beer_rating_big.png"};
                                                                                <<"dark">> ->
                                                                                    #image{image="/beer_rating_big.png"}
                                                                            end
                                                                           ]}
                                                 end, Ratings)},
                     #tablerow { cells=lists:map(fun({_, _, Rating}) ->
                                                         Num = couchbeam_doc:get_value("rating", Rating),
                                                         #tablecell { body=lists:map(fun(_) -> #image{image="/beer_rating_yes.png"} end, lists:seq(1, Num)) ++ lists:map(fun(_) -> #image{image="/beer_rating_no.png"} end, lists:seq(1, 4-Num))} 
                                                 end, Ratings)}]}]}, 
    #panel{class="right", body=[#link{text="View All", url="/web/ratings/"++UserName++"?start_docid="++binary_to_list(FirstDocId)++"&rows=10"}]}].    

stats() ->
    UserName = wf:get_path_info(),
    {ok, Profile} = be_user_server:get_profile(wf:session(be_user_server), UserName),
    QueueCount = io_lib:format("~p", [be_user_server:count_queue(wf:session(be_user_server), UserName)]),
    FavoritesCount = io_lib:format("~p", [be_user_server:count_favorites(wf:session(be_user_server), UserName)]),
    SubmissionsCount = io_lib:format("~p", [be_user_server:count_brews(wf:session(be_user_server), UserName)]),
    CommentsCount = io_lib:format("~p", [be_user_server:count_comments(wf:session(be_user_server), UserName)]),
    RatingsCount = io_lib:format("~p", [be_user_server:count_ratings(wf:session(be_user_server), UserName)]),
    
    [
     #h3{text="Stats"},
     #p{ body=[
               %#strong{},
               "<strong>",
               #span{class="color_black", text=couchbeam_doc:get_value("fullname", Profile)},
               " (", couchbeam_doc:get_value("username", Profile), ") lives in ", couchbeam_doc:get_value("location", Profile)," and has been a member for ", couchbeam_doc:get_value("joined", Profile), "</strong>",
               #p{ body=[
                         %<table width="100%">
                         #table{ rows =[
                                        #tablerow { cells=[
                                                           #tablecell { text="Brew Queue", style="color: #9a9a9a;" },
                                                           #tablecell { align="left", class="color_black", text=QueueCount }
                                                          ]},
                                        #tablerow { cells=[
                                                           #tablecell { text="Favorites", style="color: #9a9a9a;" },
                                                           #tablecell { align="left", class="color_black", text=FavoritesCount }
                                                          ]},
                                        #tablerow { cells=[
                                                           #tablecell { text="Submissions", style="color: #9a9a9a;" },
                                                           #tablecell { align="left", class="color_black", text=SubmissionsCount }
                                                          ]},
                                        #tablerow { cells=[
                                                           #tablecell { text="Comments", style="color: #9a9a9a;" },
                                                           #tablecell { align="left", class="color_black", text=CommentsCount }
                                                          ]},
                                        #tablerow { cells=[
                                                           #tablecell { text="Ratings", style="color: #9a9a9a;" },
                                                           #tablecell { align="left", class="color_black", text=RatingsCount }
                                                          ]}]}
                        ]}]}].

body() ->
    UserName = wf:get_path_info(),
    {_, _, _, [H,T]} = be_user_server:get_queue(wf:session(be_user_server), UserName, 2),
    {DocId, _, _} = T,
    #link{text="Next", url="/web/queue/"++UserName++"?start_docid="++binary_to_list(DocId)++"&rows=2"}.
    
event(_) -> ok.
