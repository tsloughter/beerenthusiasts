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
                                true ->
                                    if Amount == 1 ->
                                            lists:flatten(io_lib:format("~p ~s ", [Amount, Type]));
                                       true ->
                                            lists:flatten(io_lib:format("~p ~ss ", [Amount, Type]))
                                    end
                            end
                      end, [{"day", Days}, {"hour", H}, {"minute", 
                                                         if
                                                             Days == 0 , H == 0 -> M+1;
                                                             true -> 0
                                                         end}]),
    R++"ago".

queue() ->
    UserName = wf:get_path_info(),
    case be_user_server:get_queue(wf:session(be_user_server), UserName, 4) of
        {error, not_found} ->
            "";
        {_, _, _, []} ->
            "";
        {_, _, _, Queue} ->
            get_beer_table_listing(Queue);
        [] ->
            ""
    end.
    
submissions() ->
    UserName = wf:get_path_info(),
    case be_user_server:get_brews(wf:session(be_user_server), UserName, 4) of
        {error, not_found} ->
            "";
        {_, _, _, []} ->
            "";
        {_, _, _, Submissions} ->
            get_beer_table_listing(Submissions);
        [] ->
            ""
    end.                                     

favorites() ->
    UserName = wf:get_path_info(),
    case be_user_server:get_favorites(wf:session(be_user_server), UserName, 4) of
        {error, not_found} ->
            "";
        {_, _, _, []} ->
            "";
        {_, _, _, Favorites} ->
            get_beer_table_listing(Favorites);
        [] ->
            ""
    end.

user_comments() ->
    UserName = wf:get_path_info(),
    case be_user_server:get_comments(wf:session(be_user_server), UserName, 4) of
        {error, not_found} ->
            "";
        {_, _, _, []} ->
            "";
        {_, _, _, Comments} ->
            [{FirstDocId, _, _} | _] = Comments,            
            lists:flatmap(fun({_, _, Comment}) ->
                                  Value = couchbeam_doc:get_value("body", Comment),
                                  [#p{class="comment", body=[Value, #br{}]}]
                          end, Comments)++[#panel{class="right", body=[#link{text="View All", url="/web/comments/"++UserName++"?start_docid="++binary_to_list(FirstDocId)++"&rows=10"}]}];
        [] ->
            ""
    end.

ratings() ->
    UserName = wf:get_path_info(),
    case be_user_server:get_ratings(wf:session(be_user_server), UserName, 4) of
        {error, not_found} ->
            "";
        {_, _, _, []} ->
            "";
        {_, _, _, Ratings} ->
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
             #panel{class="right", body=[#link{text="View All", url="/web/ratings/"++UserName++"?start_docid="++binary_to_list(FirstDocId)++"&rows=10"}]}];
        [] ->
            ""
    end.

stats() ->
    UserName = wf:get_path_info(), 

    {ok, Days} = be_user_server:get_days_member_for(UserName),                        
    MemberSince = if
                      Days == 0 ->
                          "1 day";
                      true ->
                          Months = Days/30,
                          if
                              Months < 1 ->
                                  lists:flatten(io_lib:format("~p days", [Days]));
                              true ->
                                  lists:flatten(io_lib:format("~p months", [round(Months)]))
                          end
                  end,
    
    {ok, Profile} = be_user_server:get_profile(wf:session(be_user_server), UserName),
    QueueCount = io_lib:format("~p", [be_user_server:count_queue(wf:session(be_user_server), UserName)]),
    FavoritesCount = io_lib:format("~p", [be_user_server:count_favorites(wf:session(be_user_server), UserName)]),
    SubmissionsCount = io_lib:format("~p", [be_user_server:count_brews(wf:session(be_user_server), UserName)]),
    CommentsCount = io_lib:format("~p", [be_user_server:count_comments(wf:session(be_user_server), UserName)]),
    RatingsCount = io_lib:format("~p", [be_user_server:count_ratings(wf:session(be_user_server), UserName)]),
    
    [
     #h3{text="Stats"},
     #p{ body=[
               "<strong>",
               #span{class="color_black", text=couchbeam_doc:get_value("fullname", Profile)},
               " (", couchbeam_doc:get_value("username", Profile), ") lives in ", couchbeam_doc:get_value("location", Profile)," and has been a member for ", MemberSince, "</strong>",
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

get_beer_table_listing(Beers) ->    
    element(1, lists:mapfoldl(fun({_DocId, _Key, Doc}, Count) ->
                                      {#panel{class=if Count rem 2 == 0 ->
                                                            "cycle_listing";
                                                       true ->
                                                            "grey cycle_listing"
                                                    end,
                                              body=[
                                                    #panel{class="cycle_listing_right",
                                                           body=[
                                                                 #h4{text=couchbeam_doc:get_value("name", Doc)},
                                                                 #panel{class="color_grey",
                                                                        body=[
                                                                           "doing it"]},
                                                                 #panel{class="right color_grey",
                                                                        body=[ 
                                                                               "<strong>27</strong>"]}
                                                                ]},
                                                    #panel{
                                                            body=[
                                                                  #image{style="width: 35px; height: 60px;", image="/beer_rating_big.png"}]}]}, Count+1}
                              end, 0, Beers)).
