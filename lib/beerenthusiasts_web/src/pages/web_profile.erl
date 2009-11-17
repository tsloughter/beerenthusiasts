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

last_logged_in() ->
    "2 hours ago".

user_comments() ->
    UserName = wf:get_path_info(),
    {_, _, _, Comments} = be_user_server:get_comments(wf:session(be_user_server), UserName, 4),
    lists:flatmap(fun({_, _, Comment}) ->
                          [couchbeam_doc:get_value("body", Comment), #br{}]
                  end, Comments).

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
     #p{},
     %#strong{},
     "<strong>",
     #span{class="color_black", text=couchbeam_doc:get_value("fullname", Profile)},
     " (", couchbeam_doc:get_value("username", Profile), ") lives in ", couchbeam_doc:get_value("location", Profile)," and has been a member for ", couchbeam_doc:get_value("joined", Profile), "</strong>",
	   #p{},
     %<table width="100%">
     #table{ rows =[
                    #tablerow { cells=[
                                       #tablecell { text="BrewQueue" },
                                       #tablecell { align="right", class="color_black", text=QueueCount }
                                      ]},
                    #tablerow { cells=[
                                       #tablecell { text="Favorites" },
                                       #tablecell { align="right", class="color_black", text=FavoritesCount }
                                      ]},
                    #tablerow { cells=[
                                       #tablecell { text="Submissions" },
                                       #tablecell { align="right", class="color_black", text=SubmissionsCount }
                                      ]},
                    #tablerow { cells=[
                                       #tablecell { text="Comments" },
                                       #tablecell { align="right", class="color_black", text=CommentsCount }
                                      ]},
                    #tablerow { cells=[
                                       #tablecell { text="Ratings" },
                                       #tablecell { align="right", class="color_black", text=RatingsCount }
                                      ]}]}
     ].

body() ->
    UserName = wf:get_path_info(),
    {_, _, _, [H,T]} = be_user_server:get_queue(wf:session(be_user_server), UserName, 2),
    {DocId, _, _} = T,
    #link{text="Next", url="/web/queue/"++UserName++"?start_docid="++binary_to_list(DocId)++"&rows=2"}.
    
event(_) -> ok.
