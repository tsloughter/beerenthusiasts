%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(be_db_interface).

%% API
-compile(export_all).
 
-include_lib("nitrogen/include/wf.inc").

init() ->
    mysql:prepare(add_user_query, 
                  <<"INSERT INTO users (username, email, fullname, password, date_joined, last_logged_in) VALUES (?, ?, ?, PASSWORD(?), DATE(NOW()), NOW())">>),


    mysql:prepare(get_last_logged_in_query, 
                  <<"SELECT round(now() - last_logged_in, 0) FROM users WHERE username=? OR email=?">>),    
    
    mysql:prepare(validate_user_query, 
                  <<"SELECT 1 FROM users WHERE (username=? OR email=?) AND password=PASSWORD(?)">>),

    mysql:prepare(update_last_logged_in_query, 
                  <<"UPDATE users SET last_logged_in=now() WHERE username=?">>),

    %%%%
    %%% Need to remove from friends and other tables....
    %%%
    mysql:prepare(delete_user_query, 
                  <<"DELETE FROM users WHERE username=?">>),
    
    mysql:prepare(is_username_used_query, 
                  <<"SELECT 1 FROM users WHERE username=?">>),
    
    mysql:prepare(is_email_used_query, 
                  <<"SELECT 1 FROM users WHERE email_address=?">>),
    
    mysql:prepare(get_email_address_query, 
                  <<"SELECT email FROM users WHERE username=?">>),

    mysql:prepare(get_user_id_query, 
                  <<"SELECT id FROM users WHERE username=? OR anon_username=?">>),

    mysql:prepare(add_friend_query,
                  <<"INSERT INTO friends (user_id, friend_id) VALUES (?, ?)">>),

    mysql:prepare(remove_friend_query,
                  <<"DELETE FROM friends WHERE user_id=? AND friend_id=?">>),

    mysql:prepare(get_friends_query,
                  <<"SELECT friend_id FROM friends, users WHERE friends.user_id=users.id AND users.username=?">>),

   mysql:prepare(add_friend_request_query, 
                  <<"INSERT IGNORE INTO pending_friends (user_id, pending_friend_id) VALUES (?, ?)">>),

   mysql:prepare(delete_friend_request_query, 
                  <<"DELETE FROM pending_friends WHERE user_id=? AND pending_friend_id=?">>),

    mysql:prepare(get_all_friend_requests_query, 
                  <<"SELECT requester.username FROM pending_friends, users user, users requester WHERE pending_friends.pending_friend_id=user.id AND user.username=? AND requester.id=pending_friends.user_id">>).

add_user(Username, FullName, Email, Password) ->
    case mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, add_user_query, [Username, Email, FullName, Password])
                      end) of
        {atomic, {updated, _}} ->
            ok;
        {aborted, {{error, {mysql_result,[],[],0,0, Reason}}, _}} ->
            {aborted, Reason}
    end.

validate_user(Username, Password) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, validate_user_query, [Username, Username, Password])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            if 
                length (Rows) == 1 -> 
                    {ok, valid};
                true -> 
                    {error, false}
            end;
        _ ->
            {error, false}
    end.    

update_last_logged_in(Username) ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, update_last_logged_in_query, [Username])
                      end).

delete_user(Username) ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, delete_user_query, [Username])
                      end).

is_username_used(Username) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, is_username_used_query, [Username])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            if 
                length (Rows) == 1 ->            
                    false;
                true -> 
                    true
            end;
        _ ->
            true
    end.

is_email_used(Email) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, is_email_used_query, [Email])
                           end) of
        {atomic, {data, MySQLResults}} -> 
            Rows = mysql:get_result_rows(MySQLResults),
            if
                length (Rows) == 1 -> 
                    false;
                true -> 
                    true
            end;
        _ ->
            true
    end.

get_last_logged_in(UserName) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, get_last_logged_in_query, [UserName, UserName])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            if
                length(Rows) == 1 ->
                   {ok, hd(hd(Rows))};
                true ->
                    {error, unknown_user}
            end;
        _ ->
            {error, unknown_user}
    end.

get_email_address(Username) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, get_email_address_query, [Username])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            if
                length(Rows) == 1 ->
                   {ok, hd(hd(Rows))};
                true ->
                    {error, unknown_user}
            end;
        _ ->
            {error, unknown_user}
    end.

add_friend_request(UserName, Friend) -> 
    case mysql:transaction(p1,
                           fun () ->
                                   {ok, UserId} = get_user_id(UserName),
                                   {ok, FriendId} = get_user_id(Friend),
                                   if
                                       UserId =/= FriendId ->
                                           mysql:execute(p1, add_friend_request_query, [UserId, FriendId]);
                                       true ->
                                           failed
                                   end
                           end) of
        {atomic, {updated, {mysql_result,_,_,1,_,_}}} ->
            ok;
        _ ->
            failed
    end.

remove_friend(UserName, Friend) ->
    mysql:transaction(p1,
                      fun () ->
                              {ok, UserId} = get_user_id(UserName),
                              {ok, FriendId} = get_user_id(Friend),
                              mysql:execute(p1, remove_friend_query, [UserId, FriendId])
                      end).

accept_friend_request(UserName, Requester) ->
    mysql:transaction(p1,
                      fun () ->
                              {ok, UserId} = get_user_id(UserName),
                              {ok, RequesterId} = get_user_id(Requester),
                              mysql:execute(p1, add_friend_query, [UserId, RequesterId]),
                              mysql:execute(p1, delete_friend_request_query, [RequesterId, UserId])
                      end).

deny_friend_request(UserName, Requester) ->
    mysql:transaction(p1,
                      fun () ->
                              {ok, UserId} = get_user_id(UserName),
                              {ok, RequesterId} = get_user_id(Requester),
                              mysql:execute(p1, delete_friend_request_query, [RequesterId, UserId])
                      end).

get_all_friend_requests(UserName) ->
    case mysql:transaction(p1,
                           fun () ->
                                   mysql:execute(p1, get_all_friend_requests_query, [UserName])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            Rows;
        _ ->
            {error, "get all friend requests"}
    end.


get_user_id (User) ->
    {data, {mysql_result, _, [[UserID]|_], _, _, _}} = 
        mysql:execute(p1, get_user_id_query, [User, User]),
    {ok, UserID}.
