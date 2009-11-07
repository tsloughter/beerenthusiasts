% From http://code.google.com/p/erls3/

-module(erls3).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         start/0,
         stop/0,
         start_link/1]).

-export([put_file/1, 
         put_file/2,
         put_file/3,
         put_binary_file/2, 
         put_binary_file/3,
         put_binary_file/4,
         check_acl/1,
         get_file_size/1,
         simple_request/3,
         simple_request/4,
         simple_request/5,
         get_file/2,
         get_content_length/1,
         get_date/0,
         get_unix_timestamp/0,
         get_datetime/0,
         sign/1, sign/6,
         test/0,
         list_buckets/0,
         create_bucket/1,
         change_bucket/1,
         list_keys/0,
         get_acl/1,
         set_acl/2,
         make_link/2]).

-include_lib("kernel/include/file.hrl").
-include("s3_config.hrl").

-define(ACCESS_KEY, "0FD12FH1JNXJ0VTDK2R2").
-define(AWS_S3_HOST, "s3.amazonaws.com").
-define(TIMEOUT, infinity).
-define(CHUNK_SIZE, 5120).

-record(state, {bucket = none}).
-include_lib("xmerl/include/xmerl.hrl").


put_file(File_name, Remote_file, Acl) ->
    gen_server:call({global, ?MODULE}, {put_file, File_name, Remote_file, Acl}, ?TIMEOUT).
put_file(File_name, Remote_file) ->
    Acl = "",
    gen_server:call({global, ?MODULE}, {put_file, File_name, Remote_file, Acl}, ?TIMEOUT).
put_file(File_name) ->
    Remote_file = filename:basename(File_name),
    Acl = "",
    gen_server:call({global, ?MODULE}, {put_file, File_name, Remote_file, Acl}, ?TIMEOUT).
put_binary_file(File, File_name, Remote_file, Acl) ->
    gen_server:call({global, ?MODULE}, {put_binary_file, File, File_name, Remote_file, Acl}, ?TIMEOUT).
put_binary_file(File, File_name, Remote_file) ->
    Acl = "",
    gen_server:call({global, ?MODULE}, {put_binary_file, File, File_name, Remote_file, Acl}, ?TIMEOUT).
put_binary_file(File, File_name) ->
    Remote_file = filename:basename(File_name),
    Acl = "",
    gen_server:call({global, ?MODULE}, {put_binary_file, File, File_name, Remote_file, Acl}, ?TIMEOUT).
get_file(Remote_file, Local_file) ->
    gen_server:call({global, ?MODULE}, {get_file, Remote_file, Local_file}, ?TIMEOUT).
list_buckets() ->
    gen_server:call({global, ?MODULE}, {list_buckets}, ?TIMEOUT).
create_bucket(Bucket) ->
    gen_server:call({global, ?MODULE}, {create_bucket, Bucket}, ?TIMEOUT).
change_bucket(Bucket) ->
    gen_server:call({global, ?MODULE}, {change_bucket, Bucket}).
list_keys() ->
    gen_server:call({global, ?MODULE}, {list_keys, ""}, ?TIMEOUT).
get_acl(Resource) ->
    gen_server:call({global, ?MODULE}, {get_acl, Resource}, ?TIMEOUT).
set_acl(Resource, ACL) ->
    gen_server:call({global, ?MODULE}, {set_acl, Resource, ACL}, ?TIMEOUT).
make_link(Expire_time, Resource) ->
    gen_server:call({global, ?MODULE}, {make_link, Expire_time, Resource}).

start_link(Bucket) ->
    % {scope, module_name}, callback_location, args_passed_to_init, gen_server_options)
    gen_server:start_link({global, ?MODULE}, ?MODULE, [Bucket], []).
start() ->
    start_link("DEFAULT_BUCKET").
stop() ->
    gen_server:cast({global, ?MODULE}, stop).

init([Bucket]) ->
    application:start(crypto),
    inets:start(),
    State = #state{bucket = Bucket},
    {ok, State}.

% the http library could be used for this call but I wanted to steam the data from disk
% seems like a waste to open up the whole file and store it in memory
% hence we have to create the http packet manually then pass around the file resource
handle_call({put_file, Local_file, Remote_file, Acl_temp}, From, State) ->
    Acl = check_acl(Acl_temp),
    {File_size, File} = get_file_size(Local_file),
    Date = get_date(),
    Mime = "image/jpeg",
    To_sign = lists:flatten(["PUT\n\n", Mime, "\n", Date, "\nx-amz-acl:", Acl, "\n/", State#state.bucket,"/", Remote_file]),
    Sig = sign(To_sign),
    Payload = lists:flatten([
                             "PUT /", Remote_file, " HTTP/1.1\r\n",
                             "Content-type: ", Mime, "\r\n",
                             "Content-length: ", integer_to_list(File_size), "\r\n",
                             "Host: ", State#state.bucket, ".", ?AWS_S3_HOST, "\r\n",
                             "Date: ", Date, "\r\n",
                             "x-amz-acl: ", Acl, "\r\n",
                             "Authorization: ", auth_header(Sig), "\r\n\r\n"
                            ]),
    spawn(fun() -> send_headers(Payload, File, From) end),
    {reply, ok, State};
handle_call({put_binary_file, File, Local_file, Remote_file, Acl_temp}, From, State) ->
    Acl = check_acl(Acl_temp),
    File_size = size(File),
    Date = get_date(),
    Mime = "image/jpeg",
    To_sign = lists:flatten(["PUT\n\n", Mime, "\n", Date, "\nx-amz-acl:", Acl, "\n/", State#state.bucket,"/", Remote_file]),
    Sig = sign(To_sign),
    Payload = lists:flatten([
                             "PUT /", Remote_file, " HTTP/1.1\r\n",
                             "Content-type: ", Mime, "\r\n",
                             "Content-length: ", integer_to_list(File_size), "\r\n",
                             "Host: ", State#state.bucket, ".", ?AWS_S3_HOST, "\r\n",
                             "Date: ", Date, "\r\n",
                             "x-amz-acl: ", Acl, "\r\n",
                             "Authorization: ", auth_header(Sig), "\r\n\r\n"
                            ]),
    spawn(fun() -> send_headers(Payload, File, From) end),
    {reply, ok, State};
handle_call({get_file, Resource, Local_file}, From, State) ->
    Date = get_date(),
    To_sign = lists:flatten(["GET\n\n\n", Date, "\n/", State#state.bucket, "/", Resource ]),
    Sig = sign(To_sign), 
    Payload = lists:flatten(["GET /", Resource, " HTTP/1.1\r\n",
                             "Host: ", State#state.bucket, ".", ?AWS_S3_HOST, "\r\n",
                             "Date: ", Date, "\r\n",
                             "Authorization: ", auth_header(Sig), "\r\n\r\n"]),
    {ok, Socket} = gen_tcp:connect(?AWS_S3_HOST, 80, [binary, {active, false}, {packet, 0}]),
    gen_tcp:send(Socket, list_to_binary(Payload)),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            % assume we got the a stream decode the packet
            case erlang:decode_packet(http, Data, []) of
                {ok, {http_response, _, Code, _} , Response} when Code =:= 200->
                    %io:format("~s", [binary_to_list(Response)]),
                    Body = get_body(Response),
                    {ok, File} = file:open(Local_file, write),
                    file:write(File, Body),
                    spawn(fun() -> write_to_file(Socket, File, From, get_content_length(Response), size(Body)) end),
                    {reply, ok, State};
                _ ->
                    {reply, packet_error, State}
            end;
        _ ->
            {reply, tcp_error, State}
    end;
% change the current bucket being worked with
handle_call({change_bucket, Bucket}, _From, State) ->
    New_state = State#state{bucket = Bucket},
    {reply, {bucket_changed, Bucket}, New_state};
handle_call({list_buckets}, _From, State) ->
    case erls3:simple_request(get, "/", "", "") of
        {ok, Response} ->
            {XML, _Rest} = xmerl_scan:string(Response), 
            Buckets_xml  = xmerl_xpath:string("//Name/text()", XML),
            Buckets = [Bucket || #xmlText{value = Bucket} <- Buckets_xml],
            {reply, {ok, Buckets}, State};
        {error, Code, Code_string, {_Headers, _Response}} ->
            {reply, {error, Code, Code_string}, State}
    end;
handle_call({create_bucket, Bucket}, _From, State) ->
    case erls3:simple_request(put, "/" ++ Bucket, "", "") of
        {ok, []} ->
            {reply, {ok, Bucket}, State};
        {error, 409, _, _} ->
            {reply, {error, bucket_exists}, State}
    end;
handle_call({list_keys, Resource}, _From, State) ->
    case erls3:simple_request(get, "/" ++ State#state.bucket, Resource, "") of
        {ok, Response} ->
            {XML, _Rest} = xmerl_scan:string(Response),
            Keys_xml  = xmerl_xpath:string("//Key/text()", XML),
            Keys = [Key || #xmlText{value = Key} <- Keys_xml],
            {reply, {ok, Keys}, State};
        {error, Code, Code_string, {_Headers, _Response}} ->
            {reply, {error, Code, Code_string}, State}
	end;
handle_call({get_acl, Resource}, _From, State) ->
    case erls3:simple_request(get, "/" ++ State#state.bucket, "/" ++ Resource, "?acl") of
        {ok, Response} ->
            {XML, _Rest} = xmerl_scan:string(Response),
            Grants_xml = xmerl_xpath:string("//Grant", XML),
            Permissions_list = lists:foldl(fun(Elem, Accum) ->
                                                   [#xmlText{value = Permission}] = xmerl_xpath:string("//Permission/text()", Elem),
                                                   Grantee = xmerl_xpath:string("//Grantee", Elem),
                                                   [{Permission, Grantee} | Accum]
                                           end, [], Grants_xml),
            %Permissions_xml  = xmerl_xpath:string("//Permission/text()", XML),
            %Permissions = [Permission || #xmlText{value = Permission} <- Permissions_xml],
            {reply, {ok, Permissions_list}, State};
        {error, Code, Code_string, {_Headers, _Response}} ->
            {reply, {error, Code, Code_string}, State}
    end;
handle_call({set_acl, Resource, ACL_temp}, _From, State) ->
    ACL = check_acl(ACL_temp),
    case ACL =/= ACL_temp of
        true ->
            {reply, invalid_acl, State};
        false ->
            case simple_request(put, "/" ++ State#state.bucket, "/" ++ Resource, "?acl", [{"x-amz-acl", ACL}]) of
                {ok, []} ->
                    {reply, ok, State};
                {error, Code, Code_string, _} ->
                    {reply, {error, Code, Code_string}, State}
            end
    end;
handle_call({make_link, Expire_time, Resource}, _From, State) ->
    Expires = integer_to_list(Expire_time + get_datetime()),
    To_sign = lists:flatten(["GET\n\n\n", Expires, "\n/", State#state.bucket, "/", Resource ]),
    erlang:display(To_sign),
    Sig = sign(To_sign),
    URL = lists:flatten(["http://", ?AWS_S3_HOST, "/", State#state.bucket, "/", Resource, "?AWSAccessKeyId=", ?ACCESS_KEY, "&Signature=", Sig, "&Expires=", Expires]),
    io:format("~s", [URL]),
    {reply, {url, URL}, State};

handle_call(Request, _From, State) ->
    erlang:display(Request),
    {reply, ignored, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Server Stopped ~n"),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER FUNCTIONS											%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_body(<<>>) ->
    <<>>;
get_body(Data) ->
    case erlang:decode_packet(httph, Data, []) of
        {ok, {http_header, _, _, _, _}, Rest} ->
            get_body(Rest);
        {ok, http_eoh, Rest} ->
            Rest;
        _ ->
            <<>>
                end.

get_content_length(<<>>) ->
    0;
get_content_length(Data) ->
    Length = get_header_value(Data, 'Content-Length'),
    list_to_integer(Length).

get_header_value(<<>>, _) ->
    "";
get_header_value(Data, Header) ->
    case erlang:decode_packet(httph, Data, []) of
        {ok, {http_header, _, Header, _, Raw_value}, _Rest} -> Raw_value;
        {ok, {http_header, _, _, _, _}, Rest} -> get_header_value(Rest, Header);
        {ok, http_eoh, _} -> <<>>
                                 end.


%% functions used to steam a file off the hd
% returns a handler to the file resource and the file size {size, File()}
get_file_size(File_name) ->
    case file:open(File_name, read) of
        {ok, File} ->
            {ok, File_info} = file:read_file_info(File_name),
            {File_info#file_info.size, File};
        _ ->
            {error, no_file}
    end.
send_headers(Headers, File, From) ->
    {ok, Socket} = gen_tcp:connect(?AWS_S3_HOST, 80, [binary, {active, false}, {packet, 0}]),
    gen_tcp:send(Socket, list_to_binary(Headers)),
    case is_binary(File) of
        true ->
            send_binary(Socket, File, From);
        false ->
            send_data(Socket, File, From)
    end.
send_data(Socket, File, Global_from) ->
    {From, _} = Global_from,
    case file:read(File, ?CHUNK_SIZE) of
        {ok, Data} ->
            Begin = erlang:now(),
            gen_tcp:send(Socket, list_to_binary(Data)),
            End = erlang:now(),
            From ! {data_sent, length(Data), timer:now_diff(End, Begin)},
            send_data(Socket, File, Global_from);
        eof ->
            gen_tcp:close(Socket),
            From ! send_complete
	end.

send_binary(Socket, File, Global_from) ->
    {From, _} = Global_from,
    Size = size(File),
    if
        Size < ?CHUNK_SIZE ->
            Data = File,
            Rest = <<>>;
        true ->
            <<Data:?CHUNK_SIZE/binary, Rest/binary>> = File
    end,
    Begin = erlang:now(),
    gen_tcp:send(Socket, Data),
    End = erlang:now(),
    From ! {data_sent, Size, timer:now_diff(End, Begin)},
    case Rest of
        <<>> ->
            gen_tcp:close(Socket),
            From ! send_complete;
        _ ->
            send_binary(Socket, Rest, Global_from)
    end.

write_to_file(Socket, File, Global_from, Total_size, Current_size) when Current_size < Total_size ->
    {From, _} = Global_from,
    case gen_tcp:recv(Socket, 0, 500) of
        {ok, Data} ->
            file:write(File, Data),
            Size = size(Data),
            From ! {writing, Size},
            write_to_file(Socket, File, Global_from, Total_size, Current_size + Size);
        Anything ->
            % do some clean up to the file
            From ! Anything
    end;
write_to_file(Socket, File, Global_from, _Total_size, Current_size) ->
    gen_tcp:close(Socket),
    file:close(File),
    {From, _} = Global_from,
    From ! {file_writen, Current_size},
    ok.
get_date() ->
    httpd_util:rfc1123_date(erlang:localtime()).
get_unix_timestamp() ->
    calendar:datetime_to_gregorian_seconds(erlang:universaltime()).
get_datetime() ->
    {Mega, Sec, _Micro} = erlang:now(),
    (Mega * 1000000) + Sec. 
sign(To_sign) ->
    base64:encode(crypto:sha_mac(?SECRET_KEY, To_sign)).
sign(Method, Date, Bucket, Resource, Query_string, Additional_headers) ->
    Headers = lists:foldl(fun(Elem, Accum) -> 
                                  {Head, Body} = Elem,
                                  [lists:flatten([string:to_lower(Head), ":", Body, "\n"]) | Accum]
                          end, [], Additional_headers),
    String = lists:flatten([Method, "\n\n\n", Date, "\n", Headers, Bucket, Resource, Query_string]),
    sign(String).
auth_header(Sig) ->
    lists:flatten(["AWS ", ?ACCESS_KEY, ":", Sig]).
auth_header(Method, Date, Bucket, Resource, Query_string, Additional_headers) ->
    Sig = sign(Method, Date, Bucket, Resource, Query_string, Additional_headers),
    auth_header(Sig).
% checks to see if a valid acl was given and returns it
% if it's not valid it defaults
check_acl(Acl) ->
    Default = "private",
    Acls = ["private", "public-read", "public-read-write", "authenticated-read"],
    case lists:member(Acl, Acls) of
        true ->
            Acl;
        false ->
            Default
    end.
%% Simple s3 Request
simple_request(Method, Bucket, Resource) ->
    simple_request(Method, Bucket, Resource, "").
simple_request(Method, Bucket, Resource, Query_string) ->
    simple_request(Method, Bucket, Resource, Query_string, []).
simple_request(Method, Bucket, Resource, Query_string, Additional_headers) ->
    Method_string = string:to_upper(atom_to_list(Method)),
    Date = get_date(),
    URL = lists:flatten(["http://", ?AWS_S3_HOST, Bucket, Resource, Query_string]),
    Authorization = auth_header(Method_string, Date, Bucket, Resource, Query_string, Additional_headers),
    Headers = [{"Host", ?AWS_S3_HOST},
               {"Date", Date},
               {"Authorization", Authorization} |
               Additional_headers
              ],
    
    Request = case Method of
                  get -> 	   {URL, Headers};
                  %head ->   {URL, Headers};
                  put -> 	   {URL, Headers, "plain/text", ""}
                             %delete -> { URL, Headers}
              end,
    Http_options = [],
    Options = [{sync, true}, {headers_as_is, true}],
    case http:request(Method, Request, Http_options, Options) of
        {ok, { {_, Code, _}, _Return_headers, Response}} when Code =:= 200 ->
            {ok, Response};
        {ok, { {_, Code, _}, _Return_headers, Response}} ->
            {XML, _} = xmerl_scan:string(Response),
            [#xmlText{value = Code_string}] = xmerl_xpath:string("//Code/text()", XML),
            {error, Code, Code_string, {Headers, Response}};
        _Anything ->
            {error, uknown}
    end.

test() ->
    inets:start(),
    application:start(crypto),
    erls3:simple_request(get, "/bucket", "", "").
