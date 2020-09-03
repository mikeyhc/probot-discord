-module(discord_api).

-behaviour(gen_server).

-export([start_link/1]).
-export([create_message/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("discord.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(binary()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Token) ->
    Args = #{token => Token, jobs => #{}},
    gen_server:start_link(?MODULE, Args, []).

-spec create_message(pid(), string(), binary()) -> ok.
create_message(Pid, Channel, Message) ->
    Body = #{<<"content">> => Message},
    URL = "/channels/" ++ Channel ++ "/messages",
    gen_server:cast(Pid, {post, URL, Body}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Args) ->
    gen_server:cast(self(), {connect, ?API_HOST, ?API_PATH}),
    {ok, Args}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({connect, Host, ApiPath}, State) ->
    {ok, ConnPid} = gun:open(Host, 443),
    {ok, _Protocol} = gun:await_up(ConnPid),
    MRef = monitor(process, ConnPid),
    logger:info("connected to ~s", [Host]),
    Connection = #{pid => ConnPid,
                   mref => MRef,
                   host => Host,
                   path => ApiPath
                  },
    {noreply, State#{conn => Connection}};
handle_cast({get, Url, Parser, Callback}, State) ->
    {noreply, get(Url, Parser, Callback, State)};
handle_cast({post, Url, Body}, State) ->
    {noreply, post(Url, Body, State)}.

%% TODO handle the non-ok status code
handle_info({gun_response, _ConnPid, StreamRef, nofin, Status, _Headers}, S) ->
    logger:info("received nofin api response to ~p[~p]", [StreamRef, Status]),
    if Status > 299 -> throw(unimplemented_status_code);
       true -> ok
    end,
    {noreply, S};
handle_info({gun_response, _ConnPid, StreamRef, fin, Status, _Headers}, S0) ->
    logger:info("received fin api response to ~p[~p]", [StreamRef, Status]),
    if Status < 200 andalso Status > 299 -> throw(unimplemented_status_code);
       true -> ok
    end,
    {noreply, complete_job(StreamRef, S0)};
handle_info({gun_data, _ConnPid, StreamRef, nofin, Data}, State) ->
    logger:info("received nofin api data to ~p", [StreamRef]),
    {noreply, append_data(StreamRef, Data, State)};
handle_info({gun_data, _ConnPid, StreamRef, fin, Data}, S0) ->
    logger:info("received fin api response to ~p", [StreamRef]),
    S1 = append_data(StreamRef, Data, S0),
    {noreply, complete_job(StreamRef, S1)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(Resource, P, CB, S = #{conn := Conn, token := Token, jobs := Jobs}) ->
    #{pid := ConnPid, path := ApiPath} = Conn,
    StreamRef = gun:get(ConnPid, ApiPath ++ Resource,
                        [{<<"Authorization">>, <<"Bot ", Token/binary>>}]),
    S#{jobs => Jobs#{StreamRef => #{callback => CB,
                                    type => body,
                                    parser => P,
                                    body => <<>>}}}.

post(Resource, Body, S = #{conn := Conn, token := Token, jobs := Jobs}) ->
    #{pid := ConnPid, path := ApiPath} = Conn,
    logger:debug("posting to ~p: ~p", [ApiPath ++ Resource, Body]),
    StreamRef = gun:post(ConnPid, ApiPath ++ Resource,
                        [{<<"Authorization">>, <<"Bot ", Token/binary>>},
                         {<<"Content-type">>, <<"application/json">>}],
                        jsone:encode(Body)),
    S#{jobs => Jobs#{StreamRef => #{type => empty}}}.

append_data(StreamRef, Data, State=#{jobs := Jobs}) ->
    #{StreamRef := Job0} = Jobs,
    Job1 = case maps:get(type, Job0) of
               empty -> Job0;
               body ->
                   #{body := Body} = Job0,
                   Job0#{body => <<Body/binary, Data/binary>>}
           end,
    State#{jobs => Jobs#{StreamRef => Job1}}.

complete_job(StreamRef, State=#{jobs := Jobs}) ->
    #{StreamRef := Job0} = Jobs,
    case maps:get(type, Job0) of
        empty -> ok;
        body ->
            #{callback := {M, F, A}, body := Body, parser := P} = Job0,
            logger:debug("got body: ~p", [Body]),
            apply(M, F, A ++ [P(jsone:decode(Body))])
    end,
    State#{jobs => maps:remove(StreamRef, Jobs)}.
