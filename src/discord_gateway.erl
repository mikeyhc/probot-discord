-module(discord_gateway).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-include("discord.hrl").

-define(MAX_RETIRES, 5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(binary()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Token) ->
    gen_server:start_link(?MODULE, #{token => Token}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Args) ->
    gen_server:cast(self(), {connect, ?GATEWAY_HOST, ?GATEWAY_ARGS}),
    {ok, Args#{retries => ?MAX_RETIRES}}.

terminate(_Reason, #{conn := Conn}) ->
    #{pid := ConnPid} = Conn,
    gun:close(ConnPid).

handle_call(_Msg, _From, State) ->
    {noreply, State}.

% TODO: check for existing connection
handle_cast({connect, Host, Args}, State) ->
    {ok, ConnPid} = gun:open(Host, ?GATEWAY_PORT, #{protocols => [http]}),
    {ok, _Protocol} = gun:await_up(ConnPid),
    MRef = monitor(process, ConnPid),
    gun:ws_upgrade(ConnPid, Args, []),
    logger:info("connected to ~s", [Host]),
    Connection = #{ pid => ConnPid,
                    mref => MRef,
                    host => Host,
                    args => Args
                  },
    {noreply, State#{conn => Connection}};
handle_cast(reconnect, #{retries := 0}) ->
    throw(max_retries_exceeded);
handle_cast(reconnect, S0 = #{conn := Conn, retries := Retries}) ->
    logger:info("reconnecting"),
    #{pid := Pid, host := Host, args := Args} = Conn,
    gun:close(Pid),
    gen_server:cast(self(), {connect, Host, Args}),
    {noreply, S0#{retries := Retries - 1}};
% TODO: detect un-acked heartbeat
handle_cast(send_heartbeat, S = #{conn := Conn, heartbeat := Heartbeat}) ->
    #{pid := ConnPid} = Conn,
    #{seq := Seq} = Heartbeat,
    Msg = jiffy:encode(#{<<"op">> => 1, <<"d">> => Seq}),
    logger:info("sending heartbeat message"),
    gun:ws_send(ConnPid, {text, Msg}),
    {noreply, S};
handle_cast(identify, S=#{token := Token, conn := Conn}) ->
    #{pid := ConnPid} = Conn,
    Ident = #{<<"token">> => Token,
              <<"properties">> => #{
                  <<"$os">> => ?OS,
                  <<"$browser">> => ?BROWSER,
                  <<"$device">> => ?BROWSER
                 }
             },
    Msg = jiffy:encode(#{<<"op">> => 2, <<"d">> => Ident}),
    logger:info("sending identify"),
    gun:ws_send(ConnPid, {text, Msg}),
    {noreply, S}.

%% eat upgrade message
handle_info({gun_upgrade, _Pid, Stream, _Protos, _Headers}, S=#{conn := C}) ->
    {noreply, S#{conn => C#{stream => Stream}}};
handle_info({gun_ws, _ConnPid, _Ref, {text, Body}}, State) ->
    Msg = jiffy:decode(Body, [return_maps]),
    #{<<"op">> := Op} = Msg,
    {noreply, handle_message(Op, Msg, State)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_message(0, Msg, S0) ->
    logger:info("received dispatch message"),
    logger:debug("message was: ~p", [Msg]),
    #{<<"d">> := DField, <<"s">> := Seq} = Msg,
    OldSessionId = maps:get(session_id, S0, undefined),
    SessionId = maps:get(<<"session_id">>, DField, OldSessionId),
    case SessionId of
        undefined -> throw(missing_session_id);
        _ -> ok
    end,
    #{heartbeat := H0} = S0,
    case maps:get(<<"content">>, DField, undefined) of
        <<"!", Command/binary>> ->
            handle_command(maps:get(<<"channel_id">>, DField), Command);
        _ -> ok
    end,
    S0#{session_id => SessionId, heartbeat => H0#{seq => Seq}};
handle_message(10, Msg, S0) ->
    logger:info("received heartbeat request"),
    H0 = maps:get(heartbeat, S0, undefined),
    if H0 =/= undefined -> remove_heartbeat_schedule(H0);
       true -> ok
    end,
    H1 = make_heartbeat(Msg),
    H2 = add_heartbeat_schedule(H1),
    case maps:is_key(session, S0) of
        true -> ok;
        false -> gen_server:cast(self(), identify)
    end,
    S0#{heartbeat => H2};
handle_message(11, _Msg, S0) ->
    logger:info("received heatbeat ack"),
    S0.

remove_heartbeat_schedule(#{tref := TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
    logger:info("hearbeat schedule removed").

make_heartbeat(#{<<"d">> := Heartbeat, <<"s">> := Seq}) ->
    #{<<"heartbeat_interval">> := Interval} = Heartbeat,
    #{interval => Interval, seq => Seq}.

add_heartbeat_schedule(H = #{interval := Interval}) ->
    {ok, TRef} = timer:apply_interval(Interval, gen_server, cast,
                                      [self(), send_heartbeat]),
    logger:info("heartbeat schedule created"),
    H#{tref => TRef}.

handle_command(Channel, CommandBin) ->
    logger:info("got command: ~s", [CommandBin]),
    [Command|Args] = binary:split(CommandBin, <<" ">>, [global]),
    PluginPid = vulkan_sup:get_plugin_server(),
    vulkan_plugin:run(PluginPid, Command, Args,
                      default_plugin_handler(Channel, Command)).

default_plugin_handler(Channel, Command) ->
    with_error_handling(Channel, Command, print_value_callback(Channel)).

with_error_handling(Channel, Command, F) when is_binary(Channel) ->
    with_error_handling(binary:bin_to_list(Channel), Command, F);
with_error_handling(Channel, Command, F) ->
    fun({error, no_such_command}) ->
            Pid = discord_sup:get_api_server(),
            B = lists:flatten(
                  io_lib:format("command not found: ~s", [Command])),
            discord_api:create_message(Pid, Channel, binary:list_to_bin(B));
       ({error, Message}) ->
            Pid = discord_sup:get_api_server(),
            B = lists:flatten(
                  io_lib:format("error in command: ~s~n~s",
                                [Command, Message])),
            discord_api:create_message(Pid, Channel, binary:list_to_bin(B));
       ({ok, V}) -> F(V)
    end.

print_value_callback(Channel) when is_binary(Channel) ->
    print_value_callback(binary:bin_to_list(Channel));
print_value_callback(Channel) ->
    fun(Msg) ->
            Pid = discord_sup:get_api_server(),
            discord_api:create_message(Pid, Channel, Msg)
    end.
