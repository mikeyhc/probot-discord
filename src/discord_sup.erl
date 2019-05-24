%%%-------------------------------------------------------------------
%% @doc discord supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(discord_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([get_api_server/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(binary()) -> {ok, pid()} |
                              ignore |
                              {error, {already_started, pid()} |
                                      {shutdown, term()}}.
start_link(Token) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Token).

-spec get_api_server() -> pid().
get_api_server() ->
    Children = supervisor:which_children(?MODULE),
    {value, {_, Pid, _, _}} = lists:keysearch(discord_api, 1, Children),
    Pid.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(Token) ->
    Flags = #{strategy => one_for_all,
              intensity => 3,
              period => 30
             },
    ChildSpec = [#{id => discord_api,
                   start => {discord_api, start_link, [Token]}},
                 #{id => discord_gateway,
                   start => {discord_gateway, start_link, [Token]}}
                ],
    {ok, {Flags, ChildSpec}}.

%%====================================================================
%% Internal functions
%%====================================================================
