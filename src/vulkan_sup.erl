%%%-------------------------------------------------------------------
%% @doc vulkan top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(vulkan_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([get_plugin_server/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Token)->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Token).

get_plugin_server() ->
    Children = supervisor:which_children(?SERVER),
    {value, {_, Pid, _, _}}  = lists:keysearch(vulkan_plugin, 1, Children),
    Pid.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Token) ->
    Flags = #{strategy => rest_for_one,
              intensity => 1,
              period => 30
             },
    ChildSpec = [#{id => discord_sup,
                  start => {discord_sup, start_link, [Token]},
                  type => supervisor},
                 #{id => vulkan_plugin,
                   start => {vulkan_plugin, start_link, []}}
                ],
    {ok, {Flags, ChildSpec}}.

%%====================================================================
%% Internal functions
%%====================================================================
