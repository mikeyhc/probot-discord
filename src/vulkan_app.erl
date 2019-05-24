%%%-------------------------------------------------------------------
%% @doc vulkan public API
%% @end
%%%-------------------------------------------------------------------

-module(vulkan_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%TODO get token from environment
start(_StartType, _StartArgs) ->
    Token = case os:getenv("DISCORD_TOKEN") of
                false -> throw(missing_token);
                T -> binary:list_to_bin(T)
            end,
    vulkan_sup:start_link(Token).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
