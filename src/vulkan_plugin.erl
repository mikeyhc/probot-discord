-module(vulkan_plugin).

-behaviour(gen_server).

-export([start_link/0]).
-export([load/2, run/4]).
-export([init/1, handle_call/3, handle_cast/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link(?MODULE, #{}, []).

load(Pid, Module) ->
    gen_server:call(Pid, {load, Module}).

run(Pid, Command, Args, Callback) ->
    gen_server:cast(Pid, {run, Command, Args, Callback}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    {ok, #{plugins => #{}, commands => #{}}}.

handle_call({load, Module}, _From, State) ->
    Info = Module:load(),
    {Reply, State1} = install_module(Module, Info, State),
    {reply, Reply, State1};
handle_call({unload, _Name}, _From, State) ->
    % TODO implement
    {noreply, State}.

handle_cast({run, Command, Args, Callback}, S=#{commands := Commands}) ->
    Result = case maps:is_key(Command, Commands) of
                 true ->
                     F = maps:get(Command, Commands),
                     F(Args);
                 false -> {error, no_such_command}
             end,
    Callback(Result),
    {noreply, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install_module(Module,
               Info=#{commands := ModuleCommands},
               S=#{commands := Commands, plugins := Plugins}) ->
    case module_installed(Module, Plugins) of
        true -> {error, {module_already_loaded, Module}};
        false ->
            case commands_installed(ModuleCommands, Commands) of
                [] ->
                    {ok, S#{plugins => Plugins#{Module => Info},
                            commands => maps:merge(ModuleCommands, Commands)
                           }};
                L -> {error, {commands_already_loaded, L}}
            end
    end.

module_installed(Module, Plugins) ->
    lists:any(fun(E) -> E =:= Module end, maps:keys(Plugins)).

commands_installed(ModuleCommands, Commands) ->
    ModuleKeys = maps:keys(ModuleCommands),
    lists:filter(fun(X) -> lists:any(fun(Y) -> Y =:= X end, ModuleKeys) end,
                 maps:keys(Commands)).

