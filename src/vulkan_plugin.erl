-module(vulkan_plugin).

-behaviour(gen_server).

-export([start_link/0]).
-export([load/2, run/4]).
-export([init/1, handle_call/3, handle_cast/2]).

-type run_cb() :: fun(([binary()]) -> {ok, binary()} |
                                      {error, no_such_command | binary()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, #{}, []).

-spec load(pid(), atom()) -> ok |
                             {error,
                              {module_already_loaded, atom()} |
                              {commands_already_loaded, [binary()]}}.
load(Pid, Module) ->
    gen_server:call(Pid, {load, Module}).

-spec run(pid(), binary(), [binary()], run_cb()) -> ok.
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
    Result = try
                 case maps:is_key(Command, Commands) of
                     true ->
                         F = maps:get(Command, Commands),
                         F(Args);
                     false -> {error, no_such_command}
                 end
             catch
                 error:badarg:St ->
                     logger:error("error in plugin: badarg~n", []),
                     logger:error("~p", [St]),
                     {error, <<"invalid argument">>};
                 error:X:St ->
                     logger:error("error in plugin: ~p~n", [X]),
                     logger:error("~p", [St]),
                     {error, <<"an error occurred">>};
                 X ->
                     logger:error("exception in plugin: ~p~n", [X]),
                     {error, <<"an error occurred">>}
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

