-module(vulkan_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

module_installed_test_() ->
    [?_assert(vulkan_plugin:module_installed(a, #{a => true})),
     ?_assertNot(vulkan_plugin:module_installed(a, #{})),
     ?_assertNot(vulkan_plugin:module_installed(a, #{b => true}))
    ].

commands_installed_test_() ->
    S0 = #{a => true },
    [?_assertEqual([], vulkan_plugin:commands_installed(#{}, S0)),
     ?_assertEqual([a], vulkan_plugin:commands_installed(S0, S0))
    ].

install_module_test_() ->
    Plugin0 = plugin,
    Plugin1 = plugin1,
    Info0 = #{commands => #{a => true}},
    State0 = #{commands => #{}, plugins => #{}},
    State1 = #{commands => #{a => true}, plugins => #{Plugin0 => Info0}},
    [?_assertEqual({ok, State1},
                   vulkan_plugin:install_module(Plugin0, Info0, State0)),
     ?_assertEqual({error, {module_already_loaded, plugin}},
                   vulkan_plugin:install_module(Plugin0, Info0, State1)),
     ?_assertEqual({error, {commands_already_loaded, [a]}},
                   vulkan_plugin:install_module(Plugin1, Info0, State1))
    ].
