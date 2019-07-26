-module(vulkan_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

module_installed_test_() ->
    [?_assert(vulkan_plugin:module_installed(a, #{a => true})),
     ?_assertNot(vulkan_plugin:module_installed(a, #{})),
     ?_assertNot(vulkan_plugin:module_installed(a, #{b => true}))
    ].

commands_installed_test_() ->
    [?_assertEqual(vulkan_plugin:commands_installed(#{}, #{a => true}),  [])
    ].
