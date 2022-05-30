rosie_interface_compiler
=====

A plugin to compile ROS interface files in erlang modules. This modules allow ROSIE to serialize records filled with data by the user. The serialization code is inserted in each module and follows the CDR_LE format.

ROS messages, services and actions official syntax is supported.
The reliability of the compiler has undergone manual testing with a variety of ROS interface descriptions. The syntax coverage is limited to the test cases and rosie examples.

Build
-----

    > rebar3 compile

How to use the plugin
-----

    % rebar.config

    {provider_hooks, [
        {pre, [{compile, {rosie, compile}}]},
        {post, [{clean, {rosie, clean}}]}
    ]}.

Check ROSIE examples [LINK TO BE ADDED] to see how to use the produced modules.

How it works
-----

- The plugin searches for .msg, .srv and .action files in directories inside each app.

***
    ├── my_app
    │   ├── action
    │   │   ├── MyAction.action
    │   ├── msg
    │   │   ├── M1.msg
    |   ├── src
    │   │   ├── my_app.app.src
    │   ├── srv
    │   │   ├── MyService.srv

- Then it compiles them in erlang modules and headers and puts them in a dedicated _rosie directory under src. A compilation of the previous tree produces the following tree.

***
    ├── my_app
    │   ├── action
    │   │   ├── MyAction.action
    │   ├── msg
    │   │   ├── M1.msg
    |   ├── src
    │   │   ├── _rosie
    │   │   |   ├── actions
    │   │   |   |   ├── my_action
    │   │   |   |   |   ├── FeedbBackMessage.msg
    │   │   |   |   |   ├── GetResult.srv
    │   │   |   |   |   ├── SendGoal.srv
    │   │   |   ├── my_app_my_action_action.erl
    │   │   |   ├── my_app_my_action_action.hrl
    │   │   |   ├── my_app_my_action_feedback_message_msg.erl
    │   │   |   ├── my_app_my_action_feedback_message_msg.hrl
    │   │   |   ├── my_app_my_action_get_result_srv.erl
    │   │   |   ├── my_app_my_action_get_result_srv.hrl
    │   │   |   ├── my_app_my_action_send_goal_srv.erl
    │   │   |   ├── my_app_my_action_send_goal_srv.hrl
    │   │   |   ├── my_app_m1_msg.erl
    │   │   |   ├── my_app_m1_msg.hrl
    │   │   |   ├── my_app_my_service_srv.erl
    │   │   |   ├── my_app_my_service_srv.hrl
    │   │   ├── my_app.app.src
    │   ├── srv
    │   │   ├── MyService.srv

- As you can notice, compilation of .action files has multiple steps because of its nature it's decomposed in 2 services and 1 message compiled independently. Plus, a general module is made to describe the action.

- Compiled interfaces have long names due to the global scope of erlang modules. This prevents module shadowing, in case of interfaces with identical names used acros ROS packages.

- Erlang headers provide record and constants definitions that the user must utilize. Default values and constants are supported.

Notes
----

The plugin implements CDR serialization directly inside the modules. Therefor each produced module has no dependency. The app using it does not need to include any extra library.

You can use official ROS messages inside yours just by including the wanted ROS package as your project dependency. See rosie_ros2_deps (https://github.com/rosie-project/rosie_ros2_deps) which allows you to depend on official ROS resources.
