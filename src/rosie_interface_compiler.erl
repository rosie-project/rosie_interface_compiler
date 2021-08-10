-module(rosie_interface_compiler).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = compile_provider:init(State),
    {ok, State2} = clean_provider:init(State1),
    {ok, State2}.
