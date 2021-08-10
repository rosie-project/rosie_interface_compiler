-module(rosie_interface_compiler).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = root_compiler:init(State),
    {ok, State1}.
