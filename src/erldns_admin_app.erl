%% Copyright (c) 2012-2019, DNSimple Corporation
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(erldns_admin_app).
-moduledoc false.

-behaviour(application).

-define(DEFAULT_PORT, 8083).

-include_lib("kernel/include/logger.hrl").

% Application hooks
-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> supervisor:startlink_ret().
start(_Type, _Args) ->
    ?LOG_DEBUG(#{what => erldns_admin_application_start}),
    case ensure_valid_config() of
        false ->
            {error, bad_configuration};
        Config ->
            erldns_admin_sup:start_link(Config)
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ?LOG_DEBUG(#{what => erldns_admin_application_stop}),
    ok.

-type env() :: [{atom(), term()}].

-spec ensure_valid_config() -> false | erldns_admin:config().
ensure_valid_config() ->
    maybe
        {true, Env} ?= env(),
        {true, Port} ?= port(Env),
        {true, Username, Password} ?= credentials(Env),
        #{port => Port, username => Username, password => Password}
    end.

-spec port(env()) -> {true, 1..65535} | false.
port(Env) ->
    case proplists:get_value(port, Env, ?DEFAULT_PORT) of
        Port when is_integer(Port), 0 < Port, Port =< 65535 ->
            {true, Port};
        OtherPort ->
            ?LOG_ERROR(#{what => erldns_admin_bad_config, port => OtherPort}),
            false
    end.

-spec credentials(env()) -> {true, binary(), binary()} | false.
credentials(Env) ->
    case lists:keyfind(credentials, 1, Env) of
        {credentials, {Username, Password}} when is_list(Username), is_list(Password) ->
            {true, list_to_binary(Username), list_to_binary(Password)};
        {credentials, {Username, Password}} when is_binary(Username), is_binary(Password) ->
            {true, Username, Password};
        OtherValue ->
            ?LOG_ERROR(#{what => erldns_admin_bad_config, credentials => OtherValue}),
            false
    end.

-spec env() -> {true, env()} | false.
env() ->
    case application:get_env(erldns_admin, admin) of
        {ok, Env} when is_list(Env) -> {true, Env};
        _ -> false
    end.
