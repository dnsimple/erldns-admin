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

%% @doc Process for an administrative HTTP API.
%%
%% Provides zone quering and command-and-control functionality.

-module(erldns_admin).
-moduledoc """
Erldns admin API.

### Configuration:
This application will read from your `sys.config` the following example:
```erlang
{erldns_admin, [
    {credentials, {<<"username">>, <<"password">>}},
    {port, 8083}
]}
```
where `credentials` is a tuple of `username` and `password` as either strings or binaries,
and `port` is a valid Unix port to listen on.
""".

-export([is_authorized/2]).

-doc """
Configuration parameters, see the module documentation for details.
""".
-type config() :: #{
    port := 0..65535,
    username := binary(),
    password := binary()
}.

-doc "Common state for all handlers".
-opaque handler_state() :: #{
    username := binary(),
    password := binary()
}.
-export_type([config/0, handler_state/0]).

-doc false.
-spec is_authorized(cowboy_req:req(), handler_state()) ->
    {true | {false, iodata()}, cowboy_req:req(), handler_state()}
    | {stop, cowboy_req:req(), handler_state()}.
is_authorized(Req, #{username := ValidUsername, password := ValidPassword} = State) ->
    maybe
        {basic, GivenUsername, GivenPassword} ?= cowboy_req:parse_header(<<"authorization">>, Req),
        true ?= is_binary_of_equal_size(GivenUsername),
        true ?= is_binary_of_equal_size(GivenPassword),
        true ?= crypto:hash_equals(GivenUsername, ValidUsername) andalso
            crypto:hash_equals(GivenPassword, ValidPassword),
        {true, Req, State}
    else
        _ ->
            {{false, <<"Basic realm=\"erldns admin\"">>}, Req, State}
    end.

-spec is_binary_of_equal_size(term()) -> boolean().
is_binary_of_equal_size(Bin) ->
    is_binary(Bin) andalso byte_size(Bin) =:= byte_size(Bin).
