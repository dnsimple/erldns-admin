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

-export([is_authorized/2]).

-define(DEFAULT_PORT, 8083).

% Gen server hooks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {}).

%% Not part of gen server

is_authorized(Req, State) ->
    case credentials() of
        {Username, Password} ->
            case cowboy_req:parse_header(<<"authorization">>, Req) of
                {basic, Username, Password} ->
                    {true, Req, Username};
                _ ->
                    {{false, <<"Basic realm=\"erldns admin\"">>}, Req, State}
            end;
        _ ->
            {{false, <<"Basic realm=\"erldns admin\"">>}, Req, State}
    end.

