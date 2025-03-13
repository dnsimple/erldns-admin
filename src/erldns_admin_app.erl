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

%% @doc The erldns OTP application.
-module(erldns_admin_app).
-behaviour(application).

-include_lib("kernel/include/logger.hrl").

% Application hooks
-export([start/2, stop/1]).

start(_Type, _Args) ->
    ?LOG_DEBUG(#{what => erldns_admin_application_start}),
    erldns_admin_sup:start_link().

stop(_State) ->
    ?LOG_DEBUG(#{what => erldns_admin_application_stop}),
    ok.
