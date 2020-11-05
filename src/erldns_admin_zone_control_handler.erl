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

%% @doc Cowboy handler that handles Admin API requests to /zones/:name/:action
%%
%% Currently no actions are supported.
-module(erldns_admin_zone_control_handler).

-export([init/2]).
-export([content_types_provided/2, is_authorized/2]).
-export([to_html/2, to_json/2, to_text/2]).

-behaviour(cowboy_rest).

-include_lib("dns_erlang/include/dns.hrl").
-include_lib("erldns/include/erldns.hrl").

init(Req, State) ->
  {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
  {[
      {<<"text/html">>, to_html},
      {<<"text/plain">>, to_text},
      {<<"application/json">>, to_json}
    ], Req, State}.

is_authorized(Req, State) ->
  erldns_admin:is_authorized(Req, State).

to_html(Req, State) ->
  {<<"erldns admin">>, Req, State}.

to_text(Req, State) ->
  {<<"erldns admin">>, Req, State}.

to_json(Req, State) ->
  Name = cowboy_req:binding(zone_name, Req),
  Action = cowboy_req:binding(action, Req),
  case Action of
    _ ->
      lager:debug("Unsupported action: ~p (name: ~p)", [Action, Name]),
      {jsx:encode([]), Req, State}
  end.
