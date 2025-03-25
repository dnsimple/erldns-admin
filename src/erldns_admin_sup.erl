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

-module(erldns_admin_sup).
-moduledoc false.

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-spec start_link(erldns_admin:config()) -> supervisor:startlink_ret().
start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

-spec init(erldns_admin:config()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(#{port := Port, username := Username, password := Password}) ->
    State = #{username => Username, password => Password},
    Dispatch = cowboy_router:compile(
        [
            {'_', [
                {"/", erldns_admin_root_handler, State},
                {"/zones/:zone_name", erldns_admin_zone_resource_handler, State},
                {"/zones/:zone_name/:action", erldns_admin_zone_control_handler, State},
                {"/zones/:zone_name/records[/:record_name]",
                    erldns_admin_zone_records_resource_handler, State}
            ]}
        ]
    ),

    TransportOpts = #{socket_opts => [inet, {ip, {0, 0, 0, 0}}, {port, Port}]},
    ProtocolOpts = #{env => #{dispatch => Dispatch}},
    {ok, _} = cowboy:start_clear(?MODULE, TransportOpts, ProtocolOpts),

    Strategy = #{strategy => one_for_one, intensity => 20, period => 10},
    {ok, {Strategy, []}}.
