# Admin API for [erldns](https://github.com/dnsimple/erldns)

This app provides an admin API for querying and controlling an erldns server.

[![Build Status](https://github.com/dnsimple/erldns-admin/actions/workflows/ci.yml/badge.svg)](https://github.com/dnsimple/erldns-admin/actions/workflows/ci.yml)
[![Module Version](https://img.shields.io/hexpm/v/erldns-admin.svg)](https://hex.pm/packages/erldns-admin)

## Building

To build:

```shell
make
```

To start fresh:

```shell
make fresh
```

## Running

To start the HTTP server, run:

```shell
rebar3 shell
```

## Formatting

If your editor doesn't automatically format Erlang code using [erlfmt](https://github.com/WhatsApp/erlfmt), run:

```shell
make format
```
