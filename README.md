# Admin API for [erldns](https://github.com/dnsimple/erldns)

This app provides an admin API for querying and controlling an erldns server.

## Building

To build:

```bash
make
```

To start fresh:

```bash
make fresh
```

## Running

To start the HTTP server, run:

```bash
rebar3 shell
```

## Formatting

If your editor doesn't automatically format Erlang code using [erlfmt](https://github.com/WhatsApp/erlfmt), run:

```bash
make format
```
