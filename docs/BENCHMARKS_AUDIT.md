# Benchmarks Audit

Current benchmark inventory in this repository.

## Static

- st-apache-deb-self
- st-cowboy-27-self
- st-erlang23-self
- st-erlang26-self
- st-erlang27-self
- st-erlindex23-self
- st-erlindex26-self
- st-erlindex27-self
- st-nginx-deb-self
- st-yaws-26-self
- st-yaws-27-self

## Dynamic

- dy-apache-deb-self
- dy-cowboy-27-self
- dy-erlang23-self
- dy-erlang26-self
- dy-erlang27-self
- dy-erlindex23-self
- dy-erlindex26-self
- dy-erlindex27-self
- dy-nginx-deb-self
- dy-yaws-26-self
- dy-yaws-27-self

## WebSocket

- ws-apache-self
- ws-cowboy-27-self
- ws-nginx-java-self
- ws-nginx-python-websockets-self
- ws-nginx-tornado-self
- ws-yaws-27-self

## Total

- 28 benchmark containers

## Discovery rule

Any directory under `benchmarks/<type>/` that contains a `Dockerfile` is treated as a benchmark image. The image tag is the directory basename.
