FROM rust:1.85-slim AS builder

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      pkg-config \
      libssl-dev \
      nodejs \
      npm && \
    rm -rf /var/lib/apt/lists/*

COPY . /opt/baseline
WORKDIR /opt/baseline/tree-sitter-baseline
RUN npx tree-sitter generate && cargo build

WORKDIR /opt/baseline
RUN cargo build --features jit --release -p blc && \
    cp target/release/blc /usr/local/bin/blc

FROM debian:bookworm-slim
COPY --from=builder /usr/local/bin/blc /usr/local/bin/blc
WORKDIR /tmp/app
ENTRYPOINT ["blc"]
