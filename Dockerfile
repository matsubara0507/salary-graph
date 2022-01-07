FROM ghcr.io/matsubara0507/ubuntu-for-haskell:18.04
ARG local_bin_path
RUN mkdir -p /usr/local/bin
COPY ${local_bin_path}/salary-graph /usr/local/bin
ENTRYPOINT ["/usr/local/bin/salary-graph"]
