# syntax=docker/dockerfile:1
#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

ARG CARDANO_NODE_IMAGE=ghcr.io/intersectmbo/cardano-node:11.0.1
ARG TINI_VERSION=v0.19.0

#                                                                              #
# --------------------------- BUILD (ogmios) --------------------------------- #
#                                                                              #

FROM --platform=${TARGETPLATFORM:-linux/amd64} busybox:1.35 AS ogmios

LABEL name=ogmios
LABEL description="A JSON WebSocket bridge for cardano-node."

ARG TARGETARCH
COPY ./server/bin/ogmios-${TARGETARCH} /bin/ogmios
COPY ./server/config/network /config

RUN chmod +x /bin/ogmios

EXPOSE 1337/tcp
HEALTHCHECK --interval=10s --timeout=5s --retries=1 CMD /bin/ogmios health-check

STOPSIGNAL SIGINT
ENTRYPOINT ["/bin/ogmios"]

#                                                                              #
# ----------------------------- TINI (verified) ------------------------------ #
#                                                                              #
# tini is fetched per target architecture and verified by sha256 via BuildKit's
# native ADD --checksum (no shell/tools required). The matching stage is
# selected below via TARGETARCH. Checksums are tini v0.19.0's published
# tini-static-<arch>.sha256sum values.

FROM busybox:1.35 AS tini-amd64
ARG TINI_VERSION
ADD --checksum=sha256:c5b0666b4cb676901f90dfcb37106783c5fe2077b04590973b885950611b30ee \
  https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-static-amd64 /tini

FROM busybox:1.35 AS tini-arm64
ARG TINI_VERSION
ADD --checksum=sha256:eae1d3aa50c48fb23b8cbdf4e369d0910dfc538566bfd09df89a774aa84a48b9 \
  https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-static-arm64 /tini

# Alias the arch-specific stage. BuildKit does not support variable expansion in
# COPY --from, so this FROM-with-ARG indirection is the supported workaround.
ARG TARGETARCH
FROM tini-${TARGETARCH} AS tini

#                                                                              #
# --------------------- RUN (cardano-node & ogmios) -------------------------- #
#                                                                              #

FROM ${CARDANO_NODE_IMAGE} AS cardano-node-ogmios

ARG NETWORK=mainnet
ARG TARGETARCH

LABEL name=cardano-node-ogmios
LABEL description="A Cardano node, side-by-side with its JSON WebSocket bridge."

COPY ./server/bin/ogmios-${TARGETARCH} /bin/ogmios
COPY ./server/config/network/${NETWORK} /config
COPY --from=tini /tini /tini

RUN chmod +x /bin/ogmios
RUN chmod +x /tini && mkdir -p /ipc

WORKDIR /root

 # Ogmios, cardano-node, ekg, prometheus
EXPOSE 1337/tcp 3000/tcp 12788/tcp 12798/tcp
HEALTHCHECK --interval=10s --timeout=5s --retries=1 CMD /bin/ogmios health-check

STOPSIGNAL SIGINT
COPY scripts/cardano-node-ogmios.sh cardano-node-ogmios.sh
ENTRYPOINT ["/tini", "-g", "--", "/root/cardano-node-ogmios.sh" ]
