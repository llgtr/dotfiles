FROM node:lts-slim

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
        apt-transport-https ca-certificates curl ttfautohint git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /work

RUN git clone --depth 1 https://github.com/be5invis/Iosevka.git .

COPY generate-font.sh .
COPY private-build-plans.toml .

CMD ["bash", "generate-font.sh"]

