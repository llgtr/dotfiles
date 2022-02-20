FROM debian:buster-slim

# Mostly borrowed from this gist:
# https://gist.github.com/tasuten/0431d8af3e7b5ad5bc5347ce2d7045d7

ARG OTFCC_VER=0.10.4
ARG PREMAKE_VER=5.0.0-alpha15
ARG NODE_VER=14

# Build deps
RUN apt-get update \
    && apt-get install --no-install-recommends -y \
        build-essential curl git ca-certificates ttfautohint

# Node.js
RUN curl -sSL https://deb.nodesource.com/setup_${NODE_VER}.x | bash - \
    && apt-get install --no-install-recommends -y nodejs

# premake (build dep for otfcc)
WORKDIR /tmp
RUN curl -sSLo premake5.tar.gz https://github.com/premake/premake-core/releases/download/v${PREMAKE_VER}/premake-${PREMAKE_VER}-linux.tar.gz \
    && tar xvf premake5.tar.gz \
    && mv premake5 /usr/local/bin/premake5 \
    && rm premake5.tar.gz

# otfcc
WORKDIR /tmp
RUN curl -sLo otfcc.tar.gz https://github.com/caryll/otfcc/archive/v${OTFCC_VER}.tar.gz \
    && tar xvf otfcc.tar.gz \
    && mv otfcc-${OTFCC_VER} otfcc \
    && cd otfcc \
    && premake5 gmake \
    && cd build/gmake \
    && make config=release_x64 \
    && cd /tmp/otfcc/bin/release-x64 \
    && mv otfccbuild /usr/local/bin/otfccbuild \
    && mv otfccdump /usr/local/bin/otfccdump \
    && cd /tmp \
    && rm -rf otfcc/ otfcc.tar.gz

# Iosevka repo
ARG CACHEBUST=1
RUN git clone https://github.com/be5invis/Iosevka.git
WORKDIR /tmp/Iosevka

COPY iosevka-build.sh /
COPY private-build-plans.toml .
CMD ["/iosevka-build.sh"]

