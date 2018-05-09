FROM debian:buster-slim

# Mostly borrowed from this gist:
# https://gist.github.com/tasuten/0431d8af3e7b5ad5bc5347ce2d7045d7

# Build deps
RUN apt-get update \
    && apt-get install -y build-essential curl git

# Node.js
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - \
    && apt-get install -y nodejs

# ttfautohint
RUN apt-get install -y ttfautohint

# premake (build dep for otfcc)
WORKDIR /tmp
RUN curl -sLo premake5.tar.gz https://github.com/premake/premake-core/releases/download/v5.0.0-alpha11/premake-5.0.0-alpha11-linux.tar.gz \
    && tar xvf premake5.tar.gz \
    && mv premake5 /usr/local/bin/premake5 \
    && rm premake5.tar.gz

# otfcc
WORKDIR /tmp
RUN curl -sLo otfcc.tar.gz https://github.com/caryll/otfcc/archive/v0.8.4.tar.gz \
    && tar xvf otfcc.tar.gz && mv otfcc-0.8.4 otfcc \
    \
    && cd otfcc \
    && premake5 gmake \
    && cd build/gmake \
    && make config=release_x64 \
    \
    && cd /tmp/otfcc/bin/release-x64 \
    && mv otfccbuild /usr/local/bin/otfccbuild \
    && mv otfccdump /usr/local/bin/otfccdump \
    \
    && cd /tmp \
    && rm -rf otfcc/ otfcc.tar.gz

# Iosevka repo
RUN git clone https://github.com/be5invis/Iosevka.git
WORKDIR /tmp/Iosevka

COPY docker-iosevka.sh /
CMD ["/docker-iosevka.sh"]

