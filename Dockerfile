FROM thoughtbot/ghc

RUN mkdir -p /app
WORKDIR /app

COPY fomobot.cabal ./
RUN cabal update
RUN cabal install --dependencies-only -j4 --enable-tests
