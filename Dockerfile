FROM thoughtbot/yesod:1.4.10-2

RUN mkdir -p /app
WORKDIR /app

COPY novproject.cabal ./
RUN cabal install --dependencies-only -j4 --enable-tests
