FROM thoughtbot/yesod:1.4.16.1

RUN mkdir -p /app
WORKDIR /app

COPY novproject.cabal ./
RUN cabal update
RUN cabal install --dependencies-only -j4 --enable-tests
