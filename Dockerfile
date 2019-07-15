FROM haskell:8
RUN apt-get update && apt-get install -y libpcre++-dev
RUN stack upgrade && stack update
WORKDIR /colorlog
ENTRYPOINT ["stack", "build"]
