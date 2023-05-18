###############
# BUILD PHASE #
###############

## Let's use decaf-clever-runtime-lang-r as the base image.
##
## It provides rdecaf and its dependencies. Furthermore, we will use its
## entrypoint/command, working directory, exposed volumes and ports etc.
FROM registry.docker.decafhub.com/decaf-clever-runtime-lang-r:v0.0.3

## Configure shell:
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

## Copy install script:
COPY install.sh /usr/local/share/decaf/clever/runtime/remaputils/install.sh

## Perform base installation:
RUN /usr/local/share/decaf/clever/runtime/remaputils/install.sh

## Define build arguments:
ARG VERSION

## Define environment variables:
ENV DECAF_CLEVER_RUNTIME_IMAGE_NAME=decaf-clever-runtime-remaputils \
    DECAF_CLEVER_RUNTIME_IMAGE_VERSION=$VERSION

## Copy remaputils codebase:
COPY ./ /tmp/remaputils

## Perform remaputils installation and cleanup:
##
## NOTE: We are performing this in its own step instead of inside the install.sh
## script as we do not want to run the entire install.sh script from scratch
## everytime we make a change to our codebase (Docker has layers for each step,
## and it can judge itself if a layer should be rebuild by checking its
## dependencies). However, if dependencies change, we must install them
## separately in the install.sh script.
RUN cd /tmp/remaputils && \
    /usr/local/bin/install.r . && \
    cd - && \
    rm -Rf /tmp/*
