#!/usr/bin/env sh

## Show commands as they are being issued:
set -x

## Stop on errors:
set -e

## Configure debconf:
echo "debconf debconf/frontend select Noninteractive" | debconf-set-selections

## Update apt repositories:
apt-get update -qy

## Install R packages as Debian packages:
##
## NOTE: This is our preferred way of installing R packages as they and their
## dependencies are precompiled. However, we do not want recommended and
## suggested packages in favour of installing them explicitly.
apt-get install -qy --no-install-recommends --no-install-suggests \
    r-cran-devtools \
    r-cran-httr \
    r-cran-jpeg \
    r-cran-jsonlite \
    r-cran-png \
    r-cran-r.utils \
    r-cran-rcolorbrewer \
    r-cran-rjava \
    r-cran-stringdist \
    r-cran-tidyverse \
    r-cran-timeseries \
    r-cran-tseries \
    r-cran-webshot

## Install R packages from CRAN (our fallback solution):
/usr/local/bin/install2.r -d FALSE \
    mailR \
    tableHTML

## Create working directory:
mkdir -p /decaf/clever/run

## Cleanup:
apt-get clean autoclean
apt-get autoremove -y
rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
