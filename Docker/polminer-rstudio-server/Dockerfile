
FROM rocker/rstudio:4.6

LABEL org.opencontainers.image.source=https://github.com/PolMine/polmineR

RUN apt-get update \
        && apt-get install -y --no-install-recommends \
        libglib2.0-dev \
        libpcre2-dev \
        libuv1-dev \
        libssl-dev \
        libcurl4-openssl-dev \
        libxml2-dev \
        libprotobuf-dev \
		libsodium-dev \
        libsecret-1-0 \
        && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
 	    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "options(warn = 2); install.packages('devtools')"

ARG branch=master

RUN Rscript -e "options(warn = 2); remotes::install_github('PolMine/cwbtools', ref = '${branch}', build_vignettes = FALSE)"

RUN Rscript -e "options(warn = 2); remotes::install_github('PolMine/polmineR', ref = '${branch}', build_vignettes = FALSE)"

RUN Rscript -e "library(cwbtools); library(polmineR)"