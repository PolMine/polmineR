FROM ghcr.io/polmine/polminer-builder:latest

LABEL org.opencontainers.image.source https://github.com/PolMine/polmineR

ARG branch=master

RUN Rscript -e "remotes::install_github('PolMine/cwbtools', ref = '${branch}', build_vignettes = FALSE)"

RUN Rscript -e "remotes::install_github('PolMine/polmineR', ref = '${branch}', build_vignettes = FALSE)"

ENTRYPOINT [ "R" ]