FROM ghcr.io/polmine/polminer-builder:latest

LABEL org.opencontainers.image.source=https://github.com/PolMine/polmineR

ARG branch=master

# warn = 2: a dependency that fails to install stops the build
RUN Rscript -e "options(warn = 2, repos = 'https://cloud.r-project.org'); remotes::install_github('PolMine/cwbtools', ref = '${branch}', build_vignettes = FALSE)"

RUN Rscript -e "options(warn = 2, repos = 'https://cloud.r-project.org'); remotes::install_github('PolMine/polmineR', ref = '${branch}', build_vignettes = FALSE)"

RUN Rscript -e "library(cwbtools); library(polmineR)"

ENTRYPOINT [ "R" ]