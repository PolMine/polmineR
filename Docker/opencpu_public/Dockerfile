FROM opencpu:ubuntu-22.04

RUN apt-get update && apt-get install -y --no-install-recommends apt-utils dialog

RUN apt-get update -y && apt-get install -y bison \
  flex \
  libglib2.0-dev \
  libpcre3 \
  libpcre3-dev \
  libncurses5-dev \
  libncursesw5-dev \ 
  libcurl4-gnutls-dev \
  libssl-dev \
  libxml2-dev \
  libsodium-dev \
  librdf0-dev \
  libprotobuf-dev \
  protobuf-compiler

ARG DUMMY=0003

RUN Rscript -e 'install.packages(pkgs = c("polmineR", "remotes"), repos = "https://cran.rstudio.com/")'
RUN Rscript -e 'install.packages(pkgs = "cwbtools", repos = "https://cran.rstudio.com/")'

RUN mkdir /opt/data && \
  mkdir /opt/data/cwb && \
  mkdir mkdir /opt/data/cwb/registry && \
  mkdir /opt/data/cwb/indexed_corpora
  
ENV CORPUS_REGISTRY=/opt/data/cwb/registry

RUN Rscript -e 'cwbtools::corpus_install(doi = "https://doi.org/10.5281/zenodo.3823245")'

ARG DUMMY=0004

RUN Rscript -e 'remotes::install_github("PolMine/polmineR", ref = "ocpu", build_vignettes = FALSE)'
RUN Rscript -e 'remotes::install_github("PolMine/cwbtools", ref = "dev", build_vignettes = FALSE)'

RUN sed -i 's/"preload":\s\["lattice"\]/"preload": ["polmineR", "methods"]/' /etc/opencpu/server.conf

RUN echo "LC_ALL=en_US.UTF-8" >> /etc/environment
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
RUN echo "LANG=en_US.UTF-8" > /etc/locale.conf
RUN locale-gen en_US.UTF-8
RUN echo "export LC_ALL=en_US.UTF-8" >> ~/.bashrc && /bin/bash -c "source ~/.bashrc"

ENV POLMINER_USE_TMP_REGISTRY=false
RUN touch ~/.Renviron && echo "POLMINER_USE_TMP_REGISTRY=false" >> ~/.Renviron
