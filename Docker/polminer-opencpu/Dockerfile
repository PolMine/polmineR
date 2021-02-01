ARG PARENT_IMAGE=ghcr.io/polmine/polminer:latest
FROM ${PARENT_IMAGE}

LABEL org.opencontainers.image.source https://github.com/PolMine/polmineR

RUN \
  apt-get update && \
  apt-get -y dist-upgrade && \
  apt-get install -y software-properties-common && \
  add-apt-repository -y ppa:opencpu/opencpu-2.2 && \
  apt-get update && \
  apt-get install -y opencpu-server && \
  apt-mark hold opencpu-server && \
  apt-get install -y --no-install-recommends apt-utils dialog && \
  apt-get install -y bison \
  flex \
  libglib2.0-dev \
  libpcre3 \
  libpcre3-dev \
  libncurses5-dev \
  libncursesw5-dev \
  libcurl4-openssl-dev \
  libssl-dev

RUN mkdir /opt/data && \
  mkdir /opt/data/cwb && \
  mkdir mkdir /opt/data/cwb/registry && \
  mkdir /opt/data/cwb/indexed_corpora

ENV CORPUS_REGISTRY=/opt/data/cwb/registry

RUN sed -i 's/"preload":\s\["lattice"\]/"preload": ["polmineR"]/' /etc/opencpu/server.conf

ENV POLMINER_USE_TMP_REGISTRY=false
RUN touch ~/.Renviron && echo "POLMINER_USE_TMP_REGISTRY=false" >> ~/.Renviron

RUN \
    rm /etc/apache2/sites-enabled/000-default.conf /etc/apache2/sites-enabled/default-ssl.conf  /etc/apache2/sites-enabled/rstudio.conf /var/www/html/index.html

# Prints apache logs to stdout
RUN \
  ln -sf /proc/self/fd/1 /var/log/apache2/access.log && \
  ln -sf /proc/self/fd/1 /var/log/apache2/error.log && \
  ln -sf /proc/self/fd/1 /var/log/opencpu/apache_access.log && \
  ln -sf /proc/self/fd/1 /var/log/opencpu/apache_error.log

# Set opencpu password so that we can login
RUN \
  echo "opencpu:opencpu" | chpasswd

# Apache ports

EXPOSE 8004

# Start non-daemonized webserver
ENTRYPOINT [ "apachectl", "-DFOREGROUND" ] 