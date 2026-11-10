FROM ubuntu:noble

LABEL org.opencontainers.image.source=https://github.com/PolMine/polmineR

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		software-properties-common \
        dirmngr \
        ed \
		less \
		locales \
		vim-tiny \
		wget \
		ca-certificates

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
	&& locale-gen en_US.utf8 \
	&& /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8

## This was not needed before but we need it now
ENV DEBIAN_FRONTEND=noninteractive

## Otherwise timedatectl will get called which leads to 'no systemd' inside Docker
ENV TZ=UTC

# Add R repository, see https://cran.r-project.org/bin/linux/ubuntu/
# (apt-key is not available on Ubuntu 24.04)
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
        > /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc \
    && add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/"

# Now install R and build dependencies for polmineR
# (libpcre2-dev and libglib2.0-dev for RcppCWB, libuv1-dev for fs)
RUN apt-get update \
        && apt-get install -y --no-install-recommends \
 		r-base \
 		r-base-dev \
 		r-recommended  \
        libglib2.0-dev \
        libpcre2-dev \
        libuv1-dev \
        libssl-dev \
        libcurl4-openssl-dev \
        libxml2-dev \
        libprotobuf-dev \
		libsodium-dev \
        && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
 	    && rm -rf /var/lib/apt/lists/*

# Images derived from this one install packages with remotes::install_github()
RUN Rscript -e "options(warn = 2); install.packages('remotes', repos = 'https://cloud.r-project.org')"
CMD ["R"]
