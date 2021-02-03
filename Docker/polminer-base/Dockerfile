FROM ubuntu:focal

LABEL org.opencontainers.image.source https://github.com/PolMine/polmineR

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

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

## This was not needed before but we need it now
ENV DEBIAN_FRONTEND noninteractive

## Otherwise timedatectl will get called which leads to 'no systemd' inside Docker
ENV TZ UTC

RUN apt install -y --no-install-recommends gnupg2
#Add  R Repository
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9  \
    && add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/"

# Now install R and build dependecies for polmineR
RUN apt-get update \
        && apt-get install -y --no-install-recommends \
 		r-base \
 		r-base-dev \
 		r-recommended  \
        libglib2.0-dev \
        libssl-dev \
        libcurl4-openssl-dev \
        libxml2-dev \
        libprotobuf-dev \
		libsodium-dev \
        && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
 	    && rm -rf /var/lib/apt/lists/*

RUN  R -e "install.packages('devtools')"
CMD ["R"]