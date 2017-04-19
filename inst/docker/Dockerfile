FROM rocker/r-base

RUN sudo apt-get install bison
RUN sudo apt-get install flex
RUN sudo apt-get install libglib2.0-dev
RUN apt-get install libpcre3 libpcre3-dev
RUN sudo apt-get install libncurses5-dev libncursesw5-dev

RUN sudo apt-get install libcurl4-openssl-dev
RUN sudo apt-get install libssl-dev
RUN sudo apt-get install libssh2-1-dev

RUN Rscript -e 'install.packages(pkgs = c("RUnit", "devtools", "plyr", "tm"))'
RUN Rscript -e 'install.packages("rcqp")'
RUN Rscript -e 'install.packages("polmineR")'
