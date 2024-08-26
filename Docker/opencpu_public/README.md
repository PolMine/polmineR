## About the docker image 'opencpu_public'

### Building the container 

The image starts from opencpu/ubuntu-22.04, which includes an installation of R and of OpenCPU server.

For Apple Silicon machines, the image is not available at Docker Hub and needs to be build locally. The Dockerfile is available here: https://github.com/opencpu/opencpu-server. To use this image, change 'FROM opencpu/ubuntu-22.04' to 'FROM opencpu:ubuntu-22.04'.

The Dockerfile installs system requirements for the Corpus Worbench included in the RcppCWB package. Finally, the latest development versions of polmineR and cwbtools are installed.


### Testing the container

```sh
cd ~/Lab/github/docker/opencpu_public
docker build -t ocpu_public:latest .
docker pull polmine/opencpu_public:latest # not updated
docker run -i -t ocpu_public:latest /bin/bash # start interactive mode, for testing purposes
docker run -t -p 8004:8004 ocpu_public:latest # run on localhost
docker run -t -d -p 8004:8004 ocpu_public:latest # run on localhost, demon mode
curl http://localhost:8004/ocpu/info # check that opencpu is available and running
```

This is a check you can run locally.

```r
library(polmineR)
x <- corpus("GERMAPARLSAMPLE", server = "http://192.168.178.82:8004", restricted = FALSE)
y <- size(x)
show(y)
```

### Server install

For running the container on a server to grant access to corpora stored outside the docker container, start the container as follows.

```r
docker run -t -d \
    -v /opt/data/cwb/registry:/opt/data/cwb/registry \
    -v /opt/data/cwb/indexed_corpora:/opt/data/cwb/indexed_corpora \
    -p 8004:8004 \
    ocpu_public:latest
```

Note: For technical reasons, the CWB directories ('registry' and 'indexed_corpora') on the host need to be the identical with the directories within the Docker container. Otherwise, the paths in the registry files would have to be changed/mapped to conform to the mounting point within the Docker container.

