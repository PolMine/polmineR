## Docker image 'opencpu_restricted'

### Purpose

Access to resources exposed via polmineR/OpenCPU can be restricted on Apache2 webserver using .htaccess files. Building on the 'opencpu_public' image, the webserver is re-configured for restricted access.

The .htpasswd-Files with (hashed) passwords remain outside the container and are made available by mounting a volume, see the `docker run` command below. 


### Building the container 

The image builds on the image build with the Docker file in the directory opencpu_public and is assumed to be present as 'ocpu_public'.

```sh
docker build -t ocpu_restricted:latest .
```

### Server install

```sh
docker run -t -d \
    -v /usr/local/apache2/htdocs:/usr/local/apache2/htdocs \
    -v /opt/data/cwb/registry:/opt/data/cwb/registry \
    -v /opt/data/cwb/indexed_corpora:/opt/data/cwb/indexed_corpora \
    -p 8004:8004 \
    ocpu_restricted:latest
```
