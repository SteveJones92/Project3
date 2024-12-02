# Project3
Project 3 - ST558 - Data Science Foundations

[Final Project](final_project.pdf)

# Notes/Steps

### Install Docker Desktop


### Create Dockerfile
Dockerfile specifies how to build the docker image
> touch Dockerfile

### Build docker image
docker build (image)
    -t api . (name of image from current directory)
    --no-cache (rebuilds the image from scratch)
> docker build -t api . --no-cache

### Run docker container
docker run (container)
    --rm removes the container after it is done running
    -it interactive terminal
    -e DISABLE_AUTH=true disables authentication
    -p 8000:8000
    -v ${PWD}:/home/rstudio/work
> docker run -it --rm -e DISABLE_AUTH=true -p 8000:8000 -v ${PWD}:/home/rstudio/work api

### Kill container
docker kill (container)
> docker kill api

### Save the image as a tar
docker save (image) -o (output file)
> docker save api -o api_image.tar

### Load the image from a tar
docker load -i (input file)
> docker load -i api_image.tar
