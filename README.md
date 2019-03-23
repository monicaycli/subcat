# README

This repository contains data and analysis scripts as reported in the study of
[**Individual Differences in Subphonemic Sensitivity and Phonological Skills**]
(https://osf.io/6rd2u/)

## Download the Repo

Run the following command in your terminal or command line

```bash
git clone https://github.monicaycli/subcat
```

## Docker Container

For reproducibility, we customized a Docker container image with
a version-controlled RStudio environment including all packages required for
running the analysis scripts.

1. Install Docker: click [here](https://docs.docker.com/install/)

1. Pull the Docker image: enter the following command in your terminal or
   command line

    ```bash
    docker pull monicaycli/subcat
    ```

1. Run an instance of the Docker image:

    ```bash
    docker run -d \
    --name=SUBCAT \
    -p 8787:8787 \
    -v $(pwd):/home/rstudio \
    monicaycli/subcat
    ```

    * Note that you can replace `$(pwd)` with any local directory you would like to have access to from within the container

1. Open the container's RStudio in a broswer:

    * Open a browser and enter the url: `localhost:8787`

    * When prompted, enter `rstudio` for both the username and password

    * In the RStudio interface, navigate to the directory container the analysis scripts and run the scripts

1. To stop the container once you are done:

    ```bash
    docker container stop SUBCAT
    ```

1. To remove the container instance (not the image itself):

    ```bash
    docker container rm SUBCAT
    ```

1. To remove the image:

    ```bash
    docker image rm monicaycli/subcat
    ```

## Licenses
* The data are released under the [CC-By Attribution 4.0 International License](./DATA/LICENSE)
* The code in this repo is released under the [MIT License](./ANALYSIS/LICENSE)
