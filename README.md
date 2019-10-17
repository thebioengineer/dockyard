
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dockyard

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/thebioengineer/dockyard.svg?branch=master)](https://travis-ci.org/thebioengineer/dockyard)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/thebioengineer/dockyard?branch=master&svg=true)](https://ci.appveyor.com/project/thebioengineer/dockyard)
[![Coverage
status](https://codecov.io/gh/thebioengineer/dockyard/branch/master/graph/badge.svg)](https://codecov.io/github/thebioengineer/dockyard?branch=master)

The goal of dockyard is to provide tooling for building and deploying
docker containers directly from R. Most best practices suggest using
docker to containerize work, but do not provide the instructions on how
to do so.

## Installation

*You can install the released version of dockyard from
[CRAN](https://CRAN.R-project.org) with:*

``` r
# Not true yet
#install.packages("dockyard")
```

We are only on github at the moment, and can be downloaded at:

``` r
install.packages("remotes")
remotes::install_github("thebioengineer/dockyard")
```

## Example

So far, {dockyard} has tooling for creating basic dockerfiles and
building from them. Below is an example of building a dockerimage in the
same format as the [rocker/shiny](https://hub.docker.com/r/rocker/shiny)
image.

In this example, the output image is called “dockyard\_shiny\_example”.
When printing out the dockerfile object, it may look like some steps
were skipped, but rest assured all the steps outlined exist. When RUN
statements are listed one after another, they are linked together via
`&&`. This reduces the number of intermediary steps docker goes through
when generating our image.

``` r
library(dockyard)
#> 
#> Attaching package: 'dockyard'
#> The following object is masked from 'package:stats':
#> 
#>     update
#> The following object is masked from 'package:base':
#> 
#>     save

shiny_dockerfile <- dockerfile() %>%
 from("rocker/r-ver:devel") %>%
 update() %>%
 install("sudo","gdebi","pandoc","pandoc-citeproc",
         "libcurl4-gnutls-dev","libcairo2-dev",
         "libxt-dev","wget") %>%
  run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O version.txt") %>%
  run("VERSION=$(cat version.txt)") %>%
  run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb -O ss-latest.deb") %>%
  run("gdebi -n ss-latest.deb") %>%
  run("rm -f version.txt ss-latest.deb") %>%
  run(". /etc/environment") %>%
  install_r_lib("shiny","rmarkdown") %>%
  run("cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/") %>%
  expose(3838) %>%
  install("curl") %>% 
  copy_from_url("https://raw.githubusercontent.com/rocker-org/shiny/master/shiny-server.sh","/usr/bin/shiny-server.sh") %>%
  run("chmod +x /usr/bin/shiny-server.sh") %>%
  cmd("/usr/bin/shiny-server.sh") 

shiny_dockerfile
#> <dockerfile>
#>  FROM rocker/r-ver:devel
#>      RUN apt-get update -qq && apt-get install -y  sudo gdebi pandoc pandoc-citeproc libcurl4-gnutls-dev libcairo2-dev libxt-dev wget && wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O version.txt && VERSION=$(cat version.txt) && wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb -O ss-latest.deb && gdebi -n ss-latest.deb && rm -f version.txt ss-latest.deb && . /etc/environment && R -e "install.packages(c( 'shiny', 'rmarkdown' ))" && cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/
#>      EXPOSE 3838
#>      RUN apt-get install -y  curl && curl https://raw.githubusercontent.com/rocker-org/shiny/master/shiny-server.sh > /usr/bin/shiny-server.sh && chmod +x /usr/bin/shiny-server.sh
#>      CMD ["/usr/bin/shiny-server.sh"]
```

``` r
shiny_dockerfile %>% 
  save("shiny_dockerfile",overwrite = TRUE) %>% 
  build("dockyard_shiny_example")
```

Now to get your newly formed docker image into a docker container and
control it, we use `docker_run`.

``` r

docker_conn <- docker_run(
                 image = "dockyard_shiny_example", # the docker image to use
                 name  = "dockyard_container",     # the name to assign to the docker container
                 ports = "3838:3838"               # which port from local to connect to in the container 
                 ) 

docker_conn
#> < Docker Container: dockyard_container | Port: 0.0.0.0:3838 | Status: Up Less than a second | image: dockyard_shiny_example >
```

To stop the docker container, we use `docker_stop`

``` r
# we can also use the name of the container to stop it
# docker_stop("dockyard_container")
docker_stop(docker_conn)
#> Stopped Docker Container: dockyard_container

docker_conn
#> < Docker Container: dockyard_container | Port: N/A | Status: Does Not Exist | image: dockyard_shiny_example >
```

## Code of Conduct

Please note that the ‘dockyard’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
