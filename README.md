
<!-- README.md is generated from README.Rmd. Please edit that file -->
dockyard
========

The goal of dockyard is to provide tooling for building and deploying docker containers directly from R. Most best practices suggest using docker to containerize work, but do not provide the instructions on how to do so.

Installation
------------

*You can install the released version of dockyard from [CRAN](https://CRAN.R-project.org) with:*

``` r
# Not true yet
#install.packages("dockyard")
```

We are only on github at the moment, and can be downloaded at:

``` r
remotes::install_github("thebioengineer/dockyard")
```

Example
-------

So far, {dockyard} has tooling for creating basic dockerfiles and building from them. Below is an example of building a dockerimage in the same format as the [rocker/shiny](https://hub.docker.com/r/rocker/shiny) image.

In this example, the output image is called "dockyard\_shiny\_example"

``` r
dockerfile() %>%
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
  cmd("/usr/bin/shiny-server.sh") %>% 
  save("shiny_dockerfile",overwrite = TRUE) %>% 
  build("dockyard_shiny_example")
```
