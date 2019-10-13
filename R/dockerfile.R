#' @export
#' @title Describe the base docker image to use
#' @description Creating a docker image in a dockerfile requires specification
#'    of the base image to be creating your new docker image off of. {dockyard}
#'    assumes the user is basing their image off the latest available image
#'    from rocker/r-base, but it can be changed at any time.
#' @param df a dockerfile object from `dockerfile()`
#' @param image Repository address in the format username/image[:tag]
#' @return dockerfile
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1
#' dockerfile() %>%
#'   from("rocker/r-ver:devel")
#' @family dockerfile
#'
from <- function(df, image = "rocker/r-base") {
  if(!check_docker_imagename(image)){
    stop("Enter a valid docker image in the format username/image[:tag]")
  }
  add_base_image(df, image)
}


#' @export
#' @title Run update docker image
#' @description Add command when creating docker image to update installed software
#' @param df a dockerfile object from `dockerfile()`
#' @return dockerfile
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1
#' dockerfile() %>%
#'   from("rocker/r-ver:devel") %>%
#'   update()
#' @family dockerfile
#'
update <- function(df) {
  add_command(df, "RUN apt-get update -qq")
}

#' @export
#' @title Apt-get install software
#' @description Add software to install to docker image via `apt-get`
#' @param df a dockerfile object from `dockerfile()`
#' @param ... The software to install to the docker image
#' @return dockerfile
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl.
#'
#' dockerfile() %>%
#'   from("rocker/r-ver:devel") %>%
#'   update() %>%
#'   install("sudo","gdebi","pandoc","pandoc-citeproc",
#'           "libcurl4-gnutls-dev","libcairo2-dev",
#'           "libxtdev","wget")
#' @family dockerfile
#'
install <- function(df, ...) {
  to_install <- match.call(expand.dots = FALSE)$`...`
  install_command <- paste(
    "apt-get install -y ",
    paste0(to_install, collapse = " ")
  )
  run(df, install_command)
}

#' @export
#' @title Install packages for R
#' @description install packages from the default CRAN location for R
#' @param df a dockerfile object from `dockerfile()`
#' @param ... The R packages to install to the docker image
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl.
#'
#' dockerfile() %>%
#'  from("rocker/r-ver:devel") %>%
#'  update() %>%
#'  install("sudo","gdebi","pandoc","pandoc-citeproc",
#'          "libcurl4-gnutls-dev","libcairo2-dev",
#'          "libxtdev","wget")
#'   install_r_lib("tidyverse")
#' @family dockerfile
#'
install_r_lib <- function(df, ...) {
  to_install <- match.call(expand.dots = FALSE)$`...`

  install_command <- paste(
    "R -e \"install.packages(c(",
    paste0("'", to_install, "'", collapse = ", "),
    "))\""
  )

  run(df, install_command)
}


#' @export
#' @title RUN command in docker image
#' @description Command to be run in the docker image during building the image.
#' @param df a dockerfile object from `dockerfile()`
#' @param cmd command to be run in the docker image during build
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl. Then initialize Rstudio server
#'
#' dockerfile() %>%
#'  from("rocker/r-ver:devel") %>%
#'  update() %>%
#'  install("sudo","gdebi","pandoc","pandoc-citeproc",
#'          "libcurl4-gnutls-dev","libcairo2-dev",
#'          "libxtdev","wget") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O version.txt") %>%
#'   run("VERSION=$(cat version.txt)") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb -O ss-latest.deb") %>%
#'   run("gdebi -n ss-latest.deb") %>%
#'   run("rm -f version.txt ss-latest.deb") %>%
#'   run(". /etc/environment") %>%
#'   install_r_lib("shiny","rmarkdown") %>%
#'   run("cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/")
#'
#' @family dockerfile
#'
run <- function(df, cmd) {
  if(grepl("^RUN",df[length(df)])){
    command<-paste(df[[length(df)]], "&&", cmd )
    update_command(df,length(df),command)
  }else{
    command <- paste("RUN", cmd)
    add_command(df, command)
  }
}

#' @export
#' @title EXPOSE a port from the docker image
#' @description Command to expose a port from inside the docker container through
#' @param df a dockerfile object from `dockerfile()`
#' @param port the port to be exposing from the docker container
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl. Then initialize Rstudio server, and copy in a config file
#'
#' dockerfile() %>%
#'  from("rocker/r-ver:devel") %>%
#'  update() %>%
#'  install("sudo","gdebi","pandoc","pandoc-citeproc",
#'          "libcurl4-gnutls-dev","libcairo2-dev",
#'          "libxtdev","wget") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O version.txt") %>%
#'   run("VERSION=$(cat version.txt)") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb -O ss-latest.deb") %>%
#'   run("gdebi -n ss-latest.deb") %>%
#'   run("rm -f version.txt ss-latest.deb") %>%
#'   run(". /etc/environment") %>%
#'   install_r_lib("shiny","rmarkdown") %>%
#'   run("cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/") %>%
#'   expose(3838)
#'
#' @family dockerfile
#'
expose <- function(df,port){
  valid_ports<-c(1023,65535)
  if(port<valid_ports[1] | port>valid_ports[2]){
    stop("Enter a valid port number between ",valid_ports[1]," and ",valid_ports[2],".")
  }
  cmd <- paste("EXPOSE",port)
  add_command(df, cmd)
}

#' @export
#' @title Copy files into docker image
#' @description Command to copy files in the docker image during building the image.
#' @param df a dockerfile object from `dockerfile()`
#' @param source file to be copied from your local computer
#' @param dir location the file is to be copied to
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl. Then initialize Rstudio server, and copy in a config file
#'
#' dockerfile() %>%
#'  from("rocker/r-ver:devel") %>%
#'  update() %>%
#'  install("sudo","gdebi","pandoc","pandoc-citeproc",
#'          "libcurl4-gnutls-dev","libcairo2-dev",
#'          "libxtdev","wget") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O version.txt") %>%
#'   run("VERSION=$(cat version.txt)") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb -O ss-latest.deb") %>%
#'   run("gdebi -n ss-latest.deb") %>%
#'   run("rm -f version.txt ss-latest.deb") %>%
#'   run(". /etc/environment") %>%
#'   install_r_lib("shiny","rmarkdown") %>%
#'   run("cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/") %>%
#'   expose(3838) %>%
#'   copy("shiny-server.sh", "/usr/bin/shiny-server.sh")
#'
#' @family dockerfile
#' @rdname copy
#'
copy <- function(df, source, dir) {
  install_command <- paste("COPY", source, dir)
  add_command(df, install_command)
}

#' @export
#' @title Copy files from a URL into docker image
#' @description Command to copy files from a URL into the docker image during building the image.
#' @param df a dockerfile object from `dockerfile()`
#' @param source file to be copied from your local computer
#' @param dir location the file is to be copied to
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl. Then initialize Rstudio server, and copy in a config file
#'
#' dockerfile() %>%
#'  from("rocker/r-ver:devel") %>%
#'  update() %>%
#'  install("sudo","gdebi","pandoc","pandoc-citeproc",
#'          "libcurl4-gnutls-dev","libcairo2-dev",
#'          "libxtdev","wget","curl") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O version.txt") %>%
#'   run("VERSION=$(cat version.txt)") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb -O ss-latest.deb") %>%
#'   run("gdebi -n ss-latest.deb") %>%
#'   run("rm -f version.txt ss-latest.deb") %>%
#'   run(". /etc/environment") %>%
#'   install_r_lib("shiny","rmarkdown") %>%
#'   run("cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/") %>%
#'   expose(3838) %>%
#'   copy_from_url("https://raw.githubusercontent.com/rocker-org/shiny/master/shiny-server.sh","/usr/bin/shiny-server.sh")
#'
#' @family dockerfile
#' @rdname copy
#'
copy_from_url<-function(df,source,file){
  copy_command <- paste("curl", source,">", file)
  run(df, copy_command)
}

#' @export
#' @title Add files from local computerinto docker image
#' @description Command to ADd files from local computer into the docker image during building the image.
#' @param df a dockerfile object from `dockerfile()`
#' @param source file to be copied from your local computer
#' @param dir location the file is to be copied to
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl. Then initialize Rstudio server, and copy in a config file
#'
#' dockerfile() %>%
#'  from("rocker/r-ver:devel") %>%
#'  update() %>%
#'  install("sudo","gdebi","pandoc","pandoc-citeproc",
#'          "libcurl4-gnutls-dev","libcairo2-dev",
#'          "libxtdev","wget","curl") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O version.txt") %>%
#'   run("VERSION=$(cat version.txt)") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb -O ss-latest.deb") %>%
#'   run("gdebi -n ss-latest.deb") %>%
#'   run("rm -f version.txt ss-latest.deb") %>%
#'   run(". /etc/environment") %>%
#'   install_r_lib("shiny","rmarkdown") %>%
#'   run("cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/") %>%
#'   expose(3838) %>%
#'   copy_from_url("https://raw.githubusercontent.com/rocker-org/shiny/master/shiny-server.sh","/usr/bin/shiny-server.sh") %>%
#'   add("/my/local/shiny/folder/","/srv/shiny-server/)
#'
#' @family dockerfile
#' @rdname copy
#'
add <- function(df, source, dir) {
  if(!(dir.exists(source)|file.exists(source))){
    stop("`source` not found. Please supply a valid source.")
  }
  Add_command <- paste("ADD", source, dir)
  add_command(df, Add_command)
}

#' @export
#' @title Command to exectute on docker image creation
#' @description desctribe the script to be executed on docker image creation
#' @param df a dockerfile object from `dockerfile()`
#' @param source file to be executed
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl. Then initialize Rstudio server, and copy in a config file
#'
#' dockerfile() %>%
#'  from("rocker/r-ver:devel") %>%
#'  update() %>%
#'  install("sudo","gdebi","pandoc","pandoc-citeproc",
#'          "libcurl4-gnutls-dev","libcairo2-dev",
#'          "libxtdev","wget") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O version.txt") %>%
#'   run("VERSION=$(cat version.txt)") %>%
#'   run("wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb -O ss-latest.deb") %>%
#'   run("gdebi -n ss-latest.deb") %>%
#'   run("rm -f version.txt ss-latest.deb") %>%
#'   run(". /etc/environment") %>%
#'   install_r_lib("shiny","rmarkdown") %>%
#'   run("cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/") %>%
#'   expose(3838) %>%
#'   copy("shiny-server.sh", "/usr/bin/shiny-server.sh") %>%
#'   cmd("/usr/bin/shiny-server.sh")
#'
#' @family dockerfile
#'
cmd <- function(df, source) {
  cmd_run <- paste0("CMD [\"", source, "\"]")
  add_command(df, cmd_run)
}
