#' @export
#' @title Build the supplied dockerfile object into a dockerimage
#' @description Build the dockerfile into a docker image that can be then deployed.
#' @param df a dockerfile object
#' @param image name of the output image username/image[:tag]
#' @return dockerfile
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1
#' dockerfile() %>%
#'   from("rocker/shiny:3.6.1")
#' @family dockerfile
build <- function(df, image, builddir = ".") {
  UseMethod("build")
}

build.character <- function(df, image, builddir = "."){
  build(dockerfile(df), image, builddir = ".")
}

build.dockerfile <- function(df, image, builddir = ".") {
  build_from_dockerfile(attr(df, ".dockerfile"), image, builddir)
}

build.edited_dockerfile <- function(df, image, builddir = ".") {
  commands <- commands(df)
  tempDF <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
  writeLines(commands(df), tempDF)
  build_from_dockerfile(tempDF, image, builddir)
}



build_from_dockerfile <- function(dockerfile, image, builddir = ".") {
  if(!check_docker_imagename(paste0("dummyname/",image))){
    stop("`image` needs to be a valid image name format: image[:tag]")
  }
  if (!check_docker()) {
    stop("`docker` needs to be added to your execution path.")
  }
  cmd <- paste("docker build -f",dockerfile, "-t", image, builddir)
  # print(cmd)
  system(cmd)
}

#' @export
#' @title RUN command in docker image
#' @description Command to cope files  in the docker image during building the image.
#' @param df a dockerfile object from `dockerfile()`
#' @param source file to be copied from your local computer
#' @param dir location the file is to be copied to
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl. Then initialize Rstudio server, and copy in a config file
#'
#' new_dockerfile <- tempfile(pattern = "new_dockerfile_", fileext = "")
#'
#'
#' df <- dockerfile() %>%
#'   from("rocker/shiny:3.6.1") %>%
#'   update() %>%
#'   install("git", "libcurl4-openssl-dev") %>%
#'   install_r_lib("rcurl", "dplyr") %>%
#'   run("wget --no-check-certificate https://raw.githubusercontent.com/rocker-org/rstudio-daily/master/latest.R") %>%
#'   run("Rscript latest.R") %>%
#'   run("dpkg -i rstudio-server-daily-amd64.deb") %>%
#'   copy("userconf.sh", "/etc/cont-init.d/userconf") %>%
#'   save(new_dockerfile)
#' @family dockerfile
save <- function(df, path, overwrite = FALSE) {
  if (file.exists(path) & !overwrite) {
    stop("File already exists. Change overwrite to TRUE to overwrite existing dockerfile")
  }
  writeLines(commands(df), path, sep = "\n")
  attr(df, ".dockerfile") <- path
  class(df) <- "dockerfile"
  invisible(df)
}
