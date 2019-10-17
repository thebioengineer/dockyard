#' @export
#' @title Build the supplied dockerfile object into a dockerimage
#' @description Build the dockerfile into a docker image that can be then deployed.
#' @param x a dockerfile object or path to a dockerfile
#' @param image name of the output image username/image[:tag]
#' @param builddir directory to build the docker image in. defaults to the current working directory
#' @param verbose should the command line results be printed to the R console?

#' @return dockerfile
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1
#' dockerfile() %>%
#'   from("rocker/shiny:3.6.1")
#' @family dockerfile
build <- function(x, image, builddir = ".", verbose = TRUE) {
  UseMethod("build")
}

build.character <- function(x, image, builddir = ".", verbose = TRUE){
  build_from_dockerfile(x, image, builddir, verbose)
}

build.dockerfile <- function(x, image, builddir = ".", verbose = TRUE) {
  dockerfile_path<-attr(x, ".dockerfile")
  if(!file.exists(dockerfile_path)){
    if(dockerfile_path==""){
      dockerfile_path <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
    }
    writeLines(commands(x), dockerfile_path)
  }
  build_from_dockerfile(dockerfile_path, image, builddir, verbose)
}

build.edited_dockerfile <- function(x, image, builddir = ".", verbose = TRUE) {
  commands <- commands(x)
  tempDF <- normalizePath(tempfile(tmpdir = builddir), winslash = "/", mustWork = FALSE)
  writeLines(commands(x), tempDF)
  build_from_dockerfile(tempDF, image, builddir, verbose)
  unlink(tempDF)
}



build_from_dockerfile <- function(path, image, builddir = ".", verbose = TRUE) {
  if(!file.exists(path)){
    stop("dockerfile does not exist")
  }

  if(!check_docker_imagename(image)){
    stop("`image` needs to be a valid image name format: [username/]image[:tag]")
  }
  if (!check_docker()) {
    stop("`docker` needs to be added to your execution path.")
  }

  cmd <- paste("docker build -f",path, "-t", image, builddir)
  # print(cmd)
  system(cmd,intern = TRUE,show.output.on.console = verbose)
}
