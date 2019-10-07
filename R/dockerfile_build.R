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
build <- function(df, image) {
  UseMethod("build")
}

build.dockerfile <- function(df, image) {
  build_from_dockerfile(attr(df, ".dockerfile"), image)
}

build.edited_dockerfile <- function(df, image) {
  commands <- commands(df)
  tempDF <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
  writeLines(commands(df), tempDF)
  build_from_dockerfile(tempdf, image)
}

build_from_dockerfile <- function(dockerfile, image) {
  if (!check_docker()) {
    stop("`docker` needs to be added to your execution path.")
  }
  cmd <- paste0("docker build -f", dockerfile, "-t", image)
  print(cmd)
  # system(cmd)
}
