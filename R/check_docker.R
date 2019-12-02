#' @export
#' @title Check if `docker` is in execution path
#' @description Check if the docker executable is available in the
#'    computers path. Not necessary for setting up dockerfile's, but expected to pass if
#'    you are expecting to take advantage of {dockyards} docker interactions.
#' @return TRUE/FALSE
#' @examples
#' check_docker()
check_docker <- function() {

  cmd <- suppressWarnings({
      docker(
      "",
      stdout = FALSE,
      stderr = FALSE
      )
    })
  return(cmd == 0)
}

#' @export
#' @title Check image name format
#' @description Checks if the image name is a valid format
#' @param imagename character vector of name(s) of the output image. Should follow username/image[:tag] format
#' @return TRUE/FALSE
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1
#' check_docker_imagename("rocker/shiny:3.6.1")
#' check_docker_imagename("rocker/shiny")
#'
check_docker_imagename <- function(imagename){
  if(any(grepl("\\s",imagename))){
    stop("image name should not contain spaces")
  }

  uppercase_image <- grepl(
    "^(([A-Za-z0-9-_]+)\\/)*([A-Za-z0-9-_]+)+([:][a-z0-9-_.]+)*$",
    imagename,
    perl=TRUE
    )

  valid <- grepl(
    "^([A-Za-z0-9-_]+)\\/([a-z0-9-_]+)+([:][a-z0-9-_.]+)*$",
    imagename,
    perl=TRUE
    )
  valid2 <- grepl(
    "^([a-z0-9-_]+)+([:][a-z0-9-_.]+)*$",
    imagename,
    perl=TRUE
  )

  if(uppercase_image & !(valid|valid2)){
    stop("image name must be lowercase")
  }

  valid|valid2

}
