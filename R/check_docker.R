#' @export
#' @title Check if `docker` is in execution path
#' @description Check if the docker executable is available in the
#'    computers path. Not necessary for setting up dockerfile's, but expected to pass if
#'    you are expecting to take advantage of {dockyards} docker interactions.
#' @return TRUE/FALSE
#' @examples
#' check_docker()
check_docker <- function() {
  cmd <- system("docker")
  result <- cmd == 0
  return(result)
}
