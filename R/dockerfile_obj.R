
#' @title Specify dockerfile to base build off of
#'
#' @details  This function should be used with the dockercommands family of functions to generate
#' a dockerfile to be used for building a new docker image
#'
#' @param path path to either existing dockerfile to base off of, or where the output will be

#' @return dockefile object
#' @examples
#' \dontrun{
#' df <- dockerfile()
#' df2 <- dockerfile("path/to/existing/dockerfile")
#' }
#' @export

dockerfile <- function(path = tempfile()) {
  if (file.exists(path)) {
    commands <- readLines(path)
    base_image <- gsub("^FROM ", "", commands[1], perl = TRUE)
    commands <- commands[-1]
  } else {
    commands <- ""
    base_image <- NULL
  }
  structure(
    .Data = commands,
    .dockerfile = path,
    .base_image = base_image,
    class = "dockerfile"
  )
}

print.dockerfile <- function(x, ...) {
  loc <- attr(x, ".dockerfile")
  cat("dockerfile:\n")
  if (dirname(loc) != normalizePath(tempdir(), winslash = "/")) {
    cat(c("Located at:", loc))
  }
  commands <- x
  if (!is.null(attr(x, ".base_image"))) {
    commands <- c(paste("FROM", attr(x, ".base_image")), commands)
  } else {
    warning("No base image defined. Use 'from' to define base image.")
  }
  cat(paste0("\t", commands, "\n"))
  invisible(x)
}

print.edited_dockerfile <- function(x, ...) {
  print.dockerfile(x, ...)
}

add_command <- function(x, new_command) {
  if (x[1] != "") {
    new_command <- c(x, new_command)
  }
  structure(
    .Data = new_command,
    .dockerfile = attr(x, ".dockerfile"),
    .base_image = attr(x, ".base_image"),
    class = c("edited_dockerfile", "dockerfile")
  )
}

update_command <- function(x, index ,updated_command){
  if (x[1] != "") {
    x[index] <- updated_command
  }
  class(x) <- c("edited_dockerfile", "dockerfile")
  x
}


add_base_image <- function(x, image) {
  o_image <- attr(x, ".base_image")
  if (!is.null(o_image)) {
    warning("Overwriting original base image '", o_image, "' with '", image, "'.")
  }
  attr(x, ".base_image") <- image
  class(x) <- c("edited_dockerfile", "dockerfile")
  return(x)
}


#' @export
#' @title Get the list of commands
#' @description Get the list of commands from the dockerfile object.
#' @param df a dockerfile object
#' @return character vector
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1
#' df <- dockerfile() %>%
#'   from("rocker/shiny:3.6.1") %>%
#'   run("echo 'hello world'")
#'
#' commands(df)
#' @family
#'
commands <- function(df) {
  commands <- df
  if (!is.null(attr(df, ".base_image"))) {
    commands <- c(paste("FROM", attr(df, ".base_image")), commands)
  } else {
    stop("No base image defined. Use 'from' to define base image.")
  }
  return(commands)
}
