
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
#' @exportClass dockerfile edited_dockerfile
#'
dockerfile <- function(path="") {
  if (file.exists(path)) {
    df <- read_dockerfile(path)
    commands <- df$commands
    base_image <- df$base_image
  } else {
    commands <- ""
    base_image <- ""
  }
  structure(
    .Data = commands,
    .dockerfile = path,
    .base_image = base_image,
    class = c("dockerfile")
  )
}

#' @export
print.dockerfile <- function(x, ...) {
  loc <- attr(x, ".dockerfile")
  cat("<dockerfile>\n")
  if (dirname(loc) != "") {
    cat(c("Located at:", loc,"\n"))
  }
  commands <- x
  if (attr(x, ".base_image")!="") {
    commands <- c(paste("FROM", attr(x, ".base_image")), commands)
  } else {
    warning("No base image defined. Use 'from' to define base image.")
  }
  cat(paste0("\t", commands, "\n"))
  invisible(x)
}

#' @export
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
  if (o_image!="") {
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
#'
commands <- function(df) {
  commands <- df
  if (attr(df, ".base_image")!="") {
    commands <- c(paste("FROM", attr(df, ".base_image")), commands)
  } else {
    stop("No base image defined. Use 'from' to define base image.")
  }
  return(commands)
}


read_dockerfile <- function(path){
  commands <- readLines(path)
  base_image <- gsub("^FROM\\s+", "", commands[1], perl = TRUE)
  commands <- commands[-1]
  #combine_commands
  commands <- commands[commands != ""]
  if(any(grepl("\\\\$",commands))){
    grouped_commands <- which(grepl("\\\\$",commands))
    grouped_commands <- by(grouped_commands,cumsum(c(0,diff(grouped_commands)!=1)),function(g){
      g <- c(g,g[length(g)]+1)
      list(idx = g[1],rows = g,command = paste(gsub("\\\\$","",commands[g]),collapse = " "))
    })
    names(grouped_commands)<- seq_along(grouped_commands)
    grouped_commands_idx <- sapply(grouped_commands,`[[`,1)
    new_commands <- c()
    idx <- 1
    while(idx < length(commands)){
      if(idx %in% grouped_commands_idx){
        new_commands <- c(new_commands, grouped_commands[[which(grouped_commands_idx==idx)]][["command"]])
        idx <- idx + length(grouped_commands[[which(grouped_commands_idx==idx)]][["rows"]])
      }else{
        new_commands <- c(new_commands,commands[idx])
        idx <- idx + 1
      }
    }
    commands <- new_commands
  }

  return(list(
    base_image = base_image,
    commands = commands
  ))
}
