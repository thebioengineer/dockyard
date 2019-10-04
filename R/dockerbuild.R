
#' @importFrom glue glue
#' @importFrom httr head
#' @export
#'
from<-function(imagename = "rocker/r-base",tag){
  check_valid(imagename,tag)
  if(!missing(tag)){
    imagename<-glue("{imagename}:{tag}")
  }
  paste("FROM",imagename)
}

#' @importFrom glue glue
#' @importFrom httr head
#'
check_valid<-function(imagename,tag){
  image<-strsplit(imagename,split = "/")[[1]]
  user<-image[1]
  container<-image[2]
#
#   url<-glue("https://hub.docker.com/r/{user}/{container}")
#   if(!missing(tag)){
#     url<-paste
#   }
#
#
  invisible()
}

update<-function(df){
  c(df,"RUN apt-get update -qq")
}

install<-function(df,...){
  to_install <- match.call(expand.dots = FALSE)$`...`

  install_command<-paste("RUN apt-get install -y --no-install-recommends \\ \n",
                         paste0("\t",to_install,collapse = " \\\n"))
  c(df,install_command)
}

install_r_lib<-function(df,...){
  to_install <- match.call(expand.dots = FALSE)$`...`

  install_command<-paste("RUN R -e \"install.packages(c(",
                         paste0("'",to_install,"'",collapse = ", "),
                         "))\"")
  c(df,install_command)
}

run<-function(df,...){
  to_install<-substitute(substitute(...))[-1]
  install_command<-paste("RUN",to_install)
  c(df,install_command)
}


from("rocker/r-devel") %>%
  update %>%
  install("git","file","libapparmor1","libcurl4-openssl-dev") %>%
  install_r_lib("tidyverse","broom")
