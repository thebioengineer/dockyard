#' @export
#' @title Install Docker
#' @description provide the utility to install docker locally
#' @param force force install of latest docker version
#' @example
#' \donotrun{
#' dockyard::install_docker()
#'
#' }
install_docker<-function(force=FALSE){
  if(check_docker() & force == FALSE){
    ("`docker` is already installed.")
  }else{
    os <- get_os()
    UseMethod("install_docker",os)
  }
}

get_os<-function(){
  if ( .Platform$OS.type == "windows"){
    os<-"windows"
  } else if( Sys.info()[["sysname"]] == "Darwin" ){
    os<-"mac"
  } else if( .Platform$OS.type == "unix" ){
    os<-"unix"
  }else{
    stop("Unknown OS")
  }
  structure(
    os,
    class=c(os,"os")
  )
}

install_docker.windows <- function(user=TRUE){
  installer <- normalizePath(
    file.path(tempdir(),"docker_installer.exe"),
    wins
    lash = "/",
    mustWork = FALSE)

  message("Downloading Docker.\nThis may take a few minutes...\n\n")
  download.file("https://download.docker.com/win/stable/Docker%20for%20Windows%20Installer.exe",
                installer,
                mode="wb")
  message("\nInstalling Docker. Follow the installers instructions.\n")
  system(paste("start", installer))
  invisible()
}

install_docker.mac <- function(){
  # installer_dmg <- normalizePath(tempfile("docker_installer",fileext=".dmg"),
  #                              winslash = "/",
  #                              mustWork = FALSE)
  # message("Downloading Docker.\nThis may take a few minutes...")
  # download_result <- download.file("https://download.docker.com/mac/stable/Docker.dmg",
  #                                  installer_dmg)
  # system()
}

install_docker.unix <- function(){
  print("install me three!")
  # installer_exe <- normalizePath(tempfile("docker_installer",fileext=".exe"),
  #                              winslash = "/",
  #                              mustWork = FALSE)
  # download_result <- download.file("https:://download.docker.com/win/stable/Docker for Windows Installer.exe",installer_exe)
}
