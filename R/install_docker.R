#' @export
#' @title Install Docker
#' @description provide the utility to install docker locally
#' @example
#' \donotrun{
#' dockyard::install_docker()
#'
#' }
install_docker<-function(user=TRUE){
  if(!check_docker()){
    stop("docker is already installed.")
  }
  os <- get_os()
  UseMethod("install_docker",os)
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
    file.path(tempdir(),"docker_installer.zip"),
    winslash = "/",
    mustWork = FALSE)

  # https://docs.docker.com/install/windows/docker-ee/#use-a-script-to-install-docker-ee
  # https://github.com/docker/docker-ce/releases/tag/v19.03.3
  message("Downloading Docker")
  download.file("https://download.docker.com/win/static/stable/x86_64/docker-17.09.0-ce.zip",
                installer)
  unzip(installer,exdir = )
}

install_docker.mac <- function(){
  installer_dmg <- normalizePath(tempfile("docker_installer",fileext=".dmg"),
                               winslash = "/",
                               mustWork = FALSE)
  message("Downloading Docker.\nThis may take a few minutes. After downloading, the installer will open. Fo")
  download_result <- download.file("https://download.docker.com/mac/stable/Docker.dmg",installer_dmg)
  system()
}

install_docker.unix <- function(){
  print("install me three!")
  # installer_exe <- normalizePath(tempfile("docker_installer",fileext=".exe"),
  #                              winslash = "/",
  #                              mustWork = FALSE)
  # download_result <- download.file("https:://download.docker.com/win/stable/Docker for Windows Installer.exe",installer_exe)
}
