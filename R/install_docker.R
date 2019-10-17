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

install_docker.windows <- function(force=FALSE){
  if(check_docker() & force == FALSE){
    message("`docker` is already installed.\n Use `dockyard::install_docker(force=TRUE)` to force a new installation.")
    # fun message
    if(runif(1)>.8){
      print_docker_logo()
    }
  }else{
    installer <- normalizePath(
      file.path(tempdir(),"docker_installer.exe"),
      winslash = "/",
      mustWork = FALSE)

    message("Downloading Docker.\nThis may take a few minutes...\n\n")
    download.file("https://download.docker.com/win/stable/Docker%20for%20Windows%20Installer.exe",
                  installer,
                  mode="wb")
    message("\nRunning Docker Installer. Follow the installers instructions.\n")
    system2("start", installer)
  }
  invisible()
}

install_docker.mac <- function(){
  # if(check_docker() & force == FALSE){
  #   message("`docker` is already installed.\n Use `dockyard::install_docker(force=TRUE)` to force a new installation")
  # }else{
  # installer <- normalizePath(
  #   file.path(tempdir(),"docker_installer.dmg"),
  #   winslash = "/",
  #   mustWork = FALSE)
  #
  # message("Downloading Docker.\nThis may take a few minutes...")
  # download_result <- download.file("https://download.docker.com/mac/stable/Docker.dmg",
  #                                  installer)
  # message("\nInstalling Docker. Follow the installers instructions.\n")
  # system()
  # }
  # invisible()
}

install_docker.unix <- function(){
  print("install me three!")
  # installer_exe <- normalizePath(tempfile("docker_installer",fileext=".exe"),
  #                              winslash = "/",
  #                              mustWork = FALSE)
  # download_result <- download.file("https:://download.docker.com/win/stable/Docker for Windows Installer.exe",installer_exe)
}


print_docker_logo<-function(){
  docker_logo_ascii <-c(
  '',
  '                    ##        .',
  '              ## ## ##       ==',
  '           ## ## ## ##      ===',
  '       /""""""""""""""""\\___/ ===',
  '  ~~~ {~~ ~~~~ ~~~ ~~~~ ~~ ~ /  ===- ~~~',
  '       \\______ o          __/',
  '        \\    \\        __/',
  '         \\____\\______/',
  '',
  '         |          |',
  '      __ |  __   __ | _  __   _',
  '     /  \\| /  \\ /   |/  / _\\ |',
  '     \\__/| \\__/ \\__ |\\_ \\__  |',
  '')

  message(paste(docker_logo_ascii,collapse = "\n"))
}
