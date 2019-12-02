docker <- function(cmd, ...) {
  win <- .Platform$OS.type == "windows"
  if(!win) {
    systemcmd <- "sudo"
    realcmd <- paste("docker", cmd)
  } else {
    systemcmd <- "docker"
    realcmd <- cmd
  }
  system2(systemcmd, realcmd, ...)
}
