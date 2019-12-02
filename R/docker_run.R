#' @export
#' @title Initialize a docker container from an image
#' @description Initialize a docker image into a docker container. Generates a
#'     dockercon object to help monitor status of docker container.
#' @param image Repository address in the format username/image[:tag]
#' @param name name of the docker container. can be left empty
#' @param ports what port to expose and connect to in the docker container
#'     format is port:port
#' @param mountpoints what folder to expose to the docker container, where to
#'     connect to it within the docker container. format is dir:dir
#' @param docker_run_args any additional arguments to be passed to the `docker run` command.
#' @return docker_conn object
#' @examples
#' \dontrun{
#' # Start a docker container from the rocker/r-ver:devel image
#' d_conn<-docker_run(image = "rocker/r-ver:devel", name = "testimage")
#' }
docker_run <- function(image,name,ports,mountpoints,docker_run_args){
  if(!check_docker_imagename(image)){
    stop("Enter a valid docker image name in the format: [user/]image[:tag]")
  }

  if(!missing(name)){
    if(grepl("\\s", name, perl=TRUE)){
    stop("Must use a valid name without spaces")
  }}

  run_cmd<-"run --rm -d"

  if(!missing(ports)){
    if(!grepl("\\d+[:]\\d+",ports)){
      stop("Must use a valid port format: internal_port:docker_port")
    }
    run_cmd <- paste(run_cmd,"-p",ports)
  }

  if(!missing(mountpoints)){
    if(!grepl(".+[:].+",mountpoints,perl=TRUE)){
      stop("Must use a valid mountpoints format: internal_dir:docker_dir")
    }
    run_cmd <- paste(run_cmd,"-v",mountpoints)
  }

  if(!missing(name)){
    run_cmd <- paste(run_cmd,"--name",name)
  }

  if(!missing(docker_run_args)){
    run_cmd <- paste(run_cmd,docker_run_args)
  }

  run_cmd <- paste(run_cmd,image)

  start_results<-docker(run_cmd,stdout = TRUE)

  if(!is.null(attr(start_results,"status"))){
  if(attr(start_results,"status")!=0){
    stop(start_results)
  }}

  if(missing(name)){
    running_containers<-list_containers()
    which_image<-do.call('c',lapply(running_containers$CONTAINER.ID,function(id,newcontainerid){
      grepl(paste0("^",id),newcontainerid)},
      start_results))
    name <- running_containers$NAMES[which_image]
  }
  docker_connection(name)
}

#' @export
#' @title kill a docker container
#' @description kill a currently running docker container
#' @param x either the name of the docker container or a docker_connection
#' @return NULL
#' @export
docker_kill <- function(x){
  UseMethod("docker_kill")
}
#' @export
docker_kill.character<-function(x){
  conn<-docker_connection(x)
  docker_kill.docker_connection(conn)
}
#' @export
docker_kill.docker_connection<-function(x){
  id <- attr(x,".docker_id")
  kill_cmd <- paste("kill",id)
  result<-docker(kill_cmd,stdout = TRUE)
  if(is.null(attr(result,"status"))){
    message("Killed Docker Container: ",attr(x,".name"))
  }
}


#' @export
#' @title Stop a docker container from running
#' @description Stop a currently running docker container
#' @param x either the name of the docker container or a docker_connection
#' @return NULL
#' @export
docker_stop<-function(x){
  UseMethod("docker_stop",x)
}
#' @export
docker_stop.character<-function(x){
  conn<-docker_connection(x)
  docker_stop.docker_connection(conn)
}
#' @export
docker_stop.docker_connection<-function(x){
  id <- attr(x,".docker_id")
  stop_cmd <- paste("stop",id)
  result<-docker(stop_cmd,stdout = TRUE)
  if(is.null(attr(result,"status"))){
    message("Stopped Docker Container: ",attr(x,".name"))
  }
}

#' @export
#' @title Pause a docker container to maintain its state
#' @description Pause a docker container from the list of docker containers
#' @param x either the name of the docker container or a docker_connection
#' @return NULL
#' @export
docker_pause<-function(x){
  UseMethod("docker_pause")
}
#' @export
docker_pause.character<-function(x){
  conn<-docker_connection(x)
  docker_pause.docker_connection(conn)
}
#' @export
docker_pause.docker_connection<-function(x){
  id <- attr(x,".docker_id")
  rm_cmd <- paste("pause",id)
  result<-docker(rm_cmd,stdout = TRUE)
  if(is.null(attr(result,"status"))){
    message("Paused Docker Container: ",attr(x,".name"))
  }
}

#' @export
#' @title unpause a docker container
#' @description unpause a paused docker container
#' @param x either the name of the docker container or a docker_connection
#' @return NULL
#' @export
docker_unpause<-function(x){
  UseMethod("docker_unpause")
}
#' @export
docker_unpause.character<-function(x){
  conn <- docker_connection(x)
  docker_unpause.docker_connection(conn)
}
#' @export
docker_unpause.docker_connection<-function(x){
  id <- attr(x, ".docker_id")
  rm_cmd <- paste("unpause", id)
  result <- docker(rm_cmd, stdout = TRUE)
  if(is.null(attr(result, "status"))){
    message("Unpaused Docker Container: ", attr(x, ".name"))
  }
}
