#' @export
#' @title Initialize a docker container from an image
#' @description Initialize a docker image into a docker container. Generates a
#'     dockercon object to help monitor status of docker container.
#' @param image Repository address in the format username/image[:tag]
#' @param ports what port to expose and connect to in the docker container
#'     format is port:port
#' @param mountpoints what folder to expose to the docker container, where to
#'     connect to it within the docker container. format is dir:dir
#' @param docker_run_args any additional arguments to be passed to the `docker run` command.
#' @return docker_conn object
#' @examples
#' \donotrun{
#' # Start a docker container from the rocker/r-ver:devel image
#' d_conn<-docker_run(image = "rocker/r-ver:devel", name = "testimage")
#' }
docker_run <- function(image,name,ports,mountpoints,docker_run_args){
  if(!is_local_image(image) & !check_docker_imagename(image)){
    stop("Enter a valid docker image name in the format: user/image[:tag]")
  }

  if(!missing(name) & grepl("\\s",name)){
    stop("Must use a valid name without spaces")
  }

  run_cmd<-"docker run --rm -d"

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

  start_results<-system(run_cmd,intern = TRUE)

  if(!is.null(attr(start_results,"status"))){
  if(attr(start_results,"status")!=0){
    stop(start_results)
  }}

  if(missing(name)){
    running_containers<-list_containers()
    which_image<-do.call('c',lapply(running_containers$CONTAINER.ID,function(id,newcontainerid){
      grepl(paste0("^",id),newcontainerid)},
      start_results))
    name <- running_containers$NAMES[[which_image]]
  }
  docker_connection(name)
}



is_local_image <- function(name){
  images <- get_images()
  image <- strsplit(name,":")[[1]][1]
  image %in% images$REPOSITORY
}

docker_kill<-function(x){
  UseMethod("docker_kill")
}

docker_kill.character<-function(x){
  conn<-docker_connection(x)
  docker_kill.docker_connection(conn)
}

docker_kill.docker_connection<-function(x){
  id <- attr(x,".docker_id")
  kill_cmd <- paste("docker kill",id)
  result<-system(kill_cmd,intern = TRUE)
  if(is.null(attr(start_results,"status"))){
    message("Killed Docker Container: ",attr(x,".name"))
  }
}

docker_stop<-function(x){
  UseMethod("docker_stop")
}

docker_stop.character<-function(x){
  conn<-docker_connection(x)
  docker_stop.docker_connection(conn)
}

docker_stop.docker_connection<-function(x){
  id <- attr(x,".docker_id")
  stop_cmd <- paste("docker stop",id)
  result<-system(stop_cmd,intern = TRUE)
  if(is.null(attr(start_results,"status"))){
    message("Stopped Docker Container: ",attr(x,".name"))
  }
}


docker_kill.docker_connection<-function(x){
  id <- attr(x,".docker_id")
  kill_cmd <- paste("docker kill",id)
  result<-system(kill_cmd,intern = TRUE)
  if(is.null(attr(start_results,"status"))){
    message("Killed Docker Container: ",attr(x,".name"))
  }
}

docker_rm<-function(x){
  UseMethod("docker_rm")
}

docker_rm.character<-function(x){
  conn<-docker_connection(x)
  docker_rm.docker_connection(conn)
}

docker_rm.docker_connection<-function(x){
  id <- attr(x,".docker_id")
  rm_cmd <- paste("docker rm",id)
  result<-system(rm_cmd,intern = TRUE)
  if(is.null(attr(start_results,"status"))){
    message("Removed Docker Container: ",attr(x,".name"))
  }
}

