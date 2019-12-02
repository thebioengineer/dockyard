
#' @title Define the connection to the docker container
#'
#' @details This object defines the connection to the docker image when it can accept
#' external inputs
#'
#' @param name the name of the docker image

#' @return dockefile object
#' @exportClass docker_connection docker_container
#' @export
docker_connection <- function(name){
  container_table <- list_containers()
  if(!(name%in%container_table$NAMES)){
    stop("Must be a valid container name:\n", paste("\t", container_table$NAMES, "\n"))
  }

  docker_info <- container_table[container_table$NAME==name, ]
  if(!is.na(docker_info$PORTS)){
    internal_port <- strsplit(docker_info$PORTS,"->")[[1]][1]
    docker_port <- strsplit(docker_info$PORTS,"->")[[1]][2]
  }else{
    internal_port <- "NA"
    docker_port <- "NA"
  }

  docker_status = strsplit(docker_info$STATUS," ")[[1]][1]

  structure(
    .Data        = name,
    .name        = name,
    .image       = docker_info$IMAGE,
    .port        = internal_port,
    .docker_port = docker_port,
    .docker_id   = docker_info$CONTAINER.ID,
    class = c("docker_connection","docker_container")
  )
}


#' @title List all docker containers available to docker daemon
#'
#' @details This function asks the docker daemon to list all the available
#'    docker comtainers that are available.
#'
#' @return data.frame of docker containers
#'
#' @importFrom utils read.csv read.fwf
#' @export
list_containers<-function(){
  dockercontainers<-tempfile()
  writeLines(docker("ps -a",stdout = TRUE),dockercontainers)
  header<-readLines(dockercontainers,n = 1)

  locs<-as.numeric(gregexpr(pattern = "\\s\\s\\w",header)[[1]])
  widths<-c(locs[1],diff(locs),100)
  containers<-read.fwf(dockercontainers,widths = widths,strip.white=TRUE)

  containers<-read.csv(text=paste(apply(containers,1,
           function(x){paste(trimws(x),collapse=",")}),"\n"),
           stringsAsFactors = FALSE)

  containers$NAMES<-trimws(containers$NAMES)
  containers
}

#' @title Get information on specific docker container
#'
#' @details This function asks the docker daemon to list all the available
#'    information for a specific docker comnainer.
#'
#' @param name the name of the container of interest
#'
#' @return data.frame of docker containers
#' @export
get_container <- function(name){
  containers <- list_containers()
  containers[containers$NAME == name,]
}



#' @title Print method for docker_conection objects
#'
#' @param x docker_connection object to be printed
#' @param ... additional paramters, ignored for `print.docker_connection`

#' @export
print.docker_connection<-function(x,...){

  container<-get_container(attr(x,".name"))

  status <- if(nrow(container)==0){
    "Does Not Exist"
  }else{
    container$STATUS
  }

  port <- if(status=="Does Not Exist"){
    "N/A"
  }else{
    attr(x,".port")
  }

  cat(paste("< Docker Container:", attr(x,".name"),
            "| Port:", port,
            "| Status:", status,
            "| image:", attr(x,".image"),
            ">"))
}


#' @title List all locally exising docker images available to docker daemon
#'
#' @details This function asks the docker daemon to list all the available
#'    docker images that are available.
#'
#' @return data.frame of docker images
#'
#' @importFrom utils read.csv read.fwf
#' @export
list_images<-function(){
  dockerimages<-tempfile()
  writeLines(docker("image ls ",stdout = TRUE),dockerimages)
  header<-readLines(dockerimages,n = 1)

  locs<-as.numeric(gregexpr(pattern = "\\s\\s\\w",header)[[1]])
  widths<-c(locs[1],diff(locs),100)
  containers<-read.fwf(dockerimages,widths = widths,strip.white=TRUE)

  read.csv(text=paste(apply(containers,1,
                            function(x){paste(trimws(x),collapse=",")}),"\n"),
           stringsAsFactors = FALSE)

}

