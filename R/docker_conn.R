
#' @title Define the connection to the docker container
#'
#' @details This object defines the connection to the docker image when it can accept
#' external inputs
#'
#' @param name the name of the docker image

#' @return dockefile object
#'
docker_connection <- function(name){
  container_table <- get_containers()
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

get_containers<-function(containername){
  dockercontainers<-tempfile()
  writeLines(system("docker ps -a",intern = TRUE),dockercontainers)
  header<-readLines(dockercontainers,n = 1)

  locs<-as.numeric(gregexpr(pattern = "\\s\\s\\w",header)[[1]])
  widths<-c(locs[1],diff(locs),100)
  containers<-read.fwf(dockercontainers,widths = widths,strip.white=TRUE)

  containers<-read.csv(text=paste(apply(containers,1,
           function(x){paste(trimws(x),collapse=",")}),"\n"),
           stringsAsFactors = FALSE)

  containers$NAMES<-trimws(containers$NAMES)

  if(!missing(containername)){
    containers[containers$NAME == containername,]
  }else{
    containers
  }
}



print.docker_connection<-function(x){

  container<-get_containers(attr(x,".name"))

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


