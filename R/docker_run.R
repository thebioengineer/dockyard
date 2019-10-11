#' @export
#' @title Initialize a docker container from an image
#' @description Initialize a docker image into a docker container. Generates a
#'     dockercon object to help monitor status of docker container.
#' @param image Repository address in the format username/image[:tag]
#' @param port what port to expose and connect to in the docker container
#'     format is port:port
#' @param mountpoint what folder to expose to the docker container, where to
#'     connect to it within the docker container. format is dir:dir
#'
#' @return dockercon
#' @examples
#' \donotrun{
#' # Start a docker container from the rocker/r-ver:devel image
#' d_conn<-docker_run(image = "rocker/r-ver:devel", name = "testimage")
#' }
docker_run <- function(image,name,port, mountpoint){
}
