% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docker_conn.R
\name{get_container}
\alias{get_container}
\title{Get information on specific docker container}
\usage{
get_container(name)
}
\arguments{
\item{name}{the name of the container of interest}
}
\value{
data.frame of docker containers
}
\description{
Get information on specific docker container
}
\details{
This function asks the docker daemon to list all the available
   information for a specific docker comnainer.
}
