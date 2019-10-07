#' @export
#' @title Describe the base docker image to use
#' @description Creating a docker image in a dockerfile requires specification
#'    of the base image to be creating your new docker image off of. {dockyard}
#'    assumes the user is basing their image off the latest available image
#'    from rocker/r-base, but it can be changed at any time.
#' @param df a dockerfile object from `dockerfile()`
#' @param image Repository address in the format username/image[:tag]
#' @return dockerfile
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1
#' dockerfile() %>%
#'   from("rocker/shiny:3.6.1")
#' @family dockerfile
#'
from <- function(df, image = "rocker/r-base") {
  add_base_image(df, image)
}


#' @export
#' @title Run update docker image
#' @description Add command when creating docker image to update installed software
#' @param df a dockerfile object from `dockerfile()`
#' @return dockerfile
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1
#' dockerfile() %>%
#'   from("rocker/shiny:3.6.1") %>%
#'   update()
#' @family dockerfile
#'
update <- function(df) {
  add_command(df, "RUN apt-get update -qq")
}

#' @export
#' @title Apt-get install software
#' @description Add software to install to docker image via `apt-get`
#' @param df a dockerfile object from `dockerfile()`
#' @param ... The software to install to the docker image
#' @return dockerfile
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl.
#'
#' dockerfile() %>%
#'   from("rocker/shiny:3.6.1") %>%
#'   update() %>%
#'   install("git", "libcurl4-openssl-dev")
#' @family dockerfile
#'
install <- function(df, ...) {
  to_install <- match.call(expand.dots = FALSE)$`...`

  install_command <- paste(
    "RUN apt-get install -y --no-install-recommends ",
    paste0(to_install, collapse = " ")
  )
  add_command(df, install_command)
}

#' @export
#' @title Install packages for R
#' @description install packages from the default CRAN location for R
#' @param df a dockerfile object from `dockerfile()`
#' @param ... The R packages to install to the docker image
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl.
#'
#' dockerfile() %>%
#'   from("rocker/shiny:3.6.1") %>%
#'   update() %>%
#'   install("git", "libcurl4-openssl-dev") %>%
#'   install_r_lib("rcurl", "dplyr")
#' @family dockerfile
#'
install_r_lib <- function(df, ...) {
  to_install <- match.call(expand.dots = FALSE)$`...`

  install_command <- paste(
    "RUN R -e \"install.packages(c(",
    paste0("'", to_install, "'", collapse = ", "),
    "))\""
  )
  add_command(df, install_command)
}


#' @export
#' @title RUN command in docker image
#' @description Command to be run in the docker image during building the image.
#' @param df a dockerfile object from `dockerfile()`
#' @param cmd command to be run in the docker image during build
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl. Then initialize Rstudio server
#'
#' dockerfile() %>%
#'   from("rocker/shiny:3.6.1") %>%
#'   update() %>%
#'   install("git", "libcurl4-openssl-dev") %>%
#'   install_r_lib("rcurl", "dplyr") %>%
#'   run("wget --no-check-certificate https://raw.githubusercontent.com/rocker-org/rstudio-daily/master/latest.R") %>%
#'   run("Rscript latest.R") %>%
#'   run("dpkg -i rstudio-server-daily-amd64.deb")
#' @family dockerfile
#'
run <- function(df, cmd) {
  install_command <- paste("RUN", cmd)
  add_command(df, install_command)
}


#' @export
#' @title RUN command in docker image
#' @description Command to cope files  in the docker image during building the image.
#' @param df a dockerfile object from `dockerfile()`
#' @param source file to be copied from your local computer
#' @param dir location the file is to be copied to
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl. Then initialize Rstudio server, and copy in a config file
#'
#' dockerfile() %>%
#'   from("rocker/shiny:3.6.1") %>%
#'   update() %>%
#'   install("git", "libcurl4-openssl-dev") %>%
#'   install_r_lib("rcurl", "dplyr") %>%
#'   run("wget --no-check-certificate https://raw.githubusercontent.com/rocker-org/rstudio-daily/master/latest.R") %>%
#'   run("Rscript latest.R") %>%
#'   run("dpkg -i rstudio-server-daily-amd64.deb") %>%
#'   copy("userconf.sh", "/etc/cont-init.d/userconf")
#' @family dockerfile
#'
copy <- function(df, source, dir) {
  install_command <- paste("COPY", source, dir)
  add_command(df, install_command)
}

#' @export
#' @title RUN command in docker image
#' @description Command to cope files  in the docker image during building the image.
#' @param df a dockerfile object from `dockerfile()`
#' @param source file to be copied from your local computer
#' @param dir location the file is to be copied to
#' @examples
#' # Start a dockerfile based off of the rocker/shiny image to generate a
#' # shiny server using R version 3.6.1, update all existing software
#' # and install git and curl. Then initialize Rstudio server, and copy in a config file
#'
#' new_dockerfile <- tempfile(pattern = "new_dockerfile_", fileext = "")
#'
#'
#' df <- dockerfile() %>%
#'   from("rocker/shiny:3.6.1") %>%
#'   update() %>%
#'   install("git", "libcurl4-openssl-dev") %>%
#'   install_r_lib("rcurl", "dplyr") %>%
#'   run("wget --no-check-certificate https://raw.githubusercontent.com/rocker-org/rstudio-daily/master/latest.R") %>%
#'   run("Rscript latest.R") %>%
#'   run("dpkg -i rstudio-server-daily-amd64.deb") %>%
#'   copy("userconf.sh", "/etc/cont-init.d/userconf") %>%
#'   save(new_dockerfile)
#' @family dockerfile
save <- function(df, path, overwrite = FALSE) {
  if (file.exists(path) & !overwrite) {
    stop("File already exists. Change overwrite to TRUE to overwrite existing dockerfile")
  }
  writeLines(commands(df), path, sep = "\n")
  attr(df, ".dockerfile") <- path
  class(df) <- "dockerfile"
  invisible(df)
}
