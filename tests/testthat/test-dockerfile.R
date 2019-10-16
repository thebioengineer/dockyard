context("test-dockerfile")

test_that("`from` adds a base image to dockerfile object ", {
  df_base_image <- dockerfile() %>%
    from("rocker/r-ver:devel")

  expect_equal(attr(df_base_image, ".base_image"),"rocker/r-ver:devel")
  expect_error(from(dockerfile(),"invalidimagename?"),
               "Enter a valid docker image")
})

test_that("`update` adds a command to update all existing linux software ", {
  df_update <- dockerfile() %>%
    update()

  expect_equal(as.character(df_update),
               "RUN apt-get update -qq")
})

test_that("`install` adds a command to install linux software ", {
  df_install <- dockerfile() %>%
    install("curl")
  df_install_2 <- dockerfile() %>%
    install("curl","wget")

  expect_equal(as.character(df_install),
               "RUN apt-get install -y  curl")
  expect_equal(as.character(df_install_2),
               "RUN apt-get install -y  curl wget")
})

test_that("`install_r_lib` adds a command to install R packages to image ", {
  df_install_r_lib <- dockerfile() %>%
    install_r_lib("ggplot2")
  df_install_r_lib_2 <- dockerfile() %>%
    install_r_lib("ggplot2","car", "dplyr")

  expect_equal(as.character(df_install_r_lib),
               "RUN R -e \"install.packages(c( 'ggplot2' ))\"")
  expect_equal(as.character(df_install_r_lib_2),
               "RUN R -e \"install.packages(c( 'ggplot2', 'car', 'dplyr' ))\"")
})

test_that("`run` adds commands to evaluate as though in the linux command line", {
  df_run <- dockerfile() %>%
    run("this command")

  df_run_2 <- dockerfile() %>%
    run("this command") %>%
    run("then this command")

  expect_equal(as.character(df_run),
               "RUN this command")
  expect_equal(as.character(df_run_2),
               "RUN this command && then this command")
})

test_that("`expose` adds commands to expose a port between 1023 and 65535", {
  df_expose <- dockerfile() %>%
    expose(1234)

  expect_equal(as.character(df_expose),
               "EXPOSE 1234")
  expect_error(expose(dockerfile(),100),
               "Enter a valid port number")
})

test_that("`copy` adds commands to copy files from local host from build dir", {
  df_copy <- dockerfile() %>%
    copy("local_host/folder","/docker/folder/path")

  expect_equal(as.character(df_copy),
               "COPY local_host/folder /docker/folder/path")
})

test_that("`copy_from_url` adds commands to curl files from a url into the docker container", {
  df_copy_from_url <- dockerfile() %>%
    copy_from_url("www.some_url.com/resolve/this","/docker/folder/path")

  expect_equal(as.character(df_copy_from_url),
               "RUN curl www.some_url.com/resolve/this > /docker/folder/path")
})

test_that("`adds` adds commands to copy files from local host", {
  df_add <- dockerfile() %>%
    add("testfiles/shiny_sample_dockerfile","/docker/folder/path")

  expect_equal(as.character(df_add),
               "ADD testfiles/shiny_sample_dockerfile /docker/folder/path")
  expect_error(add(dockerfile(),"fakefile","/docker/folder/path"),
               "`source` not found")
})

test_that("`cmd` adds the final command or file to execute on container initialization", {
  df_cmd <- dockerfile() %>%
    cmd("run_this.sh")

  expect_equal(as.character(df_cmd),
               "CMD [\"run_this.sh\"]")
})

