context("test-docker_run")

if(check_docker()){
  if(!"connection_test_image"%in%list_images()$REPOSITORY){
    dockerfile() %>%
      from("debian:buster") %>%
      cmd("tail","-f","/dev/null") %>%
      build("connection_test_image")
  }

  if(any(c("connection_test","connection_test_2")%in%list_containers()$NAMES)){
    #kill existing test containers
    containers<- c("connection_test","connection_test2")[
      c("connection_test","connection_test2")%in%list_containers()$NAMES]

    for(container in containers){
      docker_kill(container)
    }
  }
}

test_with_docker("`docker_run` thows warnings when improper arguments are used", {

  expect_error(docker_run(image = "bad_image_name?"),
               "Enter a valid docker image name")

  expect_error(docker_run(image = "bad_image_name",
                          name = "Bad Container Name"),
               "Must use a valid name without spaces")

  expect_error(docker_run(image = "good_image_name",
                          name = "good_containe_name",
                          ports = "bad_port_designation"),
               "Must use a valid port format")
  })

test_with_docker("`docker_run` can assign a port to a docker container", {

  on.exit({
    docker_stop("port_assignment_test_container")
  })

  d_conn <- docker_run(image = "rocker/shiny",
             name = "port_assignment_test_container",
             ports = "9875:3838")

  expect_equal(attr(d_conn,".port"),
               "0.0.0.0:9875")
  expect_equal(attr(d_conn,".docker_port"),
               "3838/tcp")
})


test_with_docker("docker_stop can stop a container through either a
                 docker_connection or the name of the image", {

                   test_conn <- docker_run(image = "connection_test_image",
                                           name = "connection_test",
                                           docker_run_args = "-d")
                   test_conn_2 <- docker_run(image = "connection_test_image",
                                             name = "connection_test2",
                                             docker_run_args = "-d")

                   expect_message(docker_stop(test_conn), "Stopped Docker Container:")
                   expect_message(docker_stop("connection_test2"), "Stopped Docker Container:")
                 })

test_with_docker("docker_kill can kill a container through either a
                 docker_connection or the name of the image", {

                   test_conn <- docker_run(image = "connection_test_image",
                                           name = "connection_test",
                                           docker_run_args = "-d")
                   test_conn_2 <- docker_run(image = "connection_test_image",
                                             name = "connection_test2",
                                             docker_run_args = "-d")

                   expect_message(docker_kill(test_conn), "Killed Docker Container:")
                   expect_message(docker_kill("connection_test2"), "Killed Docker Container:")
                 })

test_with_docker("docker_pause can pause a running container through either a
                 docker_connection or the name of the image", {

                   test_conn <- docker_run(image = "connection_test_image",
                                           name = "connection_test",
                                           docker_run_args = "-d")
                   test_conn_2 <- docker_run(image = "connection_test_image",
                                             name = "connection_test2",
                                             docker_run_args = "-d")
                   on.exit({
                     docker_stop(test_conn)
                     docker_stop(test_conn_2)
                   })

                   ct_status <- grepl("Paused",get_container("connection_test")$STATUS)
                   ct2_status <-  grepl("Paused",get_container("connection_test2")$STATUS)


                   expect_message(docker_pause(test_conn), "Paused Docker Container:")
                   expect_message(docker_pause("connection_test2"), "Paused Docker Container:")

                   ct_status_post <- grepl("Paused",get_container("connection_test")$STATUS)
                   ct2_status_post <-  grepl("Paused",get_container("connection_test2")$STATUS)

                   expect_false(ct_status)
                   expect_false(ct2_status)
                   expect_true(ct_status_post)
                   expect_true(ct2_status_post)
                 })

test_with_docker("docker_pause can pause a running container through either a
                 docker_connection or the name of the image", {

                   test_conn <- docker_run(image = "connection_test_image",
                                           name = "connection_test",
                                           docker_run_args = "-d")
                   test_conn_2 <- docker_run(image = "connection_test_image",
                                             name = "connection_test2",
                                             docker_run_args = "-d")

                   on.exit({
                     docker_stop(test_conn)
                     docker_stop(test_conn_2)
                   })

                   ct_status <- grepl("Paused",get_container("connection_test")$STATUS)
                   ct2_status <-  grepl("Paused",get_container("connection_test2")$STATUS)

                   docker_pause(test_conn)
                   docker_pause("connection_test2")

                   ct_status_pause <- grepl("Paused",get_container("connection_test")$STATUS)
                   ct2_status_pause <-  grepl("Paused",get_container("connection_test2")$STATUS)

                   expect_message(docker_unpause(test_conn), "Unpaused Docker Container:")
                   expect_message(docker_unpause("connection_test2"), "Unpaused Docker Container:")

                   ct_status_unpause <- grepl("Paused",get_container("connection_test")$STATUS)
                   ct2_status_unpause <-  grepl("Paused",get_container("connection_test2")$STATUS)

                   expect_false(ct_status)
                   expect_false(ct2_status)
                   expect_true(ct_status_pause)
                   expect_true(ct2_status_pause)
                   expect_false(ct_status_unpause)
                   expect_false(ct_status_unpause)
                 })
