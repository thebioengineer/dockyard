context("test-docker_conn")

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

test_with_docker("Creating a docker container generates a docker_conn object", {
  test_conn <- docker_run(image = "debian:buster",
                          name = "connection_test",
                          docker_run_args = "-d")
  expect_s3_class(test_conn, "docker_connection")

})

test_with_docker("A new docker_connection object can be generated using the
                 name of the container if it still exists", {

  test_conn <- docker_run(image = "connection_test_image",
                          name = "connection_test",
                          docker_run_args = "-d")
  on.exit({
    docker_stop(test_conn)
    })
  test_conn_2 <- docker_connection("connection_test")

  expect_s3_class(test_conn, "docker_connection")
  expect_s3_class(test_conn_2, "docker_connection")
  expect_equal(test_conn_2, test_conn)
})

test_with_docker("A `docker_connection` cannot be made for a container that does
                 not exist",{
  expect_error(docker_connection("Poorly_named_and_illegal_Dockyard_Container"),
               "Must be a valid container name")
})

test_with_docker("Printing `docker_connection` lists all available info about
                 the container; name, port, base images, status",{
  on.exit({
    if("connection_test"%in%list_containers()$NAMES){
      docker_stop("connection_test")
    }
  })

  test_conn <- docker_run(image = "connection_test_image",
                          name = "connection_test",
                          docker_run_args = "-d")

  conn_output <- capture.output({
    print(test_conn)})

  docker_stop("connection_test")

  conn_output2 <- capture.output({
    print(test_conn)})

  expect_true(grepl(
    "[<] Docker Container[:] connection_test [|] Port[:] NA [|] Status[:] Up .+[|] image[:] connection_test_image [>]",
    conn_output))

  expect_true(grepl(
    "[<] Docker Container[:] connection_test [|] Port[:] N/A [|] Status[:] Does Not Exist [|] image[:] connection_test_image [>]",
    conn_output2))

})






