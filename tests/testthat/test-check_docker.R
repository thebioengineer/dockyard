context("test-check_docker")

# not sure of a better way to check?
test_that("`check_docker` returns TRUE/FALSE", {
  docker_installed<-check_docker()
  expect_true(docker_installed%in%c(TRUE,FALSE))
})

test_that("`check_docker_imagename` checks supplied image names for format", {

  expect_error(check_docker_imagename("bad name"),
               "image name should not contain spaces")

  expect_error(check_docker_imagename("user/Bad_Name"),
               "image name must be lowercase")

  expect_true(check_docker_imagename("user/imagename"))
  expect_true(check_docker_imagename("user/imagename:tag"))

  expect_false(check_docker_imagename("user\\imagename"))
  expect_false(check_docker_imagename("user_imagename"))
})

