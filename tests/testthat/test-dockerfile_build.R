context("test-dockerfile_build")

test_that("build_from_dockerfile throws errors if the file does not exist or
          the image format is incorrect", {

    expect_error(build_from_dockerfile(path = "NONEXISTITENT_DOCKERFILE"),
                 "dockerfile does not exist")
    expect_error(build_from_dockerfile(path = "testfiles/basic_dockerfile",
                                       image = "badimagename?"),
                 "needs to be a valid image name")
})



test_with_docker("build_from_dockerfile when dockerfile and image format are correct builds image",{

    if("dockyard_test"%in%list_images()$REPOSITORY){
      system('docker image rm dockyard_test',
             show.output.on.console = FALSE)
    }

    images<-list_images()

    build_from_dockerfile(
      path = "testfiles/basic_dockerfile",
      image = "dockyard_test",
      builddir = "testfiles",
      verbose=FALSE)

    latest_images <- list_images()
    results <- system("docker run --rm --name test dockyard_test",
                      intern = TRUE,
                      show.output.on.console = FALSE)

    expect_false( "dockyard_test"%in%images$REPOSITORY)
    expect_true( "dockyard_test"%in%latest_images$REPOSITORY)
    expect_equal(results,
                 c("Hello from {dockyard}!",
                   paste("This message shows that your installation of {dockyard}",
                         "and Docker appears to be working correctly.")))
    system('docker image rm dockyard_test',
           ignore.stdout = TRUE,
           show.output.on.console = FALSE)
})


test_with_docker("build.character can build an image from a path to a dockerfile",{

  if("dockyard_test"%in%list_images()$REPOSITORY){
    system('docker image rm dockyard_test',
           show.output.on.console = FALSE)
  }

  images<-list_images()

  build.character(
    "testfiles/basic_dockerfile",
    image = "dockyard_test",
    builddir = "testfiles",
    verbose=FALSE)

  latest_images <- list_images()
  results <- system("docker run --rm --name test dockyard_test",
                    intern = TRUE,
                    show.output.on.console = FALSE)

  expect_false( "dockyard_test"%in%images$REPOSITORY)
  expect_true( "dockyard_test"%in%latest_images$REPOSITORY)
  expect_equal(results,
               c("Hello from {dockyard}!",
                 paste("This message shows that your installation of {dockyard}",
                       "and Docker appears to be working correctly.")))
  system('docker image rm dockyard_test',
         ignore.stdout = TRUE,
         show.output.on.console = FALSE)
})

test_with_docker("build.dockerfile can build an image from a path to a dockerfile",{

  if("dockyard_test"%in%list_images()$REPOSITORY){
    system('docker image rm dockyard_test',
           show.output.on.console = FALSE)
  }

  images<-list_images()

  dockerfile("testfiles/basic_dockerfile") %>%
  build(
    image = "dockyard_test",
    builddir = "testfiles",
    verbose=FALSE)

  latest_images <- list_images()
  results <- system("docker run --rm --name test dockyard_test",
                    intern = TRUE,
                    show.output.on.console = FALSE)

  expect_false( "dockyard_test"%in%images$REPOSITORY)
  expect_true( "dockyard_test"%in%latest_images$REPOSITORY)
  expect_equal(results,
               c("Hello from {dockyard}!",
                 paste("This message shows that your installation of {dockyard}",
                       "and Docker appears to be working correctly.")))
  system('docker image rm dockyard_test',
         ignore.stdout = TRUE,
         show.output.on.console = FALSE)
})


test_with_docker("build.dockerfile can build an image from a path to a dockerfile",{

  if("dockyard_test"%in%list_images()$REPOSITORY){
    system('docker image rm dockyard_test',
           show.output.on.console = FALSE)
  }

  images<-list_images()

  dockerfile() %>%
    from("debian:buster") %>%
    add("hello_world.txt", "/") %>%
    cmd("echo","This message shows if your installation worked!") %>%
    build(
      image = "dockyard_test",
      builddir = "testfiles",
      verbose=FALSE)

  latest_images <- list_images()
  results <- system("docker run --rm --name test dockyard_test",
                    intern = TRUE,
                    show.output.on.console = FALSE)

  expect_false( "dockyard_test"%in%images$REPOSITORY)
  expect_true( "dockyard_test"%in%latest_images$REPOSITORY)
  expect_equal(results,
               "This message shows if your installation worked!")
  system('docker image rm dockyard_test',
         ignore.stdout = TRUE,
         show.output.on.console = FALSE)
})
