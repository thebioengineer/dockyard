context("test-dockerfile_build")

test_that("build_from_dockerfile throws errors if the file does not exist or
          the image format is incorrect", {

    expect_error(build_from_dockerfile(path = "NONEXISTITENT_DOCKERFILE"),
                 "dockerfile does not exist")
    expect_error(build_from_dockerfile(path = "testfiles/basic_dockerfile",
                                       image = "badimagename?"),
                 "needs to be a valid image name")
})



test_that("build_from_dockerfile when dockerfile and image format are correct builds image",{
  if(check_docker()){
    images<-list_images()
    if("dockyard_test"%in%images$REPOSITORY){
      removal_output<-capture.output(system('docker image rm dockyard_test'))
    }

    images<-list_images()

    build_from_dockerfile(
      path = "testfiles/basic_dockerfile",
      image = "dockyard_test",
      builddir = "testfiles")
    latest_images<-list_images()
    expect_false("dockyard_test"%in%images$REPOSITORY)
    expect_true("dockyard_test"%in%latest_images$REPOSITORY)

    results <- system("docker run --rm --name test dockyard_test",intern = TRUE)
    expect_equal(results,
                 c("Hello from {dockyard}!",
                   "This message shows that your installation of {dockyard} and Docker appears to be working correctly."))
  }
})
