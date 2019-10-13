context("test-dockerbuild")

test_that("dockerbuild objects initialized without a start file are blank slates", {
  df<-dockerfile()
  expect_equal(as.character(df),"")
  expect_equal(attr(df,".dockerfile"),"")
  expect_equal(attr(df,".base_image"),"")
  expect_s3_class(df,c("dockerfile"))
})

test_that("dockerbuild objects initialized with a start file import all the information", {
  dockerfile_path <- "testfiles/shiny_sample_dockerfile"
  df<-dockerfile(dockerfile_path)
  expect_equal(length(as.character(df)),4)
  expect_equal(attr(df,".dockerfile"),dockerfile_path)
  expect_equal(attr(df,".base_image"),"rocker/r-ver:devel")
  expect_s3_class(df,c("dockerfile"))
})


test_that("dockerbuild objects become `edited_dockerfile` when commands are added", {
  df<-dockerfile()
  df<-df %>%
    from("rocker/r-ver:devel") %>%
    update()

  expect_equal(commands(df),c("FROM rocker/r-ver:devel","RUN apt-get update -qq"))
  expect_equal(attr(df,".dockerfile"),"")
  expect_equal(attr(df,".base_image"),"rocker/r-ver:devel")
  expect_s3_class(df,c("edited_dockerfile","dockerfile"))
})
