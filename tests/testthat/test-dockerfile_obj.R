context("test-dockerfile_obj")

test_that("dockerfile objects initialized without a start file are blank slates", {
  df<-dockerfile()
  expect_equal(as.character(df),"")
  expect_equal(attr(df,".dockerfile"),"")
  expect_equal(attr(df,".base_image"),"")
  expect_s3_class(df,c("dockerfile"))
})

test_that("dockerfile objects initialized with a start file import all the information", {
  dockerfile_path <- "testfiles/shiny_sample_dockerfile"
  df<-dockerfile(dockerfile_path)
  expect_equal(length(as.character(df)),4)
  expect_equal(attr(df,".dockerfile"),dockerfile_path)
  expect_equal(attr(df,".base_image"),"rocker/r-ver:devel")
  expect_s3_class(df,c("dockerfile"))
})


test_that("dockerfile objects become `edited_dockerfile` when commands are added", {
  df<-dockerfile()
  df<-df %>%
    from("rocker/r-ver:devel") %>%
    update()

  expect_equal(commands(df),c("FROM rocker/r-ver:devel","RUN apt-get update -qq"))
  expect_equal(attr(df,".dockerfile"),"")
  expect_equal(attr(df,".base_image"),"rocker/r-ver:devel")
  expect_s3_class(df,c("edited_dockerfile","dockerfile"))
})


test_that("printing dockerfile and edited_dockerfile objects lists all commands as they would be run when building.", {
  dockerfile_path <- "testfiles/shiny_sample_dockerfile"
  df <- dockerfile(dockerfile_path)
  df_edited <- df %>%
    run("Test New Command")

  dockerfile()


  output <- capture.output(print(df))
  output_edited <- capture.output(print(df_edited))

  original_df_lines<-readLines(dockerfile_path)

  # remove printing decoration
  cleaned_print_output <- trimws(gsub("\\t","",output[-c(1,2)]))
  cleaned_print_output_edited <- trimws(gsub("\\t","",output_edited[-c(1,2)]))

  expect_equal(cleaned_print_output,
               original_df_lines)
  expect_equal(cleaned_print_output_edited,
               c(original_df_lines,
                 "RUN Test New Command"))
  capture.output({
  expect_warning(print(dockerfile()),
                 "No base image defined")
  })
})

test_that("dockerfile objects can have new steps added to them, making them edited_dockefile objects", {
  df<-dockerfile()
  df_edited<-df %>%
    from("rocker/r-ver:devel") %>%
    update() %>%
    add_command("New command to add")

  expect_equal(commands(df_edited),
               c("FROM rocker/r-ver:devel",
                 "RUN apt-get update -qq",
                 "New command to add"))
  expect_s3_class(df,
                  c("dockerfile"))
  expect_s3_class(df_edited,
                  c("edited_dockerfile", "dockerfile"))

})

test_that("dockerfile objects can have steps edited, making them edited_dockefile objects", {
  df<-dockerfile()
  df_edited<-df %>%
    from("rocker/r-ver:devel") %>%
    run("This command to edit") %>%
    update_command(1,"edited command")

  expect_equal(commands(df_edited),
               c("FROM rocker/r-ver:devel",
                 "edited command"))
  expect_s3_class(df,
                  c("dockerfile"))
  expect_s3_class(df_edited,
                  c("edited_dockerfile", "dockerfile"))

})


test_that("dockerfile objects can have base images assigned", {
  df <- dockerfile()
  df_base_image <- add_base_image(df,"new_base_image")

  expect_equal(attr(df, ".base_image"),"")
  expect_equal(attr(df_base_image, ".base_image"),"new_base_image")
  expect_warning(add_base_image(df_base_image,"new_base_image2"),
                 "Overwriting original base image")
})

test_that("`commands` returns the list of commands to be executed from the dockerfile",{
  df<-dockerfile()
  df_base_image<-df %>%
    from("rocker/r-ver:devel") %>%
    update()

  df_no_base_image<-df %>%
    update()

  expect_equal(commands(df_base_image),
               c("FROM rocker/r-ver:devel","RUN apt-get update -qq"))
  expect_error(commands(df_no_base_image),
                 "No base image defined.")
})

