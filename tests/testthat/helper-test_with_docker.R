#provide wrapper for tests that require docker installed

test_with_docker <- function(desc, ...) {
  if(check_docker()){
      testthat::test_that(desc = desc, ...)
  }
  invisible()
}
