test_that(
  "variable_search produces a data.frame", {
    my_list <- list(iris = iris, mtcars = mtcars)
    x <- variable_search(my_list, c("sepal", "T"))
    expect_s3_class(x, "data.frame")
  }
)

test_that(
  "variable_search rejects invalid arguments", {
    expect_error(variable_search(5, c("dog")))
    expect_error(variable_search(list(iris = iris), 1))
  }
)
