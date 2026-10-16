test_that(
  "flagpoles produces a ggplot object", {
    p <- flagpoles()
    expect_s3_class(p, "ggplot")
  }
)

test_that(
  "flagpoles rejects invalid arguments", {
    expect_error(flagpoles(sd = -1))
    expect_error(flagpoles(cut = 2:3))
  }
)
