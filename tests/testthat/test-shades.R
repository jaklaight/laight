test_that(
  "shades produces a ggplot object", {
    p <- shades()
    expect_s3_class(p, "ggplot")
  }
)

test_that(
  "shades rejects invalid arguments", {
    expect_error(shades(a = -1))
    expect_error(shades(cut = "a"))
  }
)
