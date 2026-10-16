test_that(
  "ordinal_bars produces a ggplot object", {
    set.seed(1)
    data <-
      data.frame(
        category = rep(c(rep("Burn", 5), rep("Nausea", 5), rep("Headache", 5), rep("Pain", 5), rep("Other", 5)), 2),
        ordinal = as.factor(rep(c(1,2,3,4,5), 10)),
        count = c(rpois(25, 1), rpois(25, 1.5)),
        group = c(rep("Control", 25), rep("Intervention", 25))
        )
    p <- ordinal_bars(data, category, ordinal, count, group, sort = "descending")
    expect_s3_class(p, "ggplot")
  }
)

test_that(
  "ordinal_bars rejects invalid arguments", {
    expect_error(ordinal_bars(data = 5))
    expect_error(ordinal_bars(ordinal = -1))
  }
)
