test_that(
  "recruitment_plot produces a ggplot object", {
    set.seed(1)
    data <-
      data.frame(
        time = seq(as.Date("2024-10-01"), as.Date("2026-09-01"), by = "month"),
        recruited = rpois(24, 3),
        projected = cumsum(rep(2, 24))
      )
    data$time <- factor(format(data$time, "%b %Y"), levels = format(data$time, "%b %Y"))
    p <- recruitment_plot(data, time, recruited, projected)
    expect_s3_class(p, "ggplot")
  }
)

test_that(
  "recruitment_plot rejects invalid arguments", {
    expect_error(recruitment_plot(data = 5))
    expect_error(recruitment_plot(time_breaks = -1))
  }
)
