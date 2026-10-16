#' Probability density plots with shading
#'
#' Produces a ggplot2 beta distribution probability density plot with shading (alpha)
#' and vertical lines that portion out segments of the probability density. Other
#' distributions could be accommodated. My preferred styling is provided, which
#' can be overridden with theme().
#'
#' @param a alpha of beta distribution
#' @param b beta of beta distribution
#' @param direction direction of the inequality: "less than" or "greater than"
#' @param cut cutpoint(s) for estimation of probabilities
#' @param p_round Level of rounding for the probability estimate(s)
#' @param xlim minimum and maximum x values
#' @param resolution granularity of plot; does not affect probability estimate(s)
#' @param ribbon_fill fill of (sections of) the probability density area
#' @param alpha_min minimum alpha value of the fill of the probability density area
#' @param alpha_max maximum alpha value of the fill of the probability density area
#' @param curve_colour colour of the probability density curve
#' @param curve_linewidth linewidth of the probability density curve
#' @param vline_linetype linetype of the vertical segmenting line(s)
#' @param vline_colour colour of the vertical segmenting line(s)
#' @param vline_alpha alpha of the vertical segmenting line(s)
#' @param x_label x-axis label
#' @param y_label y-axis label
#' @param text_position placement of probability text
#' @param text_spacing vertical gap between multiple probability texts
#' @param box_dimensions size/position of probability text key box(es)
#'
#' @return A ggplot2 object which displays a specified beta probability
#' density function, divided at (a) specified cut-point(s), which is/are shown
#' with differing alpha values, vertical lines, and in-plot text describing
#' probabilities.
#'
#' @importFrom stats dbeta pbeta
#'
#' @export
#'
#' @examples
#' shades()
#'
#' shades(
#'   a = 15,
#'   b = 15,
#'   direction = "greater than",
#'   cut = c(0.4, 0.6, 0.65)
#'   )

shades <-
  function(
    a = 20,
    b = 10,
    direction = "less than",
    cut = c(0.6, 0.75),
    p_round = 2,
    xlim = c(0, 1),
    resolution = 2e4,
    ribbon_fill = "#6900FA",
    alpha_min = 0.2,
    alpha_max = 0.8,
    curve_colour = "black",
    curve_linewidth = 0.5,
    vline_linetype = "dashed",
    vline_colour = "black",
    vline_alpha = 0.8,
    x_label = "x",
    y_label = "Probability density",
    text_position = c(0.05, 4.5),
    text_spacing = 0.2,
    box_dimensions = c(0.025, 0.01, 0.04, 0.02)
  ) {

    stopifnot(
      is.numeric(a), length(a) == 1, a > 0,
      is.numeric(b), length(b) == 1, b > 0,
      is.numeric(cut), length(cut) >= 1,
      is.numeric(xlim), length(xlim) == 2, xlim[1] < xlim[2],
      is.numeric(resolution), length(resolution) == 1,
      is.numeric(p_round), length(p_round) == 1, p_round >= 0,
      is.character(direction), length(direction) == 1,
      direction %in% c("less than", "greater than"),
      is.numeric(alpha_min), length(alpha_min) == 1, alpha_min >= 0, alpha_min <= 1,
      is.numeric(alpha_max), length(alpha_max) == 1, alpha_max >= 0, alpha_max <= 1,
      alpha_min <= alpha_max,
      is.character(ribbon_fill), length(ribbon_fill) == 1,
      is.numeric(curve_linewidth), length(curve_linewidth) == 1,
      is.character(curve_colour), length(curve_colour) == 1,
      is.character(vline_colour), length(vline_colour) == 1,
      is.character(vline_linetype), length(vline_linetype) == 1,
      is.numeric(vline_alpha), length(vline_alpha) == 1, vline_alpha >= 0, vline_alpha <= 1,
      is.numeric(text_position), length(text_position) == 2,
      is.numeric(text_spacing), length(text_spacing) == 1,
      is.numeric(box_dimensions), length(box_dimensions) == 4
    )

    if (resolution < 1000) {warning("There may be plotting inconsistencies at low resolutions.")}

    probability_density <-
      tibble::tibble(x = seq(xlim[1], xlim[2], length.out = resolution)) |>
      dplyr::mutate(prob = stats::dbeta(x, a, b))

    less <- direction == "less than"

    cut <- if (less) {sort(cut)} else {sort(cut, decreasing = TRUE)}
    p_cut <- round(if (less) {stats::pbeta(cut, a, b)} else {1 - pbeta(cut, a, b)}, p_round)
    n_regions <- length(cut)
    alphas <- seq(alpha_max, alpha_min, length.out = n_regions)
    cut_heights <- dbeta(cut, a, b)

    plot <-
      ggplot2::ggplot(probability_density, ggplot2::aes(x = x, y = prob)) +
      ggplot2::geom_line(colour = curve_colour, linewidth = curve_linewidth) +
      ggplot2::scale_x_continuous(x_label, expand = ggplot2::expansion(0.01)) +
      ggplot2::scale_y_continuous(y_label) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 10),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor   = ggplot2::element_blank(),
        panel.border       = ggplot2::element_blank(),
        axis.line.x.bottom = ggplot2::element_line(colour = "grey20", linewidth = 0.5),
        axis.line.y.left   = ggplot2::element_line(colour = "grey20", linewidth = 0.5)
      )

    for (i in seq_along(cut)) {
      if (less) {
        ribbon_data <-
          probability_density |>
          dplyr::filter(x <= cut[i])
      } else {
        ribbon_data <-
          probability_density |>
          dplyr::filter(x >= cut[i])
      }
      plot <-
        plot +
        ggplot2::geom_ribbon(
          data = ribbon_data,
          ggplot2::aes(ymin = 0, ymax = prob),
          fill = ribbon_fill,
          alpha = alphas[i]
        ) +
        ggplot2::annotate(
          "rect",
          xmin = text_position[1] - box_dimensions[1],
          xmax = text_position[1] - box_dimensions[2],
          ymin = text_position[2] - box_dimensions[3] + (i-1)*text_spacing,
          ymax = text_position[2] + box_dimensions[4] + (i-1)*text_spacing,
          fill = ribbon_fill,
          alpha = alphas[i]
        ) +
        ggplot2::annotate(
          "text",
          x = text_position[1],
          y = text_position[2] + (i-1)*text_spacing,
          label =
            if (less) {paste0("P (x < ", round(cut[i], p_round), ") = ", p_cut[i])}
          else {paste0("P (x > ", round(cut[i], p_round), ") = ", p_cut[i])},
          hjust = 0,
          vjust = 0.5,
          size = 4
        )
    }

    plot +
      ggplot2::geom_segment(
        data = tibble::tibble(
          x = cut,
          y_end = cut_heights
        ),
        ggplot2::aes(x = x, xend = x, y = 0, yend = y_end),
        linetype = vline_linetype,
        colour = vline_colour,
        alpha = vline_alpha
      )

  }
