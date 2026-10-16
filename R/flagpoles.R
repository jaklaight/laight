#' Probability density plots with "flagpoles"
#'
#' Produces a ggplot2 normal distribution probability density plot with flagpoles,
#' text, and colours that portion out segments of the probability density, similar
#' to a plot shown here: https://jamanetwork.com/journals/jama/fullarticle/2847011.
#' This function currently produces a single two-way flagpole, but can be extended
#' to accommodate additional flagpoles. Other distributions could be accommodated.
#' My preferred styling is provided, which can be overridden with theme().
#'
#' @param mean Mean of normal distribution
#' @param sd Standard deviation of normal distribution
#' @param cut The cut-point where the flagpole will be positioned
#' @param text Text to precede probability estimates
#' @param p_round Level of rounding for the probability estimates
#' @param xlim Minimum and maximum x values
#' @param resolution Granularity of plot; does not affect probablity estimates
#' @param flag_x x end-point of the flags (arrowheads)
#' @param flag_y y position of the flags
#' @param text_x x position of the texts
#' @param text_y y position of the texts
#' @param text_size size of the texts
#' @param curve_colour colour of the probability density curve
#' @param curve_linewidth linewidth of the probability density curve
#' @param ribbon_fill fills of the area of the probability density
#' @param ribbon_alpha alphas of the area of the probability density
#' @param flagpole_fill fill of the flagpole
#' @param flagpole_linewidth linewidth of the flagpole
#' @param flagpole_alpha alpha of the flagpole
#' @param x_label x-axis label
#' @param y_label y-axis label
#'
#' @return A ggplot2 object which displays a specified normal probability
#' density function, divided at a specified cut-point.
#'
#' @importFrom stats dnorm pnorm
#'
#' @export
#'
#' @examples
#' flagpoles()
#' flagpoles(mean = 2, sd = 1, cut = 1, xlim = c(-2, 6))


flagpoles <- function(
    mean = 0,
    sd = 1,
    cut = 0,
    text = c(paste0("P(x < ", cut, ") = "), paste0("P(x > ", cut, ") = ")),
    p_round = 2,
    xlim = c(-4, 4),
    resolution = 2e4,
    flag_x = c(cut - 1, cut + 1),
    flag_y = c(0.5, 0.45),
    text_x = c(cut - 1.5, cut + 1.5),
    text_y = c(0.5, 0.45),
    text_size = 4,
    curve_colour = "black",
    curve_linewidth = 0.5,
    ribbon_fill = c("#0000ff", "#fa8775"),
    ribbon_alpha = c(0.7, 0.7),
    flagpole_fill = "black",
    flagpole_linewidth = 0.6,
    flagpole_alpha = 0.7,
    x_label = "x",
    y_label = "Probability density"
    ) {

  stopifnot(
    is.numeric(mean), length(mean) == 1,
    is.numeric(sd), length(sd) == 1, sd > 0,
    is.numeric(cut), length(cut) == 1,
    is.character(text), length(text) == 2,
    is.numeric(p_round), length(p_round) == 1, p_round >= 0, p_round == floor(p_round),
    is.numeric(xlim), length(xlim) == 2, xlim[1] < xlim[2],
    is.numeric(resolution), length(resolution) == 1,
    is.numeric(flag_x), length(flag_x) == 2,
    is.numeric(flag_y), length(flag_y) == 2,
    is.numeric(text_x), length(text_x) == 2,
    is.numeric(text_y), length(text_y) == 2,
    is.numeric(text_size), length(text_size) == 1,
    is.character(curve_colour), length(curve_colour) == 1,
    is.numeric(curve_linewidth), length(curve_linewidth) == 1,
    is.character(ribbon_fill), length(ribbon_fill) == 2,
    is.numeric(ribbon_alpha), length(ribbon_alpha) == 2,
    all(ribbon_alpha >= 0), all(ribbon_alpha <= 1),
    is.character(flagpole_fill), length(flagpole_fill) == 1,
    is.numeric(flagpole_linewidth), length(flagpole_linewidth) == 1,
    is.numeric(flagpole_alpha), length(flagpole_alpha) == 1,
    flagpole_alpha >= 0, flagpole_alpha <= 1,
    is.character(x_label), length(x_label) == 1,
    is.character(y_label), length(y_label) == 1
    )

  if (resolution < 1000) {warning("There may be plotting inconsistencies at low resolutions.")}

  probability_density <-
    tibble::tibble(x = seq(xlim[1], xlim[2], length.out = resolution)) |>
    dplyr::mutate(prob = dnorm(x, mean, sd))

  p_cut <- pnorm(cut, mean, sd)

  ggplot2::ggplot(probability_density, ggplot2::aes(x = x, y = prob)) +
    ggplot2::geom_line(colour = curve_colour, linewidth = curve_linewidth) +
    ggplot2::geom_ribbon(data = dplyr::filter(probability_density, x < cut), ggplot2::aes(ymin = 0, ymax = prob),
                fill = ribbon_fill[1], alpha = ribbon_alpha[1]) +
    ggplot2::geom_ribbon(data = dplyr::filter(probability_density, x > cut), ggplot2::aes(ymin = 0, ymax = prob),
                fill = ribbon_fill[2], alpha = ribbon_alpha[2]) +
    ggplot2::annotate("segment", x = cut, y = 0, yend = Inf, colour = flagpole_fill, alpha = flagpole_alpha, linewidth = flagpole_linewidth) +
    ggplot2::annotate("segment", x = cut, xend = flag_x[1], y = flag_y[1],
             arrow = ggplot2::arrow(length = grid::unit(0.15, "cm"))) +
    ggplot2::annotate("text", x = text_x[1], y = text_y[1],
             label = paste0(text[1], round(p_cut, p_round)), size = text_size) +
    ggplot2::annotate("segment", x = cut, xend = flag_x[2], y = flag_y[2],
             arrow = ggplot2::arrow(length = grid::unit(0.15, "cm"))) +
    ggplot2::annotate("text", x = text_x[2], y = text_y[2],
             label = paste0(text[2], round(1 - p_cut, p_round)), size = text_size) +
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
}
