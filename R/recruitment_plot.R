#' Cumulative and time-period-wise recruitment plot
#'
#' Produce a ggplot2 object which depicts cumulative recruitment (such as to a
#' clinical trial), time-period-wise (e.g. monthly) recruitment, and predicted
#' cumulative recruitment (supply your own predictions!).
#'
#' @param data a dataset containing the variables used in the plot
#' @param time name of variable in data which concerns each time point to be
#' plotted; must be a character or factor
#' @param recruited name of the variable in data which provides the number of
#' patients recruited at each time
#' @param projected name of the variable in data which provides projected
#' recruitment at each time (N.B.: this must be cumulative)
#' @param time_breaks plotting breaks for time
#' @param y_scaling scalar that defines the relationship between the two y axes
#' (number per time and cumulative number)
#' @param recruited_label name for number recruited per time on plot
#' @param cumulative_label name for cumulative recruitment on plot
#' @param projected_label name for projected recruitment on plot
#' @param recruited_fill fill of recruited per time bars
#' @param recruited_alpha alpha of recruited per time bars
#' @param cumulative_colour colour of cumulative recruitment line
#' @param cumulative_alpha alpha of cumulative recruitment line
#' @param cumulative_linewidth linewidth of cumulative recruitment line
#' @param projected_colour colour of projected recruitment line
#' @param projected_alpha alpha of projected recruitment line
#' @param projected_linewidth linewidth of projected recruitment line
#' @param projected_linetype linetype of projected recruitment line
#' @param xlab x-axis label
#' @param title plot title
#' @param date_text_angle angle at which the dates are printed
#' @param date_text_vjust vjust of date text
#' @param date_text_hjust hjust of date text
#' @param date_text_size size of date text
#'
#' @return A ggplot2 object which depicts recruitment
#'
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' data <-
#'   data.frame(
#'     time = seq(as.Date("2024-10-01"), as.Date("2026-09-01"), by = "month"),
#'     recruited = rpois(24, 3),
#'     projected = cumsum(rep(2, 24))
#'   )
#' data$time <- factor(format(data$time, "%b %Y"), levels = format(data$time, "%b %Y"))
#' recruitment_plot(data, time, recruited, projected)
#'
#'

recruitment_plot <-
  function(
    data,
    time = NULL,
    recruited = NULL,
    projected = NULL,
    time_breaks = 1,
    y_scaling = 1,
    recruited_label = "Recruited per month",
    cumulative_label = "Cumulative recruitment",
    projected_label = "Projected recruitment",
    recruited_fill = "forestgreen",
    recruited_alpha = 0.5,
    cumulative_colour = "orange",
    cumulative_alpha = 0.8,
    cumulative_linewidth = 1.2,
    projected_colour = "black",
    projected_alpha = 1,
    projected_linewidth = 1,
    projected_linetype = "dashed",
    xlab = "Time",
    title = "",
    date_text_angle = -55,
    date_text_vjust = 0.5,
    date_text_hjust = 0,
    date_text_size = 9
  ) {

    time <- rlang::ensym(time)
    recruited <- rlang::ensym(recruited)
    projected <- rlang::ensym(projected)

    time_name <- rlang::as_name(time)
    recruited_name <- rlang::as_name(recruited)
    projected_name <- rlang::as_name(projected)

    stopifnot(
      is.data.frame(data),
      is.factor(data[[time_name]]) || is.character(data[[time_name]]),
      time_name %in% names(data),
      recruited_name %in% names(data),
      projected_name %in% names(data),
      is.numeric(data[[recruited_name]]),
      is.numeric(data[[projected_name]]),
      is.numeric(time_breaks), length(time_breaks) == 1,
      time_breaks == floor(time_breaks),
      is.numeric(y_scaling), length(y_scaling) == 1,
      is.character(recruited_label), length(recruited_label) == 1,
      is.character(cumulative_label), length(cumulative_label) == 1,
      is.character(projected_label), length(projected_label) == 1,
      is.character(recruited_fill), length(recruited_fill) == 1,
      is.numeric(recruited_alpha), length(recruited_alpha) == 1,
      recruited_alpha >= 0, recruited_alpha <= 1,
      is.character(cumulative_colour), length(cumulative_colour) == 1,
      is.numeric(cumulative_alpha), length(cumulative_alpha) == 1,
      cumulative_alpha >= 0, cumulative_alpha <= 1,
      is.numeric(cumulative_linewidth), length(cumulative_linewidth) == 1,
      is.character(projected_colour), length(projected_colour) == 1,
      is.numeric(projected_alpha), length(projected_alpha) == 1,
      projected_alpha >= 0, projected_alpha <= 1,
      is.numeric(projected_linewidth), length(projected_linewidth) == 1,
      is.character(projected_linetype), length(projected_linetype) == 1,
      is.character(xlab), length(xlab) == 1,
      is.character(title), length(title) == 1,
      is.numeric(date_text_angle), length(date_text_angle) == 1,
      is.numeric(date_text_vjust), length(date_text_vjust) == 1,
      is.numeric(date_text_hjust), length(date_text_hjust) == 1,
      is.numeric(date_text_size), length(date_text_size) == 1
    )

    data <-
      data |>
      dplyr::mutate(
        cumulative = cumsum(recruited)
      )

    scale <- max(data$recruited, na.rm = TRUE)*y_scaling / max(data$cumulative, na.rm = TRUE)

    ggplot2::ggplot(data, ggplot2::aes(x = time) ) +
      ggplot2::geom_col(
        ggplot2::aes(
          y = recruited,
          fill = recruited_label
        ),
        alpha = recruited_alpha
      ) +
      ggplot2::geom_line(
        ggplot2::aes(
          y = cumulative * scale,
          colour = cumulative_label
        ),
        linewidth = cumulative_linewidth,
        alpha = cumulative_alpha,
        group = 1
      ) +
      ggplot2::geom_line(
        ggplot2::aes(
          y = projected * scale,
          linetype = projected_label
        ),
        linewidth = projected_linewidth,
        group = 1,
        colour = projected_colour,
        alpha = projected_alpha
      ) +
      ggplot2::scale_y_continuous(
        name = recruited_label,
        sec.axis = ggplot2::sec_axis(~ . / scale, name = cumulative_label)
      ) +
      ggplot2::scale_x_discrete(
        breaks = data[[time_name]][seq(1, nrow(data), by = time_breaks)]
      ) +
      ggplot2::scale_fill_manual(
        name = "",
        values = setNames(recruited_fill, recruited_label)
        ) +
      ggplot2::scale_color_manual(
        name = "",
        values = setNames(cumulative_colour, cumulative_label)
        ) +
      ggplot2::scale_linetype_manual(
        name = "",
        values = setNames(projected_linetype, projected_label)
        ) +
      ggplot2::labs(
        x = xlab,
        title = title
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(order = 1),
        color = ggplot2::guide_legend(order = 2),
        linetype = ggplot2::guide_legend(order = 3)
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          angle = date_text_angle,
          vjust = date_text_vjust,
          hjust = date_text_hjust,
          size = date_text_size
          )
        )

  }

