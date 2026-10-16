#' Stacked bar plots showing counts in ordinal categories
#'
#' Produce a stacked bar plot showing counts which are split into ordinal categories
#' (e.g. severity of adverse events), using a colour gradient to depict the
#' ordinal scale.
#'
#' @param data Input data. Must have variables concerning: categories (for the
#' bars), ordinal categories (for the bar colours), and counts. Can optionally
#' include a variable to facet the plot (e.g. treatment group).
#' @param category name of the category variable
#' @param ordinal name of the ordinal category variable
#' @param count name of the count variable
#' @param group name of the faceting variable
#' @param sort choose if and how to sort the categories
#' @param other_to_last put "other" to last
#' @param other_name identify what "other" is called
#' @param palette ordinal colour palette
#' @param na_colour colour for missing ordinal value
#' @param order_labels names of ordinal category values for legend
#' @param order_name name of ordinal category for legend
#' @param xlab x-axis label
#' @param ylab y-axis label
#'
#' @return A ggplot2 object which displays counts and ordinal categories as
#' coloured bars.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' data <-
#'   data.frame(
#'     category = rep(c(rep("Burn", 5), rep("Nausea", 5), rep("Headache", 5),
#'                 rep("Pain", 5), rep("Other", 5)), 2),
#'     ordinal = as.factor(rep(c(1,2,3,4,5), 10)),
#'     count = c(rpois(25, 1), rpois(25, 1.5)),
#'     group = c(rep("Control", 25), rep("Intervention", 25))
#'     )
#'
#' ordinal_bars(data[data$group=="Control", ], category, ordinal, count)
#' ordinal_bars(data, category, ordinal, count, group, sort = "descending")
#'
#' set.seed(2)
#' data <-
#'   data.frame(
#'     category = rep(c(rep("Wispa", 5), rep("Galaxy", 5), rep("Fruit and nut", 5),
#'                 rep("Twix", 5), rep("Curly wurly", 5)), 2),
#'     ordinal = as.factor(rep(c(1,2,3,4,5), 10)),
#'     count = rpois(50, 3),
#'     group = c(rep("Regularly shop at Asda", 25),
#'       rep("Don't regularly shop at Asda", 25))
#'     )
#' ordinal_bars(
#'   data,
#'   category,
#'   ordinal,
#'   count,
#'   group,
#'   sort = "ascending",
#'   palette = "YlOrBr",
#'   order_labels = c("Horrible", "Not the best", "Fine", "Pretty tasty", "Delectable"),
#'   order_name = "Customer rating",
#'   ylab = ""
#'   )

ordinal_bars <-
  function(
    data,
    category = NULL,
    ordinal = NULL,
    count = NULL,
    group = NULL,
    sort = c("none", "alphabetical", "reverse_alphabetical", "ascending", "descending"),
    other_to_last = TRUE,
    other_name = "Other",
    palette = "YlOrRd",
    na_colour = "grey",
    order_labels = NULL,
    order_name = "Severity",
    xlab = "",
    ylab = "Number of events"
  ) {

    category <- rlang::ensym(category)
    ordinal <- rlang::ensym(ordinal)
    count <- rlang::ensym(count)

    category_name <- rlang::as_name(category)
    ordinal_name <- rlang::as_name(ordinal)
    count_name <- rlang::as_name(count)

    stopifnot(
      is.data.frame(data),
      category_name %in% names(data),
      ordinal_name %in% names(data),
      count_name %in% names(data),
      is.numeric(data[[count_name]]),
      is.factor(data[[ordinal_name]]),
      is.logical(other_to_last), length(other_to_last) == 1,
      is.character(other_name), length(other_name) == 1,
      is.character(palette), length(palette) == 1,
      is.character(na_colour), length(na_colour) == 1,
      is.null(order_labels) || is.character(order_labels),
      is.character(order_name), length(order_name) == 1,
      is.character(xlab), length(xlab) == 1,
      is.character(ylab), length(ylab) == 1
    )

    if (missing(order_labels)) {order_labels <- as.character(c(1:length(unique(data$ordinal)), "Missing"))}

    sort <-
      match.arg(
        sort,
        c("none", "alphabetical", "reverse_alphabetical", "ascending", "descending")
      )

    if (sort == "none") {
      levels <- unique(data[[category_name]])
    } else if (sort == "alphabetical") {
      levels <- sort(unique(as.character(data[[category_name]])))
    } else if (sort == "reverse_alphabetical") {
      levels <- rev(sort(unique(as.character(data[[category_name]]))))
    } else if (sort == "ascending") {
      levels <-
        data |>
        dplyr::group_by(.data[[category_name]]) |>
        dplyr::summarise(
          total = sum(.data[[count_name]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(total) |>
        dplyr::pull(1)
    } else if (sort == "descending") {
      levels <-
        data |>
        dplyr::group_by(.data[[category_name]]) |>
        dplyr::summarise(
          total = sum(.data[[count_name]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(total)) |>
        dplyr::pull(1)
    }

    if (other_to_last && other_name %in% levels) {
      levels <- c(setdiff(levels, other_name), other_name)
    }

    levels <- rev(levels)

    data[[category_name]] <- factor(data[[category_name]], levels = levels)

    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = category, y = count, fill = ordinal)) +
      ggplot2::geom_col(colour = "black", width = 0.75, position = "stack") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_brewer(
        name = order_name,
        palette = palette,
        na.value = na_colour,
        labels = order_labels
      ) +
      ggplot2::labs(
        x = xlab,
        y = ylab
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    if (!missing(group)) {
      group <- rlang::ensym(group)

      plot <-
        plot +
        ggplot2::facet_wrap(ggplot2::vars(!!group))
    }

    plot

  }


