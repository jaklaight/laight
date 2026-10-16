#' Search for variable names
#'
#' Search for variable names in named environments or lists of data frames.
#'
#' @param x an environment or list of data frames
#' @param term a character vector of term(s) to search for
#'
#' @return a new data frame with three columns: data frame where the variable
#' was found, the name of the variable found, and the term used to find the
#' variable.
#'
#' @export
#'
#' @examples
#' my_list <- list(iris = iris, mtcars = mtcars)
#' variable_search(my_list, c("sepal", "T"))

variable_search <-
  function(x, term) {

    stopifnot(
      is.character(term), length(term) >= 1, !any(is.na(term)), !any(term == ""),
      !is.data.frame(x), is.environment(x) || is.list(x), length(x) >= 1
      )

    if (is.null(names(x))) {
      names(x) <- seq_along(x)
      message("Supplied list/environment of data frames not named; numbering the data frames.")
    }

    dataframes <- x[sapply(x, is.data.frame)]
    found <- list()
    for (i in names(dataframes)) {
      dataframe <- dataframes[[i]]
      variables <- names(dataframe)
      for (j in term) {
        matches <- grep(j, variables, value = TRUE, ignore.case = TRUE)
        if (length(matches) > 0) {
          found[[length(found) + 1]] <- data.frame(
            data_frame = i,
            variable = matches,
            matched_term = j,
            stringsAsFactors = FALSE
            )
          }
      }
    }
    if (length(found) > 0) {
      do.call(rbind, found)
    } else {
      data.frame(
        data_frame = character(),
        variable = character(),
        matched_term = character(),
        stringsAsFactors = FALSE
      )
    }

  }
