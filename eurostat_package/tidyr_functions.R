#'
#' Given either regular expression or a vector of character positions,
#' \code{separate()} turns a single character column into multiple columns.
#'
#' @param col Bare column name.
#' @export
#' @inheritParams separate_
#' @examples
#' library(dplyr)
#' df <- data.frame(x = c("a.b", "a.d", "b.c"))
#' df %>% separate(x, c("A", "B"))
#'
#' # If every row doesn't split into the same number of pieces, use
#' # the extra argument to control what happens
#' df <- data.frame(x = c("a", "a b"))
#' df %>% separate(x, c("a", "b"), extra = "merge")
#' df %>% separate(x, c("a", "b"), extra = "drop")
#'
#' # If only want to split specified number of times use extra = "merge"
#' df <- data.frame(x = c("x: 123", "y: error: 7"))
#' df %>% separate(x, c("key", "value"), ": ", extra = "merge")
separate <- function(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE,
                     convert = FALSE, ...) {
  col <- col_name(substitute(col))
  separate_(data, col, into, sep = sep, remove = remove, convert = convert, ...)
}

#' Standard-evaluation version of \code{separate}.
#'
#' This is a S3 generic.
#'
#' @param data A data frame.
#' @param col Name of column to split, as string.
#' @param into Names of new variables to create as character vector.
#' @param sep Separator between columns.
#'
#'   If character, is interpreted as a regular expression. The default
#'   value is a regular expression that matches any sequence of
#'   non-alphanumeric values.
#'
#'   If numeric, interpreted as positions to split at. Positive values start
#'   at 1 at the far-left of the string; negative value start at -1 at the
#'   far-right of the string. The length of \code{sep} should be one less than
#'   \code{into}.
#'
#' @param extra If \code{sep} is a character vector, this controls what
#'   happens when the number of pieces doesn't match \code{into}. There are
#'   three valid options:
#'
#'   \itemize{
#'    \item "error" (the default): throws error if pieces aren't right length
#'    \item "drop": always returns \code{length(into)} pieces by dropping or
#'      expanding as necessary
#'    \item "merge": only splits at most \code{length(into)} times
#'   }
#' @param remove If \code{TRUE}, remove input column from output data frame.
#' @param convert If \code{TRUE}, will run \code{\link{type.convert}} with
#'   \code{as.is = TRUE} on new columns. This is useful if the component
#'   columns are integer, numeric or logical.
#' @param ... Other arguments passed on to \code{\link{strsplit}} to control
#'   how the regular expression is processed.
#' @keywords internal
#' @export
separate_ <- function(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE,
                      convert = FALSE, extra = "error", ...) {
  UseMethod("separate_")
}

#' @export
separate_.data.frame <- function(data, col, into, sep = "[^[:alnum:]]+",
                                 remove = TRUE, convert = FALSE, extra = "error",
                                 ...) {

  stopifnot(is.character(col), length(col) == 1)

  value <- as.character(data[[col]])

  if (is.numeric(sep)) {
    l <- strsep(value, sep)
  } else if (is.character(sep)) {
    l <- str_split_fixed(value, sep, length(into), extra = extra)
  } else {
    stop("'sep' must be either numeric or character", .call = FALSE)
  }

  names(l) <- into
  if (convert) {
    l[] <- lapply(l, type.convert, as.is = FALSE)
  }

  # Insert into existing data frame
  data <- append_df(data, l, which(names(data) == col))
  if (remove) {
    data[[col]] <- NULL
  }
  data
}

#' @export
separate_.tbl_df <- function(data, col, into, sep = "[^[:alnum:]]+",
                             remove = TRUE, convert = FALSE, ...) {
  dplyr::tbl_df(NextMethod())
}


strsep <- function(x, sep) {
  sep <- c(0, sep, -1)

  nchar <- nchar(x)
  pos <- lapply(sep, function(i) {
    if (i >= 0) return(i)
    nchar + i + 1
  })

  lapply(1:(length(pos) - 1), function(i) {
    substr(x, pos[[i]] + 1, pos[[i + 1]])
  })
}

str_split_fixed <- function(value, sep, n, extra = "error") {
  extra <- match.arg(extra, c("error", "merge", "drop"))

  n_max <- if (extra == "merge") n else -1L
  pieces <- stringi::stri_split_regex(value, sep, n_max)

  ns <- vapply(pieces, length, integer(1))

  if (any(ns != n)) {
    if (extra == "error") {
      stop("Values not split into ", n, " pieces at ",
        paste0(which(ns != n), collapse = ', '), call. = FALSE)
    } else {
      pieces <- lapply(pieces, function(x) x[seq_len(n)])
    }
  }

  # Convert into a list of columns
  mat <- matrix(unlist(pieces), ncol = n, byrow = TRUE)
  lapply(1:ncol(mat), function(i) mat[, i])
}


######################################


#' Gather columns into key-value pairs.
#'
#' Gather takes multiple columns and collapses into key-value pairs,
#' duplicating all other columns as needed. You use \code{gather()} when
#' you notice that you have columns that are not variables.
#'
#' @param data A data frame.
#' @param key,value Names of key and value columns to create in output.
#' @param ... Specification of columns to gather. Use bare variable names.
#'   Select all variables between x and z with \code{x:z}, exclude y with
#'   \code{-y}. For more options, see the \link[dplyr]{select} documentation.
#' @inheritParams gather_
#' @seealso \code{\link{gather_}} for a version that uses regular evaluation
#'   and is suitable for programming with.
#' @export
#' @examples
#' library(dplyr)
#' # From http://stackoverflow.com/questions/1181060
#' stocks <- data.frame(
#'   time = as.Date('2009-01-01') + 0:9,
#'   X = rnorm(10, 0, 1),
#'   Y = rnorm(10, 0, 2),
#'   Z = rnorm(10, 0, 4)
#' )
#'
#' gather(stocks, stock, price, -time)
#' stocks %>% gather(stock, price, -time)
gather <- function(data, key, value, ..., na.rm = FALSE, convert = FALSE) {
  key_col <- col_name(substitute(key))
  value_col <- col_name(substitute(value))
  gather_cols <- unname(dplyr::select_vars(names(data), ...))

  gather_(data, key_col, value_col, gather_cols, na.rm = na.rm,
    convert = convert)
}

#' Gather (standard-evaluation).
#'
#' This is a S3 generic.
#'
#' @param data A data frame
#' @param key_var,value_var Strings giving names of key and value columns to
#'   create.
#' @param gather_cols Character vector giving column names to be gathered into
#'   pair of key-value columns.
#' @param na.rm If \code{TRUE}, will remove rows from output where the
#'   value column in \code{NA}.
#' @param convert If \code{TRUE} will automatically run
#'   \code{\link{type.convert}} on the key column. This is useful if the column
#'   names are actually numeric, integer, or logical.
#' @keywords internal
#' @export
gather_ <- function(data, key_col, value_col, gather_cols, na.rm = FALSE,
                     convert = FALSE) {
  UseMethod("gather_")
}

#' @export
gather_.data.frame <- function(data, key_col, value_col, gather_cols,
                               na.rm = FALSE, convert = FALSE) {

  data2 <- reshape2::melt(data, measure.vars = gather_cols,
    variable.name = key_col, value.name = value_col, na.rm = na.rm)
  rownames(data2) <- NULL

  if (convert) {
    data2[[key_col]] <- type.convert(as.character(data2[[key_col]]),
      as.is = TRUE)
  }

  data2
}

#' @export
gather_.tbl_df <- function(data, key_col, value_col, gather_cols,
                           na.rm = FALSE, convert = FALSE) {
  dplyr::tbl_df(NextMethod())
}


########################################333


#' Extract one column into multiple columns.
#'
#' Given a regular expression with capturing groups, \code{extract()} turns
#' each group into a new column.
#'
#' @param col Bare column name.
#' @export
#' @inheritParams extract_
#' @examples
#' library(dplyr)
#' df <- data.frame(x = c("a.b", "a.d", "b.c"))
#' df %>% extract(x, "A")
#' df %>% extract(x, c("A", "B"), "([[:alnum:]]+)\\.([[:alnum:]]+)")
extract <- function(data, col, into, regex = "([[:alnum:]]+)", remove = TRUE,
                     convert = FALSE, ...) {
  col <- col_name(substitute(col))
  extract_(data, col, into, regex = regex, remove = remove, convert = convert, ...)
}

#' Standard-evaluation version of \code{extract}.
#'
#' This is a S3 generic.
#'
#' @param data A data frame.
#' @param col Name of column to split, as string.
#' @param into Names of new variables to create as character vector.
#' @param regex a regular expression used to extract the desired values.
#' @param remove If \code{TRUE}, remove input column from output data frame.
#' @param convert If \code{TRUE}, will run \code{\link{type.convert}} with
#'   \code{as.is = TRUE} on new columns. This is useful if the component
#'   columns are integer, numeric or logical.
#' @param ... Other arguments passed on to \code{\link{regexec}} to control
#'   how the regular expression is processed.
#' @keywords internal
#' @export
extract_ <- function(data, col, into, regex = "([[:alnum:]]+)", remove = TRUE,
                      convert = FALSE, ...) {
  UseMethod("extract_")
}

#' @export
extract_.data.frame <- function(data, col, into, regex = "([[:alnum:]]+)",
                                 remove = TRUE, convert = FALSE, ...) {

  stopifnot(is.character(col), length(col) == 1)
  stopifnot(is.character(regex))

  # Extract matching groups
  value <- as.character(data[[col]])
  matches <- regexec(regex, value, ...)
  pieces <- regmatches(value, matches)

  ns <- vapply(pieces, length, integer(1))
  if (any(ns == 0)) {
    stop("Regex didn't match at ",
      paste0(which(ns == 0), collapse = ", "),
      call. = FALSE)
  }
  n <- unique(ns) - 1
  stopifnot(length(n) == 1)

  if (length(into) != n) {
    stop("'into' must be the same length as the regex capture groups",
         call. = FALSE)
  }

  # Convert into data frame
  mat <- matrix(unlist(pieces), ncol = n + 1, byrow = TRUE)
  mat <- mat[, -1, drop = FALSE] # remove complete match
  l <- lapply(1:ncol(mat), function(i) mat[, i])
  names(l) <- into
  if (convert) {
    l[] <- lapply(l, type.convert, as.is = FALSE)
  }

  # Insert into existing data frame
  data <- append_df(data, l, which(names(data) == col))
  if (remove) {
    data[[col]] <- NULL
  }
  data
}

#' @export
extract_.tbl_df <- function(data, col, into, regex = "([[:alnum:]]+)",
                             remove = TRUE, convert = FALSE, ...) {
  dplyr::tbl_df(NextMethod())
}




col_name <- function(x) {
  if (is.character(x)) return(x)
  if (is.name(x)) return(as.character(x))
  if (is.null(x)) return(x)

  stop("Invalid column specification", call. = FALSE)
}

append_df <- function(x, values, after = length(x)) {
  y <- append(x, values, after = after)
  class(y) <- class(x)
  attr(y, "row.names") <- attr(x, "row.names")

  y
}

append_col <- function(x, col, name, after = length(x)) {
  append_df(x, setNames(list(col), name), after = after)
}

#' Extract numeric component of variable.
#'
#' This uses a regular expression to strip all non-numeric character from
#' a string and then coerces the result to a number. This is useful for
#' strings that are numbers with extra formatting (e.g. $1,200.34).
#'
#' @param x A character vector (or a factor).
#' @export
#' @examples
#' extract_numeric("$1,200.34")
#' extract_numeric("-2%")
#'
#' # The heuristic is not perfect - it won't fail for things that
#' # clearly aren't numbers
#' extract_numeric("-2-2")
#' extract_numeric("12abc34")
extract_numeric <- function(x) {
  as.numeric(gsub("[^0-9.-]+", "", as.character(x)))
}

"%||%" <- function(a, b) if (is.null(a)) b else a

