
#' squishy (class)
#'
#' A class for squished data
#'
#' @param x Input data object
#' @return Object of class squishy
#' @export
squishy <- function(x) {
  tfse::set_class(x, "squishy")
  x
}


#' squish
#'
#' Method for squishing data
#'
#' @param x Input data object to be squished.
#' @return Squished x. Mostly used for printing
#' @export
squish <- function(x) UseMethod("squish")

nnn <- function(x) nchar(x) > 11L & !is.na(x)

is_int <- function(x) {
  if (is.character(x)) {
    x <- suppressWarnings(as.integer(x))
  }
  length(x) > 0L && is.integer(x) && !all(is.na(x))
}

is_dbl <- function(x) {
  if (is.character(x)) {
    x <- suppressWarnings(as.double(x))
  }
  length(x) > 0L && is.double(x) && !all(is.na(x))
}

squish.character <- function(x) {
  if (is_int(x)) {
    return(squish(as.integer(x)))
  }
  if (is_dbl(x)) {
    return(squish(as.double(x)))
  }
  x <- tfse::enc2ascii(x)
  x <- tfse::trim_ws(x)
  x[nnn(x)] <- paste0(
    tfse::trim_ws(substr(x[nnn(x)], 1, 8)),
    "..",
    tfse::trim_ws(substr(x[nnn(x)], nchar(x[nnn(x)]) - 1L, nchar(x[nnn(x)])))
  )
  x
}

format_dttm <- function(x, format) strptime(format(x, format = format), format = format)

squish.POSIXct <- function(x) as.Date(format_dttm(x, "%Y-%d-%m"))

squish.numeric <- function(x) {
  op <- options()
  options(scipen = -3, digits = 3)
  on.exit(options(op))
  x
}

squish.double <- function(x) {
  op <- options()
  options(scipen = -3, digits = 3)
  on.exit(options(op))
  x
}

squish.integer <- function(x) {
  op <- options()
  options(scipen = -3, digits = 3)
  on.exit(options(op))
  x
}

squish.logical <- function(x) {
  c("F", "T")[as.integer(x) + 1L]
}

squish.list <- function(x) {
  lapply(x, squish)
}

squish.data.frame <- function(x) {
  op <- options()
  options(scipen = -3, digits = 3)
  on.exit(options(op))
  names(x) <- squish(names(x))
  x[, 1:ncol(x)] <- lapply(x, squish)
  x
}
