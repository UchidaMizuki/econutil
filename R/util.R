#' @export
new_util <- function(x, ...,
                     class = character()) {
  structure(x, ...,
            class = c(class, "util"))
}

#' @export
is_util <- function(x) {
  inherits(x, "util")
}

#' @export
util_calibrate <- function(x, prices, amounts, ...) {
  UseMethod("util_calibrate")
}

#' @export
util_output <- function(x, amounts, ...) {
  UseMethod("util_output")
}

#' @export
util_maximize <- function(x, prices, ...) {
  UseMethod("util_maximize")
}
