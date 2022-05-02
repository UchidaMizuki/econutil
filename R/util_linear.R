#' @export
util_linear <- function() {
  f <- function(weights, amounts) {
    sum(weights * amounts)
  }

  new_util_homothetic(f,
                      weights = double(),
                      class = "util_linear")
}

#' @export
util_calibrate.util_linear <- function(x, prices, amounts, ...) {
  x$weights <- prices
  x
}

#' @export
util_demand_marshall.util_linear <- function(x, prices, income, ...) {
  out <- income / prices
  out[-which.max(x$weights / prices)] <- 0
  out
}

#' @export
obj_sum.util_linear <- function(x) {
  "Linear"
}
