#' @export
util_linear <- function() {
  f <- function(efficiency, weights, amounts) {
    efficiency * sum(weights * amounts)
  }

  new_util_homothetic(f,
                      efficiency = NA_real_,
                      weights = double(),
                      class = "util_linear")
}

#' @export
util_calibrate.util_linear <- function(x, prices, amounts, ...) {
  x$weights <- prices / sum(prices)
  x$efficiency <- sum(prices * amounts) / sum(x$weights * amounts)
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
