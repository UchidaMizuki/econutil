#' @export
util_leontief <- function() {
  f <- function(efficiency, weights, amounts) {
    efficiency * min(amounts / weights)
  }

  new_util_homothetic(f,
                      efficiency = NA_real_,
                      weights = double(),
                      class = "util_leontief")
}

#' @export
util_calibrate.util_leontief <- function(x, prices, amounts, ...) {
  x$weights <- amounts / sum(amounts)
  x$efficiency <- sum(prices * amounts) / min(amounts / x$weights)
  x
}

#' @export
util_demand_marshall.util_leontief <- function(x, prices, income, ...) {
  weights <- x$weights
  income * weights / sum(prices * weights)
}

#' @export
obj_sum.util_leontief <- function(x) {
  "Leontief"
}
