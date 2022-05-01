#' @export
util_leontief <- function() {
  f <- function(weights, amounts) {
    min(amounts / weights)
  }

  new_util(f,
           weights = double(),
           class = "util_leontief")
}

#' @export
util_calibrate.util_leontief <- function(x, prices, amounts, ...) {
  x$weights <- amounts / sum(prices * amounts)
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
