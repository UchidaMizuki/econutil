#' @export
util_leontief <- function() {
  f <- function(efficiency, weights, quantities) {
    efficiency * min(quantities / weights)
  }

  new_util_homothetic(f,
                      efficiency = NA_real_,
                      weights = double(),
                      class = "util_leontief")
}

#' @export
util_calibrate.util_leontief <- function(x, prices, quantities, ...) {
  x$weights <- quantities / sum(quantities)
  x$efficiency <- sum(prices * quantities) / min(quantities / x$weights)
  x
}

#' @export
util_demand_marshall.util_leontief <- function(x, prices, income, ...) {
  weights <- x$weights
  income * weights / sum(prices * weights)
}

#' @export
type_sum.util_leontief <- function(x, ...) {
  "Leontief"
}

#' @export
obj_sum.util_leontief <- function(x, ...) {
  paste0(type_sum(x), ": ", big_mark(x$efficiency))
}
