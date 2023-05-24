#' @export
util_linear <- function() {
  f <- function(efficiency, weights, quantities) {
    efficiency * sum(weights * quantities)
  }

  new_util_homothetic(f,
                      efficiency = NA_real_,
                      weights = double(),
                      class = "util_linear")
}

#' @export
util_calibrate.util_linear <- function(x, prices, quantities, ...) {
  x$weights <- prices / sum(prices)
  x$efficiency <- sum(prices * quantities) / sum(x$weights * quantities)
  x
}

#' @export
util_demand_marshall.util_linear <- function(x, prices, income, ...) {
  out <- income / prices
  out[-which.max(x$weights / prices)] <- 0
  out
}

#' @export
type_sum.util_linear <- function(x, ...) {
  "Linear"
}

#' @export
obj_sum.util_linear <- function(x, ...) {
  type_sum(x)
}
