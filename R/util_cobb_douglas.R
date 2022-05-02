#' @export
util_cobb_douglas <- function() {
  f <- function(efficiency, weights, amounts) {
    efficiency * prod(amounts ^ weights)
  }

  new_util_homothetic(f,
                      efficiency = NA_real_,
                      weights = double(),
                      class = "util_cobb_douglas")
}

#' @export
util_calibrate.util_cobb_douglas <- function(x, prices, amounts, ...) {
  weights <- prices * amounts
  weights <- weights / sum(weights)

  x$weights <- weights
  x$efficiency <- sum(prices * amounts) / prod(amounts ^ weights)
  x
}

#' @export
util_demand_marshall.util_cobb_douglas <- function(x, prices, income, ...) {
  weights <- x$weights
  income * weights / sum(weights) / prices
}

#' @export
obj_sum.util_cobb_douglas <- function(x) {
  "Cobb-Douglas"
}
