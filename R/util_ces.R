#' @export
util_ces <- function(substitution) {
  f <- function(substitution, efficiency, weights, amounts) {
    efficiency * sum(weights * amounts ^ substitution) ^ (1 / substitution)
  }

  new_util(f,
           substitution = substitution,
           efficiency = NA_real_,
           weights = double(),
           class = "util_ces")
}

#' @export
util_calibrate.util_ces <- function(x, prices, amounts, ...) {
  substitution <- x$substitution

  weights <- prices * amounts ^ (1 - substitution)
  weights <- weights / sum(weights)

  x$weights <- weights
  x$efficiency <- sum(prices * amounts) / sum(weights * amounts ^ substitution) ^ (1 / substitution)
  x
}

#' @export
util_demand_marshall.util_ces <- function(x, prices, income, ...) {
  substitution <- x$substitution

  weights <- x$weights
  income *
    weights ^ (1 / (1 - substitution)) * prices ^ (1 / (substitution - 1)) /
    sum(weights ^ (1 / (1 - substitution)) * prices ^ (substitution / (substitution - 1)))
}

#' @export
obj_sum.util_ces <- function(x) {
  paste0("CES(", big_mark(x$substitution), ")")
}
