#' @export
new_utility <- function(f, args, ...,
                        class = character()) {
  partialised::new_partialised(f, args, ...,
                               class = c(class, "util"))
}

#' @export
is_utility <- function(x) {
  inherits(x, "util")
}

#' @export
util_calibrate <- function(x, prices, amounts, ...) {
  UseMethod("util_calibrate")
}

#' @export
util_demand <- function(x, prices,
                        income = NULL,
                        utility = NULL, ...) {
  if (!is.null(income)) {
    stopifnot(
      is.null(utility)
    )

    util_demand_marshall(x, prices, income, ...)
  } else {
    stopifnot(
      !is.null(utility)
    )

    util_demand_hicks(x, prices, utility, ...)
  }
}

#' @export
util_demand_marshall <- function(x, prices, income, ...) {
  UseMethod("util_demand_marshall")
}

#' @export
util_demand_hicks <- function(x, prices, utility, ...) {
  UseMethod("util_demand_hicks")
}

#' @export
util_expenditure <- function(x, prices, utility, ...) {
  sum(prices * util_demand_hicks(x, prices, utility))
}

#' @export
util_indirect <- function(x, prices, income, ...) {
  x(util_demand_marshall(x, prices, income))
}
