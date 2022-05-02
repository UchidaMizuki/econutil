#' @export
new_util_homothetic <- function(f,
                                substitution = NULL,
                                efficiency = NULL,
                                weights = NULL, ...,
                                class = character()) {
  args <- list(substitution = substitution,
               efficiency = efficiency,
               weights = weights)
  args <- purrr::discard(args, is.null)

  new_utility(f, args, ...,
              class = c(class, "util_homothetic"))
}

#' @export
is_util_homothetic <- function(x) {
  inherits(x, "util_homothetic")
}

#' @export
util_demand_hicks.util_homothetic <- function(x, prices, utility, ...) {
  amounts <- util_demand_marshall(x, prices, 1)
  amounts * utility / x(amounts)
}
