#' @export
new_util_homothetic <- function(f,
                                substitution = NULL,
                                efficiency = NA_real_,
                                weights = double(), ...,
                                class = character()) {
  args <- list(substitution = substitution,
               efficiency = efficiency,
               weights = weights)
  args <- purrr::compact(args)

  new_utility(f, args, ...,
              class = c(class, "util_homothetic"))
}

#' @export
is_util_homothetic <- function(x) {
  inherits(x, "util_homothetic")
}

#' @export
util_demand_hicks.util_homothetic <- function(x, prices, utility, ...) {
  quantities <- util_demand_marshall(x, prices, 1)
  quantities * utility / x(quantities)
}
