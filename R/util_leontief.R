#' @export
util_leontief <- function(efficiency = NA_real_,
                          weights = double()) {
  new_util_homothetic(substitution = NA_real_,
                      efficiency = efficiency,
                      weights = weights,
                      class = "util_leontief")
}

#' @export
vec_ptype_abbr.util_leontief <- function(x, ...) {
  "Leontief"
}
