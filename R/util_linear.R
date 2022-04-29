#' @export
util_linear <- function(efficiency = NA_real_,
                        weights = double()) {
  new_util_homothetic(substitution = NA_real_,
                      efficiency = efficiency,
                      weights = weights,
                      class = "util_linear")
}

#' @export
vec_ptype_abbr.util_linear <- function(x, ...) {
  "Linear"
}
