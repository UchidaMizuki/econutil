#' @export
util_cobb_douglas <- function(efficiency = NA_real_,
                              weights = double()) {
  new_util_homothetic(substitution = NA_real_,
                      efficiency = efficiency,
                      weights = weights,
                      class = "util_cobb_douglas")
}

#' @export
vec_ptype_abbr.util_cobb_douglas <- function(x, ...) {
  "Cobb-Douglas"
}
