#' @export
util_ces <- function(substitution,
                     efficiency = NA_real_,
                     weights = double()) {
  new_util_homothetic(substitution = substitution,
                      efficiency = efficiency,
                      weights = weights,
                      class = "util_ces")
}

#' @export
vec_ptype_abbr.util_ces <- function(x, ...) {
  paste0("CES(", big_mark(x$substitution), ")")
}
