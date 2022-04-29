#' @export
new_util_homothetic <- function(substitution,
                                efficiency = NA_real_,
                                weights = double(), ...,
                                class = character()) {
  vec_assert(substitution, double(), 1L)
  vec_assert(efficiency, double(), 1L)
  vec_assert(weights, double())

  new_util(list(substitution = substitution,
                efficiency = efficiency,
                weights = weights), ...,
           class = c(class, "util_homothetic"))
}

#' @export
is_util_homothetic <- function(x) {
  inherits(x, "homothetic")
}

#' @export
print.util_homothetic <- function(x, ...) {
  cat_line("<", pillar::obj_sum(x), ">")

  out <- x
  if (is.na(out$substitution)) {
    out$substitution <- NULL
  }
  print_list(out)

  invisible(x)
}
