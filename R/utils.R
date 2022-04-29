cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}

big_mark <- function (x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

print_list <- function(x, ...) {
  nms <- names(x)
  nms <- ifelse(nms == "",
                "",
                paste0(nms, ": "))
  width <- max(pillar::get_extent(nms))
  nms <- pillar::align(nms,
                       width = width)

  width_old <- getOption("width")

  options(width = pmax(0, width_old - width))

  out <- mapply(x, nms,
                FUN = function(x, nm) {
                  if (is_scalar_atomic(x) && !is_named(x)) {
                    out <- as.character(x)
                  } else {
                    out <- utils::capture.output(x)
                  }

                  names(out)[[1L]] <- nm
                  names(out)[-1L] <- strrep(" ", width)

                  out
                },
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE)
  out <- vec_c(!!!out)

  options(width = width_old)

  cat(paste0(names(out), out),
      sep = "\n")

  invisible(x)
}
