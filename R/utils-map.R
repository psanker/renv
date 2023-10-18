
bapply <- function(x, f, ..., index = "Index") {
  result <- lapply(x, f, ...)
  bind(result, index = index)
}

enumerate <- function(x, f, ..., FUN.VALUE = NULL) {

  n <- names(x)
  idx <- named(seq_along(x), n)
  callback <- function(i) f(n[[i]], x[[i]], ...)

  if (is.environment(x))
    x <- as.list(x, all.names = TRUE)

  if (is.null(FUN.VALUE))
    lapply(idx, callback)
  else
    vapply(idx, callback, FUN.VALUE = FUN.VALUE)

}

enum_chr <- function(x, f, ...) {
  enumerate(x, f, ..., FUN.VALUE = character(1))
}

enum_int <- function(x, f, ...) {
  enumerate(x, f, ..., FUN.VALUE = integer(1))
}

enum_dbl <- function(x, f, ...) {
  enumerate(x, f, ..., FUN.VALUE = double(1))
}

enum_lgl <- function(x, f, ...) {
  enumerate(x, f, ..., FUN.VALUE = logical(1))
}


uapply <- function(x, f, ...) {
  f <- match.fun(f)
  unlist(lapply(x, f, ...), recursive = FALSE)
}

filter <- function(x, f, ...) {
  f <- match.fun(f)
  x[map_lgl(x, f, ...)]
}

reject <- function(x, f, ...) {
  f <- match.fun(f)
  x[!map_lgl(x, f, ...)]
}

map <- function(x, f, ...) {
  f <- match.fun(f)
  lapply(x, f, ...)
}

map_chr <- function(x, f, ...) {
  f <- match.fun(f)
  vapply(x, f, ..., FUN.VALUE = character(1))
}

map_dbl <- function(x, f, ...) {
  f <- match.fun(f)
  vapply(x, f, ..., FUN.VALUE = numeric(1))
}

map_int <- function(x, f, ...) {
  f <- match.fun(f)
  vapply(x, f, ..., FUN.VALUE = integer(1))
}

map_lgl <- function(x, f, ...) {
  f <- match.fun(f)
  vapply(x, f, ..., FUN.VALUE = logical(1))
}

map2 <- function(x, y, f, ...) {
  f <- match.fun(f)
  out <- mapply(f, x, y, MoreArgs = list(...), SIMPLIFY = FALSE)

  if (length(out) == length(x)) {
    names(out) <- names(x)
  } else {
    names(out) <- NULL
  }

  out
}

map2_lgl <- function(x, y, f, ...) {
  as.vector(map2(x, y, f, ...), "logical")
}

map2_int <- function(x, y, f, ...) {
  as.vector(map2(x, y, f, ...), "integer")
}

map2_dbl <- function(x, y, f, ...) {
  as.vector(map2(x, y, f, ...), "double")
}

map2_chr <- function(x, y, f, ...) {
  as.vector(map2(x, y, f, ...), "character")
}

extract <- function(x, ...) {
  lapply(x, `[[`, ...)
}

extract_chr <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = character(1))
}

extract_dbl <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = numeric(1))
}

extract_int <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = integer(1))
}

extract_lgl <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = logical(1))
}
