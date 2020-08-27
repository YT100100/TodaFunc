strsplit.select <- function(x, split, index, from.tail = FALSE) {

  x.split <- strsplit(x, split)
  if (from.tail) {
    index.x <- sapply(x.split, length) - index + 1
    index.x <- ifelse(index.x < 1, NA, index.x)
    mapply(function(x0, i0) {
      if (is.na(i0)) return(NA)
      x0[i0]
    }, x.split, index.x)
  } else {
    index.x <- index
    sapply(x.split, function(x0) x0[index.x])
  }

}
