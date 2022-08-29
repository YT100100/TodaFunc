#' An expanded class of array
#'
#' @param data
#' @param dim
#' @param diminfo A list of information of the array.
#'
#' @importFrom R6 R6Class
#' @export

call_expanded_array <- function() {

  ExpandedArray <- R6Class(

    classname = 'ExpandedArray',
    public = list(

      array = NULL,
      dim = NULL,
      diminfo = NULL,

      update_dimnames = function() {

        dimnames(self$array) <- lapply(dim(self$array), function(d) 1:d)

      },

      initialize = function(data = NA, dim = length(data), diminfo) {

        stopifnot(identical(class(diminfo), 'list'))
        stopifnot(length(diminfo) == length(dim))
        for (i in 1:length(dim)) {
          diminfo_length_i <- if ('ExpandedArray' %in% class(diminfo[[i]])) {
            diminfo[[i]]$length()
          } else {
            length(diminfo[[i]])
          }
          stopifnot(diminfo_length_i == dim[i])
        }

        self$array <- array(data = data, dim = dim)
        self$dim <- dim
        self$diminfo <- diminfo
        self$update_dimnames()

      },

      length = function() return(length(self$array)),

      slice_diminfo = function(new_array) {

        old_diminfo <- self$diminfo
        new_diminfo <- list()
        for (i in 1:length(dim(new_array))) {

          index_i <- as.integer(dimnames(new_array)[[i]])
          if ('ExpandedArray' %in% class(old_diminfo[[i]])) {
            new_diminfo_i <- old_diminfo[[i]]$clone()
            new_diminfo_i$slice(index_i)
            new_diminfo[[i]] <- new_diminfo_i$clone()
          } else {
            new_diminfo[[i]] <- old_diminfo[[i]][index_i]
          }

        }
        return(new_diminfo)

      },

      print_slice = function(...) {

        new_array <- self$array[..., drop = FALSE]
        new_diminfo <- self$slice_diminfo(new_array)

        cat('array:\n')
        print(new_array)
        for (i in 1:length(new_diminfo)) {
          cat('\ndim ', i, ':\n', sep = '')
          if ('ExpandedArray' %in% class(new_diminfo[[i]])) {
            new_diminfo[[i]]$print()
          } else {
            print(new_diminfo[[i]])
          }
        }

      },

      print = function() {

        self$print_slice()

      },

      head = function(n = 6) {

        print_n <- lapply(self$dim, function(d) 1:pmin(d, n))
        if (length(self$dim) == 2) {
          print_n[[2]] <- 1:self$dim[2]
        }
        do.call(self$print_slice, print_n)

      },

      slice = function(...) {

        new_exparray <- self$clone()
        new_exparray$array   <- new_exparray$array[..., drop = FALSE]
        new_exparray$dim     <- dim(new_exparray$array)
        new_exparray$diminfo <- new_exparray$slice_diminfo(new_exparray$array)
        new_exparray$update_dimnames()
        return(new_exparray)

      },

      apply = function(margin, fun) {

        new_data    <- apply(self$array, margin, fun)
        new_dim     <- if (length(margin) == 1) length(new_data) else dim(new_data)
        new_diminfo <- self$diminfo[margin]
        return(ExpandedArray$new(new_data, new_dim, new_diminfo))

      }

    )
  )

  return(ExpandedArray)

}

# a <- ExpandedArray$new(1:40, c(8, 5), list(letters[1:8], LETTERS[1:5]))
# a$array
# a$dim
# a$diminfo
# class(a)
# a$print_slice(, 4:5)
# a$head()
#
# a$slice(, 3:4)
# a$array
# a$dim
# a$diminfo
#
# a <- ExpandedArray$new(dim = 3, diminfo = list(LETTERS[1:3]))
# a$array
# a$dim
# a$diminfo
# a$head()
# a$slice(2)
# a$array
# a$dim
# a$diminfo
#
# x <- ExpandedArray$new(
#   data = 1:24,
#   dim = 4:2,
#   diminfo = list(letters[1:4], LETTERS[1:3],
#                  ExpandedArray$new(letters[1:2], diminfo = list(paste0('a', 1:2)))))
# x$print()
# x$array
# x$dim
# x$diminfo
# x$diminfo[[3]]$print()
# y <- x$slice(, , 2)
# y$array
# y$dim
# y$diminfo
# y$diminfo[[3]]
#
# x$apply(1:2, sum)
# x$apply(c(1, 3), sum)
# x$apply(3, sum)

Trait <- R6Class(

  classname = 'Trait',
  inherit = Expand

)

Pmat <- R6Class(
  classname = 'Pmat',

)

