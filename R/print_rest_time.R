#' Print rest processing time in for loop
#'
#' @param iter Number of processed iterations.
#' @param n_iter Number of all iterations.
#' @param start_time Starting time (`proc.time()`)
#' @param n_print An integer. How frequently should the message be shown?
#'
#' @export

print_rest_time <- function(iter, n_iter, start_time, n_print) {

  if (iter %% n_print == 0) {

    cat('   ', as.character(round(Sys.time())))

    i_str <- format(iter, width = ceiling(log10(n_iter + 1)))
    cat('   iter:', i_str, '/', n_iter)

    elapsed_time <- (proc.time() - start_time)[3]
    cat('   Elapsed:', convert_second_to_str(elapsed_time))

    pred_time <- elapsed_time / iter * (n_iter - iter)
    cat('   Rest:', convert_second_to_str(pred_time), '\n')

  }

}
