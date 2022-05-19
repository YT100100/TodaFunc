print_rest_time <- function(iter, start_time, n_all, n_print) {
  
  if (iter %% n_print == 0) {
    
    cat(as.character(Sys.time()))
    
    i_str <- format(iter, width = ceiling(log10(n_all + 1)))
    cat('    iter:', i_str, '/', n_all)
    
    elapsed_time <- (proc.time() - start_time)[3]
    cat('   Elapsed:', convert_second_to_str(elapsed_time))
    
    pred_time <- elapsed_time / iter * (n_all - iter)
    cat('   Rest:', convert_second_to_str(pred_time), '\n')
    
  }
  
}
