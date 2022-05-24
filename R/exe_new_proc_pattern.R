remove_dup_row <- function(base_df, remove_df) {

  if (is.null(remove_df)) return(base_df)

  is_duplicated <- apply(base_df, 1, function(pexe) {
    is_matched <- apply(remove_df, 1, function(prem) {
      all(pexe == prem)
    })
    return(any(is_matched))
  })
  return(base_df[!is_duplicated, , drop = FALSE])

}

create_output_directory <- function(outdir, func_name, input_name = NULL) {

  # abbreviate input file name
  if (!is.null(input_name)) {
    input_name <- basename(input_name)
    seploc <- regexpr('__', input_name)
    input_name_abb <- ifelse(
      seploc == -1, 'Default',
      substr(input_name, seploc + 2, nchar(input_name))
    )
  }

  # determine output directory names
  if (!is.null(input_name)) {
    outdir_now <- paste(input_name_abb, collapse = '__')
    outdir_now <- paste(outdir, func_name, outdir_now, sep = '__')
  } else {
    outdir_now <- paste(outdir, func_name, sep = '__')
  }

  # create directory
  dir.create(outdir_now, recursive = TRUE, showWarnings = FALSE)
  return(outdir_now)

}

convert_second_to_str <- function (totals) {

  rests <- totals

  second <- rests %% 60
  rests <- rests - second
  second <- round(second)

  minute <- rests %% 3600
  rests <- rests - minute
  minute <- minute / 60

  hour <- rests %% 86400
  rests <- rests - hour
  hour <- hour / 3600

  day <- rests / 86400

  ans <- ''
  ans <- paste0(ans, ifelse(day    == 0, '', paste0(day   , 'd ')))
  ans <- paste0(ans, ifelse(hour   == 0, '', paste0(hour  , 'h ')))
  ans <- paste0(ans, ifelse(minute == 0, '', paste0(minute, 'm ')))
  ans <- paste0(ans, second, 's')

  return(ans)

}

exe_new_proc_pattern <- function(
  input_path_vec = NULL,
  exe_func_list,
  outdir,
  exe_pattern = NULL,
  remove_pattern = NULL
) {

  #' Executing sequential processing patterns which have not done before
  #'
  #' @param input_path_vec Lists of functions.
  #' @param exe_func_list Lists of functions.
  #' @param outdir Lists of functions.
  #' @param remove_pattern A data frame indicating processing patterns
  #' which should not be executed.
  #'
  #' @export

  if (is.null(exe_pattern)) {
    
    # create execution pattern of functions
    exe_pattern <- data.frame(exe_func = names(exe_func_list))
    
    # create execution pattern of input paths
    for (path_name in names(input_path_vec)) {
      # i_path <- 1
      
      # fetch paths of input files
      paths_i <- list.dirs(input_path_vec[path_name], recursive = FALSE, full.names = TRUE)
      
      # extend data frame to add paths
      exe_pattern_now <- exe_pattern
      for (iter in 1:length(paths_i)) {
        if (iter == 1) next
        exe_pattern <- rbind(exe_pattern, exe_pattern_now)
      }
      
      # add a column of input file paths
      exe_pattern[[path_name]] <- rep(paths_i, each = nrow(exe_pattern_now))
      
    }
    
  }

  # remove patterns which should not be executed
  exe_pattern <- remove_dup_row(exe_pattern, remove_pattern)

  # remove existing patterns
  pattern_save_dir <- dirname(outdir)
  is_pattern_file <- grepl('^ExecutedPatterns\\.csv$', list.files(pattern_save_dir))
  if (any(is_pattern_file)) {
    exist_pattern_raw <- read.csv(paste0(pattern_save_dir, '/ExecutedPatterns.csv'), row.names = 1)
    exist_pattern <- exist_pattern_raw
    exist_pattern$folder <- NULL
    exe_pattern <- remove_dup_row(exe_pattern, exist_pattern)
  }

  # cat execution patterns
  n_pattern <- nrow(exe_pattern)
  cat('Detected', n_pattern, 'patterns.\n')
  if (n_pattern == 0) return()

  # execution
  exe_pattern_output <- exe_pattern
  exe_pattern_output$folder <- NA
  for (i_pat in 1:n_pattern) {
    # i_pat <- 1

    t1 <- proc.time()

    # function to execute
    exe_func_name_i <- exe_pattern$exe_func[[i_pat]]
    exe_func_i <- exe_func_list[[exe_func_name_i]]

    # file path to read
    if (is.null(input_path_vec)) {
      input_path_i <- NULL
    } else {
      input_path_i <- unlist(exe_pattern[i_pat, -1])
      input_path_i <- paste0(input_path_i, '/')
    }

    # create output directory
    outdir_i <- create_output_directory(outdir, exe_func_name_i, input_path_i)
    outdir_i <- paste0(outdir_i, '/')
    exe_pattern_output$folder[i_pat] <- outdir_i

    # execution
    cat('Pattern', i_pat, 'is running.\n')
    cat('  Processing starts:', as.character(Sys.time()), '\n')
    cat('  Function:', exe_func_name_i, '\n')
    if (is.null(input_path_vec)) {
      exe_func_i(outdir_i)
    } else {
      for (j in 1:length(input_path_i)) {
        cat('  ', names(input_path_vec)[j], ': ', input_path_i[j], '\n', sep = '')
      }
      exe_func_i(input_path_i, outdir_i)
    }

    # print time
    proct <- convert_second_to_str((proc.time() - t1)[3])
    cat('  Processing time: ', proct, '\n', sep = '')

  }

  # output execution patterns
  if (any(is_pattern_file)) {
    exe_pattern_output <- rbind(exist_pattern_raw, exe_pattern_output)
  }
  write.csv(exe_pattern_output, paste0(pattern_save_dir, '/ExecutedPatterns.csv'))
  cat('Finished.\n')
  return()

}
