#' Calculate Cronbach's alpha for sets of psychometric scale items
#'
#' This function calculates the Cronbach alpha for one or more sets of
#' psychometric scale items. Each item is a variable in a data frame. Each set
#' of items is defined by a tidy selection of a set of items.
#'
#'
#' @param .data A data frame with columns that are psychometric items.
#' @param ... A set of comma separated tidy selectors that selects sets of
#'   columns from `.data`. For each set of columns, the Cronbach's alpha is
#'   computed.
#' @param .ci The value of the confidence interval to calculate.
#'
#' @return A data frame whose rows are psychometric scales and for each scale,
#'   we have the Cronbach's alpha, and the lower and upper bound of the
#'   confidence interval on alpha.
#' @export
#'
#' @examples
#'  # Return the Cronbach alpha and 95% ci for two scales.
#'  # The first scale, named `x`, is identified by all items beginning with `x_`.
#'  # The second scale, named `y`, is identified by the consecutive items from `y_1` to `y_10`.
#'  cronbach(test_psychometrics,
#'           x = starts_with('x'),
#'           y = y_1:y_10)
#' 
cronbach <- function(.data, ..., .ci = 0.95){
  
  raw_alpha <- function(items) {
    total <- psych::alpha(items)$total
    
    z <- qnorm(.ci + (1 - .ci)/2)
    
    c(alpha = total$raw_alpha,
      ci_lo = total$raw_alpha - z * total$ase,
      ci_hi = total$raw_alpha + z * total$ase)
    
  }
  
  selection_sets <- rlang::enquos(...)
  
  purrr::map_dfr(selection_sets, 
                 function(selection_set){
                   raw_alpha(select(.data, !!selection_set))
                 },
                 .id = 'scale'
  )
  
}


#' Calculate the total scores from sets of scores
#'
#' @param .data
#' @param ... A comma separated set of named tidy selectors, each of which selects a set of columns to which to apply the totalling function.
#' @param .method The method used to calculate the total. Must be one of "mean",
#'   "sum", or "sum_like". The "mean" is the arithmetic mean, skipping missing
#'   values. The "sum" is the sum, skipping missing values. The "sum_like" is
#'   the arithmetic mean, again skipping missing values, multiplied by the number of elements, including missing values.
#' @param .append logical If FALSE, just the totals be returned. If TRUE, the totals are appended as new columns to original data frame.
#'
#' @return
#' @export
#'
#' @examples
#' # Calculate the mean of all items beginning with `x_` and separately all items beginning with `y_`
#' total_scores(test_psychometrics, x = starts_with('x'), y = starts_with('y'))
#' # Calculate the sum of all items beginning with `z_` and separately all items beginning with `x_`
#' total_scores(test_psychometrics, .method = 'sum', z = starts_with('z'), x = starts_with('x_'))
#' # Calculate the mean of all items from `x_1` to `y_10`
#' total_scores(test_psychometrics, xy = x_1:y_10)
total_scores <- function(.data, ..., .method = 'mean', .append = FALSE){
  
  totalling_function <- function(.data_selection, .method){
    switch(.method,
           mean = rowMeans(.data_selection, na.rm = T),
           sum = rowSums(.data_selection, na.rm = T),
           sum_like = rowMeans(.data_selection, na.rm = T) * ncol(.data_selection)
    )
  }
  
  if (!(.method %in% c('mean', 'sum', 'sum_like'))) {
    stop('The function that calculates the total must be either "mean", "sum" or "sum_like".')
  }
  
  selection_sets <- rlang::enquos(...)
  
  results_df <- purrr::map_dfc(selection_sets, 
                               function(selection_set){
                                 totalling_function(select(.data, !!selection_set),
                                                    .method)
                               }
  )
  
  if (.append) {
    bind_cols(.data, results_df)
  } else {
    results_df
  }
}

#' Recode specified values by new values
#'
#' @param x A vector, including column of data frame
#' @param from The set of old values to be replaced by new ones
#' @param to The set of new values to replace the old ones
#'
#' @return A vector that is the original vector but with old values replaced by new ones
#' @export
#'
#' @examples
#' # Replace any occurrence of 1 and 2 with 101 and 201, respectively
#' x <- c(1, 2, 3, 4, 5, 1, 2)
#' re_code(x, from = c(1, 2), to = c(101, 201))
re_code <- function(x, from, to){                                         
  plyr::mapvalues(x,                                                      
                  from = from,                                            
                  to = to,                                                
                  warn_missing = F)                                       
}