#' Calculate Cronbach's alpha for sets of psychometric scale items
#'
#' This function calculates the Cronbach alpha for one
#' or more sets of psychometric scale items. Each item
#' is a variable in a data frame. Each set of items is 
#' defined by a tidy selection of a set of items.
#' 
#' 
#' @param .data A data frame with columns that are psychometric items. It is assumed that there will be rela
#' @param ... A set of comma separated tidy selec
#'
#' @return A vector of Cronbach alpha scores, with one score per each set of items.
#' @export
#'
#' @examples
#'  cronbach(test_psychometrics, 
#'           x = starts_with('x'), 
#'           y = y_1:y_10) 
#' 
cronbach <- function(.data, ...){
  
  raw_alpha <- function(items) {psych::alpha(items)$total$raw_alpha}
  
  selection_sets <- rlang::enquos(...)
  
  purrr::map(selection_sets, 
             function(selection_set){
               raw_alpha(select(.data, !!selection_set))
             }
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
#' total_scores(test_psychometrics, x = starts_with('x'), y = starts_with('y')
#' # Calculate the sum of all items beginning with `z_` and separately all items beginning with `x_`
#' total_scores(test_psychometrics, .method = 'sum', z = starts_with('z'), x = starts_with('x_')
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

