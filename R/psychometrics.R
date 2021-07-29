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

