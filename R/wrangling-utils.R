#' Remove an additional header row from a data frame

#' @description Remove the first row of a data frame assuming that row was
#'   essentially a second (and redundant) header row in the original raw data
#'   file. After that row is removed, the data frame is reparsed to
#'   reinfer the data-types of each column.
#' 
#' @details Some software, including [Qualtrics](https://www.qualtrics.com)
#'   (survey software) and [Gorilla](https://gorilla.sc/) (behavioural
#'   experiment software), sometimes export their data where the first two rows
#'   are both essentially headers, i.e., column labels. These two rows are not
#'   identical and often the second is redundant and so needs to be skipped.
#'   Data import functions like \link[readr]{read_csv}, and many others, do not
#'   let you skip the second row if the first row is not skipped. On the other
#'   hand, it is easy to read in all the data as per usual and then use, for
#'   example, \link[dplyr]{slice}, to remove the second row in the original. For
#'   example, `slice(data_df, -1)` will remove the first row in the data frame
#'   named `data_df`, which would be the second row of the original data file
#'   (assuming, as is common, that the first row of the original was used as the
#'   header to create the column names).
#' 
#'   Although removing one row is easy to accomplish using basic tools in R, the
#'   bigger problem is that when the data was originally imported, it probably
#'   parsed all columns as character vectors. This is because the presence of
#'   header information in the second row of the original data, which are
#'   usually parsed as strings, forced the parser in a function like
#'   \link[readr]{read_csv} to parse the whole column as a character vector.
#'   After that second header row is removed, all the columns still remain as
#'   character vectors even though they could be, numeric, logical, etc. It is
#'   possible to use, for example, \link[dplyr]{mutate} and \link[dplyr]{across}
#'   to recode these columns, but that is not always possible with one simple
#'   command.
#'   
#'   An alternative approach is, after the header row is removed, to reparse all
#'   the columns to infer their data types and then automatically recode them.
#'   This is what is done in this function. The parser that is used is the one
#'   used by \link[=readr]{readr}.
#'   
#'   Note that this reparsing is no more, or no less, foolproof than what
#'   happens when we ever use, for example, \link[readr]{read_csv} to import
#'   data without specifying explicitly the data type for each column, which is
#'   commonly done. Given this, it is wise to check the new data types to make
#'   sure that there are no errors.
#'   
#'
#' @param data_df A data frame where it is assumed that the first row
#'   provides redundant header information and so it needs to be removed.
#'
#' @return A new data frame where the data types of all columns were re-inferred after the first row was removed.
#' @export
#'
#' @examples
#' double_headered_csv <- '
#' a,b,c
#' x,x,x
#' 1,2024/12/27,TRUE
#' 2,2024/12/17,TRUE
#' 3,2024/12/27,FALSE
#' '
#' readr::read_csv(double_headered_csv) |>
#'   remove_double_header()
remove_double_header <- function(data_df){
  dplyr::slice(data_df, -1) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), readr::parse_guess))
}


#' Rename selected columns as a sequence
#'
#' @description This function will rename a selection of columns as, for
#' example, `var_1`, `var_2`, `var_2` ... `var_10`, where the prefix, `var` in
#' this example, is arbitrary.
#'
#' @details If we had, for example, a data frame where columns were the names of
#'   drugs and we wanted to rename these columns something like `drug_1`,
#'   `drug_2`, ..., this would be easy to do with \link[dplyr]{rename} if there
#'   were just a few columns to rename. When there are more than just a few,
#'   individual renaming is somewhat tedious and error prone. We can use
#'   \link[dplyr]{rename_with} to do this in one operation. However, the code
#'   for doing so is not very simple and would require some proficiency in R and
#'   `tidyverse`. This function is essentially just a wrapper to a `rename_with`
#'   function to allow the renaming to be done in one simple command.
#' 
#' @param data_df A data frame
#' @param col_selector A tidy selector, e.g. `contains('foo')`,
#'   `ends_with('bar')`.
#' @param prefix The prefix for the sequence, e.g. 'drug' to produce names like
#'   `drug_1`, `drug_2` etc.
#'
#' @return A data frame with renamed columns
#' @export
#'
#' @examples
#' data_df <- readr::read_csv('
#' subject, age, gender, Aripiprazole, Clozapine, Olanzapine, Quetiapine
#' A, 27, F, 20, 10, 40, 25
#' B, 23, M, 21, 21, 35, 27
#' ')
#'
#' rename_with_seq(data_df, col_selector = Aripiprazole:Quetiapine, prefix = 'drug')
rename_with_seq <- function(data_df, col_selector, prefix = 'var'){
  selection_set <- rlang::enquo(col_selector)
  # count the number of cols selected by the selector
  k <- ncol(dplyr::select(data_df, !!selection_set))
  dplyr::rename_with(data_df, 
                     .fn = ~stringr::str_c(prefix, seq(k), sep = '_'), 
                     .cols = !!selection_set)
}




#' Drop rows if all values on selected columns are missing
#'
#' @description
#' Remove a row if all values on selected columns, or by default, on all
#' columns, are missing, i.e. have values of NA or NaN.
#'
#' @details
#' The \link[tidyr]{drop_na} function will remove any row if it has any NA in selected columns.
#' By default, it will remove the row there is any NA or NaN in any column.
#' This `drop_if_all_na` function is similar but removes the row only if all values in the selected columns are NA or NaN.
#' As with \link[tidyr]{drop_na}, by default it will use all columns.
#' In other words, by default, `drop_if_all_na` removes any row if all values on that row are NA or NaN.
#' 
#' @param data A data frame
#' @param ...  <[`tidy-select`][tidyr_tidy_select]> Columns to inspect for missing values. If empty, all columns are used.
#'
#' @return A data frame, possibly with some rows dropped.
#' @export
#' @examples
#' data_df <- data.frame(x = c(1, 2, NA, NA), y = c(2, NA, 5, NA))
#' 
#' drop_if_all_na(data_df)
#' drop_if_all_na(data_df, x)
#' drop_if_all_na(data_df, y)
#' drop_if_all_na(data_df, x, y)
#' drop_if_all_na(data_df, x:y)
#' drop_if_all_na(data_df, starts_with('x'), ends_with('y'))
#' 
drop_if_all_na <- function(data, ...) {
  dots <- enquos(...)
  not_na <- function(x) !is.na(x)
  
  if (rlang::is_empty(dots)) {
    # Use all columns if no `...` are supplied
    dplyr::filter(data, dplyr::if_any(.cols = everything(), not_na))
  } else {
    dplyr::filter(data, dplyr::if_any(.cols = c(!!!dots), not_na))
  }
  
}
