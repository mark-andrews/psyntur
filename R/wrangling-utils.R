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
    dplyr::mutate(dplyr::across(dplyr::everything(), readr::parse_guess))
}
