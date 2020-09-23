#' Calculate descriptive statistics 
#' 
#' This function is a lightweight wrapper to `dplyr`'s `summarize` function.
#' It can be used to calculate any descriptive or summary statistic for any
#' variable in the data set. Optionally, a `by` grouping variable can be used,
#' and then the summary statistics are calculated for each subgroup defined by 
#' the different values of the `by` variable.
#' 
#' @examples 
#' describe(faithfulfaces, avg = mean(faithful), stdev = sd(faithful))
#' describe(faithfulfaces, by = face_sex, avg = mean(faithful), stdev = sd(faithful))
#' 
#' @param data A data frame
#' @param by A grouping variable. If included, the `data` will be grouped by the values of the 
#' `by` variable before the summary statistics are applied.
#' @param ... Arguments of functions applied to variables, e.g. `avg = mean(x)`.
#' 
#' @return A tibble data frame.
#' 
#' @import dplyr
#' @export 
describe <- function(data, by = NULL, ...){
  if (is.null(enexpr(by))){
    summarise(data, ...)
  } else {
    summarise(group_by(data, across({{ by }})), ..., .groups = 'drop')
  }
  
}

#' Apply multiple descriptive functions to multiple variables
#' 
#' This function is a wrapper to `dplyr`'s `summarize` used with the
#' `across` function. For each variable in a set of variables, calculate each
#' summary statistic from a list of summary statistic functions. Optionally,
#' group the variables by a grouping variable, and then calculate the
#' statistics. Optionally, the tibble that is returned by default, which is in a
#' wide format, can be pivoted to a long format.
#' 
#' @param data A data frame
#' @param variables A vector of variables in `data`
#' @param functions A list of summary statistic function. If it is named list, which
#' is recommended, the names of the functions will be used to make the names of the 
#' returned data frame.
#' @param by A grouping variable. If included, the `data` will be grouped by the values of the 
#' `by` variable before the summary statistics are applied.
#' @param pivot A logical variable indicating if the wide format da
#' 
#' @return A tibble data frame. If `pivot = F`, which is the default, the data
#'   frames contains one row per value of the `by` variable, or just one row overall
#'   if there is no `by` variable. If `pivot = T`, there will be `k` + 1 columns
#'   if there is no `by` variable, or `k` + 2 columns if there is a `by` variable,
#'   where `k` is the number of functions.
#' 
#' @examples 
#' describe_across(faithfulfaces, 
#'                 variables = c(trustworthy, faithful), 
#'                 functions = list(avg = mean, stdev = sd),
#'                 pivot = TRUE)
#' describe_across(faithfulfaces, 
#'                 variables = c(trustworthy, faithful), 
#'                 functions = list(avg = mean, stdev = sd), 
#'                 by = face_sex)
#' describe_across(faithfulfaces, 
#'                 variables = c(trustworthy, faithful), 
#'                 functions = list(avg = mean, stdev = sd), 
#'                 by = face_sex,
#'                 pivot = TRUE)
#' @import dplyr
#' @export 
describe_across <- function(data, variables, functions, by = NULL, pivot = FALSE){
  
  if (!is.null(enexpr(by))){
    data <- group_by(data, across({{ by }}))
  }
  
  if (!pivot){
    results <- data %>% 
      summarise(across({{ variables }}, functions), .groups = 'drop')
  } else {
    results <- data %>% 
      summarise(across({{ variables }}, functions, .names = "{.fn}_____{.col}"), .groups = 'drop') %>%
      tidyr::pivot_longer(cols = contains('_____'),
                          names_to = c('.value', 'variable'),
                          names_sep = '_____')
  }
  
  results
}


#' Descriptive statistics for variables with missing values
#' 
#' Most descriptive statistic function like [base::sum()], [base::mean()],
#' [stats::median()], etc., do not skip `NA` values when computing the results
#' and so always return `NA` if there is at least one `NA` in the input vector.
#' The `NA` values can be skipped always by setting the `na.rm` argument to
#' `TRUE`. While this is simply to do usually, in some cases, such as when a
#' function is being passed to another function, setting `na.rm = TRUE` in that
#' function requires creating a new anonymous function. The functions here,
#' which all end in `_xna`, are wrappers to common statistics functions, but
#' with `na.rm = TRUE`.
#' 
#'
#' @param ... Arguments to a descriptive statistic function
#' 
#' @examples 
#' set.seed(10101)
#' # Make a vector of random numbers
#' x <- runif(10, min = 10, max = 20)
#' # Concatenate with a NA value
#' x1 <- c(NA, x)
#' sum(x)
#' sum(x1) # Will be NA
#' sum_xna(x1) # Will be same as sum(x)
#' stopifnot(sum_xna(x1) == sum(x))
#' stopifnot(mean_xna(x1) == mean(x))
#' stopifnot(median_xna(x1) == median(x))
#' stopifnot(iqr_xna(x1) == IQR(x))
#' stopifnot(sd_xna(x1) == sd(x))
#' stopifnot(var_xna(x1) == var(x))
#' 
#' @export 
sum_xna <- function(...) base::sum(..., na.rm = TRUE)

#' @describeIn sum_xna The arithmetic mean for vectors with missing values.
#' @export
mean_xna <- function(...) base::mean(..., na.rm = TRUE)

#' @describeIn sum_xna The median for vectors with missing values.
#' @export
median_xna <- function(...) stats::median(..., na.rm = TRUE)

#' @describeIn sum_xna The interquartile range for vectors with missing values.
#' @export
iqr_xna <- function(...) stats::IQR(..., na.rm = TRUE)

#' @describeIn sum_xna The standard deviation for vectors with missing values.
#' @export
sd_xna <- function(...) stats::sd(..., na.rm = TRUE)

#' @describeIn sum_xna The variance for vectors with missing values.
#' @export
var_xna <- function(...) stats::var(..., na.rm = TRUE)


