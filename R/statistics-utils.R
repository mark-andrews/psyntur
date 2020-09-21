#' Shapiro-Wilk normality test
#' 
#' A wrapper around [stats::shapiro.test()]. It can be applied to single vectors
#' or groups of vectors.
#' 
#' @return A tibble data frame with one row for each value of the `by` variable,
#'   or one row overall if there is no `by` variable. For the `y` variable whose
#'   normality is being tested, for each subset of values corresponding to the
#'   values of they `by` variable, or for all values if there is no `by`
#'   variable, return the Shapiro-Wilk statistic, and the corresponding p-value.

#' @param y A numeric variable whose normality is being tested.
#' @param by An optional grouping variable
#' @param data A data frame containing `y` and the `by` variable
#' @examples 
#' shapiro_test(faithful, data = faithfulfaces)
#' shapiro_test(faithful, by = face_sex, data = faithfulfaces)
#' @import dplyr
#' @export
shapiro_test <- function(y, by = NULL, data){
  
  get_shapiro_test_results <- function(x){
    results <- stats::shapiro.test(x)
    tibble(statistic = results$statistic, p_value = results$p.value)
  }
  
  if (is.null(enexpr(by))) {
    results <- summarise(data, get_shapiro_test_results({{y}}))
  } else {
    results <- summarise(group_by(data, {{by}}), 
                         get_shapiro_test_results({{y}}),
                         .groups = 'drop')
  }
  
  results
 
}

#' Independent samples t-test
#' 
#' A wrapper to [stats::t.test()] with `var.equal = TRUE`.
#' 
#' @param formula A two sided formula with one variable on either side, e.g. y ~
#'   x, where the left hand side, dependent, variable is a numeric variable in
#'   `data` and the right hand side, independent, variable is a categorical or
#'   factor variable in `data`, and which has only two distinct values.
#' @param data A data frame that contains the dependent and independent
#'   variables.
#'   
#' @examples 
#' t_test(trustworthy ~ face_sex, data = faithfulfaces)
#'   
#' @import formula.tools
#' @export 
t_test <- function(formula, data) {
  
  two_sided_formula <- formula.tools::is.two.sided(formula)
  independent_variable <- formula.tools::rhs.vars(formula)
  dependent_variable <- formula.tools::lhs.vars(formula)
  
  if (!(two_sided_formula && length(dependent_variable) == 1 && length(independent_variable) == 1)) {
    stop(sprintf('The formula should be two sided, with one variable on each side, e.g. y ~ x, not %s.', as.character(formula)))
  }
  
  y <- data[[dependent_variable]]
  x <- data[[independent_variable]]
  
  if (is.null(y)) {
    stop(
      sprintf('The variable "%s" does not exist in the "%s" data frame.', 
              dependent_variable, 
              deparse(substitute(data)))
    )
  }
  
  if (is.null(x)) {
    stop(
      sprintf('The variable "%s" does not exist in the "%s" data frame.', 
              independent_variable, 
              deparse(substitute(data)))
    )
  }
  
  if (!is.numeric(y)) {
    stop(
      sprintf('The dependent variable "%s" should be a numeric variable, not a %s.', 
              dependent_variable, 
              class(y))
    )
  }
  
  if (!is.factor(x) && !is.character(x)) {
    stop(
      sprintf('The independent variable "%s" should be a character or factor variable, not a %s.', 
              independent_variable, 
              class(x))
    )
  }
  
  if (n_distinct(independent_variable) == 2) {
    stop(
      sprintf('The independent variable "%s" should have two distinct values not %d.', 
              independent_variable, 
              n_distinct(independent_variable))
    )
  }
  
  stats::t.test(formula, data = data, var.equal = TRUE)
}


#' Paired samples t-test
#' 
#' A wrapper to [stats::t.test()] with `paired = TRUE`.
#' 
#' @param y1 A numeric vector of observations
#' @param y2 A numeric vector of observations, with each value of y2 is assumed to 
#' be paired, such as by repeated measures, the corresponding value of y1.
#' @param data A data frame with `y1` and `y2` as values.
#' @param ... Additional arguments passed to [stats::t.test()].
#' 
#' @examples 
#' paired_t_test(y1, y2, data = pairedsleep)
#' @import dplyr
#' @import tidyr
#' @export
paired_t_test <- function(y1, y2, data, ...){
  
  vec_1 <- data[[enexpr(y1)]]
  vec_2 <- data[[enexpr(y2)]]
  
  if (is.null(vec_1)) {
    stop(
      sprintf('The variable "%s" does not exist in the "%s" data frame.', 
              y1, 
              deparse(substitute(data)))
    )
  }
  
  if (is.null(vec_2)) {
    stop(
      sprintf('The variable "%s" does not exist in the "%s" data frame.', 
              y2, 
              deparse(substitute(data)))
    )
  }
  
  if (!is.numeric(vec_1)) {
    stop(
      sprintf('The variable "%s" should be a numeric variable, not a %s.', 
              y1, 
              class(vec_1))
    )
  }
  
  if (!is.numeric(vec_2)) {
    stop(
      sprintf('The variable "%s" should be a numeric variable, not a %s.', 
              y2, 
              class(vec_2))
    )
  }
  
  if (!(length(vec_1) == length(vec_2))){
    stop(
      sprintf('The variables "%s" and "%s" must be of the same length.', 
              y1, 
              y2)
    )
  }
  
  stats::t.test(x = vec_1, y = vec_2, paired = TRUE, ...)
}


#' Pairwise t-test
#'
#' This is wrapper to the `pairwise.t.test` function. The p-value adjustment is
#' "bonferroni" by default. Other possible values are "holm", "hochberg",
#' "hommel", "BH", "BY", "fdr", "none". See [stats::p.adjust()].
#'
#' @param formula A two sided formula with one variable on either side, e.g. y ~
#'   x, where the left hand side, dependent, variable is a numeric variable in
#'   `data` and the right hand side, independent, variable is a categorical or
#'   factor variable in `data`.
#' @param data A data frame that contains the dependent and independent
#'   variables.
#' @param p_adj The p-value adjustment method (see Description).
#' 
#' @examples 
#' data_df <- dplyr::mutate(vizverb, IV = interaction(task, response))
#' pairwise_t_test(time ~ IV, data = data_df)
#'
#' @import formula.tools
#' @export
pairwise_t_test <- function(formula, data, p_adj = 'bonferroni'){
  
  two_sided_formula <- formula.tools::is.two.sided(formula)
  independent_variable <- formula.tools::rhs.vars(formula)
  dependent_variable <- formula.tools::lhs.vars(formula)
  
  if (!(two_sided_formula && length(dependent_variable) == 1 && length(independent_variable) == 1)) {
    stop(sprintf('The formula should be two sided, with one variable on each side, e.g. y ~ x, not %s.', as.character(formula)))
  }
  
  y <- data[[dependent_variable]]
  x <- data[[independent_variable]]
  
  if (is.null(y)) {
    stop(
      sprintf('The variable "%s" does not exist in the "%s" data frame.', 
              dependent_variable, 
              deparse(substitute(data)))
    )
  }
  
  if (is.null(x)) {
    stop(
      sprintf('The variable "%s" does not exist in the "%s" data frame.', 
              independent_variable, 
              deparse(substitute(data)))
    )
  }
  
  if (!is.numeric(y)) {
    stop(
      sprintf('The dependent variable "%s" should be a numeric variable, not a %s.', 
              dependent_variable, 
              class(y))
    )
  }
  
  if (!is.factor(x) && !is.character(x)) {
    stop(
      sprintf('The independent variable "%s" should be a character or factor variable, not a %s.', 
              independent_variable, 
              class(x))
    )
  }
  
  stats::pairwise.t.test(y, x, p.adjust.method = p_adj)
}
