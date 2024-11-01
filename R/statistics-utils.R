#' Shapiro-Wilk normality test
#' 
#' This function is a wrapper around [stats::shapiro.test()]. 
#' It implements the Shapiro-Wilk test that tests the null hypothesis that a sample of values is a sample from a normal distribution.
#' Thie function can be applied to single vectors or groups of vectors.
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

#' Test for Correlation Between Paired Samples
#' 
#' This function is a wrapper around [stats::cor.test()]. 
#' It implements the Pearson's correlation test that tests the null hypothesis 
#' that two paired samples of values are unrelated.
#' This function must be applied to two numeric vectors.
#' 
#' @return A tibble data frame with the correlation statistic, and the corresponding p-value.

#' @param x A numeric variable.
#' @param y A numeric variable.
#' @param data A data frame containing the `y` and `x` variables
#' @param method 	A character string indicating which correlation 
#' coefficient is to be used: "pearson", "kendall", or "spearman". Default method is "pearson".
#' @examples 
#' cor_test(y = sex_dimorph, x = attractive, data = faithfulfaces)
#' cor_test(y = sex_dimorph, x = attractive, method = "spearman", data = faithfulfaces)
#' @import dplyr
#' @export
cor_test <- function(x, y, method = "pearson", data){
  
  get_cor_test_results <- function(x, y, method){
    results <- stats::cor.test(x, y, method = method)
    dplyr::tibble(cor = results$estimate, 
                  t = results$statistic, 
                  df = results$parameter,  
                  p_value = results$p.value)
  }
  
  results <- dplyr::summarise(data, get_cor_test_results({{x}}, {{y}}, {{method}}))
  
  results
  
}

#' Test for Correlation Between Paired Samples for 2 or More Variables
#' 
#' This function is a wrapper around [stats::cor.test()]. 
#' It implements the Pearson's correlation test that tests the null hypothesis 
#' that two or more paired samples of values are unrelated.
#' This function can be applied to two or more numeric variables in the provided data.
#' 
#' @return By default a matrix with correlation coefficients. Output format and included statistics 
#' can be changed in the argument settings.

#' @param .data A data frame.
#' @param ... Variables for which the correlation coefficient should be returned. 
#' If no variable name is provided, correlations will be returned for all numeric 
#' variables in `.data`.
#' @param .pvalues logical If FALSE (default), p-values will be omitted from the
#' output. If TRUE, p-values will be included in the output.
#' @param .ci logical If FALSE (default), 95% confidence interval bounds will be 
#' omitted from the output. If TRUE, 95% confidence interval bounds will be 
#' included in the output.
#' @param .as_matrix logical If TRUE (default), results will be return as matrix. 
#' If TRUE, results will be returned as tibble.
#' @param .omit_redundancies logical If FALSE (default), all n^2 correlations 
#' will be include in the output. If TRUE, only unique correlations will be 
#' returned (x ~ y but not y ~ x) and correlation of a variable with itself will
#' be omitted.
#' @param .method 	A character string indicating which correlation coefficient 
#' is to be used: "pearson", "kendall", or "spearman". Default method is "pearson".
#' @examples 
#' # Calculate the correlations between all numeric variables in the `faithfulfaces` data.
#' cor_test_multi(faithfulfaces)
#' # Calculate the correlations between the 1st, 2nd and 4th variable.
#' cor_test_multi(faithfulfaces, c(1,2,4))
#' # Calculate the correlations between `sex_dimorph`, `attractive`, and `trustworthy`.
#' cor_test_multi(faithfulfaces, sex_dimorph, attractive, trustworthy)
#' # Calculate all correlations and return p-values and 95% confidence intervals.
#' cor_test_multi(faithfulfaces, .pvalues = TRUE, .ci = TRUE)
#' # Calculate all correlations with p-values and 95% confidence intervals and 
#' return results as table with only unique pairs of the off-diagonal correlations.
#' cor_test_multi(faithfulfaces, .pvalues = TRUE, .ci = TRUE, .as_matrix = FALSE, 
#' .omit_redundancies = TRUE)

cor_test_multi <- function(.data,
                           ...,
                           .pvalues = FALSE,
                           .ci = FALSE,
                           .as_matrix = TRUE, 
                           .omit_redundancies = FALSE,
                           .method = "pearson"){
  
  if (!(.method %in% c("pearson", "kendall", "spearman"))) {
    stop('The correlation method must be either "pearson", "kendall" or "spearman".')
  }
  
  data <- dplyr::select(.data, ...)
  
  # If not variables are named, use all numeric values in data
  if(ncol(data)==0) data <- dplyr::select(.data, where(is.numeric))
  
  nvars <- ncol(data)
  varnames <- names(data)
  
  results_matrix <- matrix(nrow = nvars, 
                           ncol = nvars, 
                           dimnames = list(varnames, varnames))
  
  results <- list(results_matrix, results_matrix,
                  results_matrix, results_matrix)
  
  names(results) <- c("cor", "p_value", "lower", "upper")
  
  cor_list <- purrr::map(varnames,
                         ~purrr::map(varnames, 
                                     ~stats::cor.test(x = pull(data, .x),
                                                      y = pull(data, .y), 
                                                      method = .method), 
                                     .y=.x)) 
  
  for(i in seq(nvars)){
    for(j in seq(nvars)){
      results$cor[i,j] <- as.vector(cor_list[[i]][[j]]$estimate)
      p_value <- cor_list[[i]][[j]]$p.value
      ci <- cor_list[[i]][[j]]$conf.int
      results$lower[i,j] <- ci[1]
      results$upper[i,j] <- ci[2]
      if(p_value == 0) p_value <- .Machine$double.xmin
      results$p_value[i,j] <- p_value
    }
  }
  
  # Return results as table
  if(!.as_matrix){
    results <- results %>% 
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      tidyr::pivot_longer(-rowname, names_to = c(".value", "name"),
                          names_pattern = "(.*)\\.(.*)") %>%
      dplyr::rename(x = rowname, y = name) %>%
      dplyr::select(x, y, cor, lower, upper, p_value)
    
    if(.omit_redundancies){
      results <- results %>%
        dplyr::filter(x != y) %>%
        dplyr::mutate(test1 = paste(x,y),
                      test2 = paste(y,x)) %>%
        tidyr::pivot_longer(test1:test2) %>%
        dplyr::mutate(dup = duplicated(value)) %>%
        dplyr::filter(!dup) %>%
        dplyr::select(-value, -dup, -name) %>%
        unique()
    } 
    
    # Remove p-values and CIs if not required (default)
    if(!.pvalues) results <- dplyr::select(results, -p_value)
    if(!.ci) results <- dplyr::select(results, -lower, -upper)
    
  } else if(.as_matrix){
    # Remove redundant pairs
    if(.omit_redundancies){
      for(i in 2:nvars){
        results$cor[i,i:nvars] <- NA
        results$p_value[i,i:nvars] <- NA
        results$lower[i,i:nvars] <- NA
        results$upper[i,i:nvars] <- NA
      }
      results$cor <- results$cor[-1,-nvars]
      results$p_value <- results$p_value[-1,-nvars]
      results$lower <- results$lower[-1,-nvars]
      results$upper <- results$upper[-1,-nvars]
    }    
    # Remove p-values and CIs if not required
    if(!.pvalues) results["p_value"] <- NULL
    if(!.ci) results[c("lower", "upper")] <- NULL
    
  }
  
  return(results)
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
#' @return A list with class "htest" as returned by [stats::t.test()].
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
#' @return A list with class "htest" as returned by [stats::t.test()].
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
#' @return An object of class `pairwise.htest` as returned by [stats::pairwise.t.test()].
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

#' Cohen's d and Hedges g effect size
#' 
#' This is wrapper to the [effsize::cohen.d()] function.
#' @param ... A comma separated list of arguments. See [effsize::cohen.d()].
#' @return A list of class `effsize` as returned by [effsize::cohen.d()].
#' @examples 
#' cohen_d(weight ~ gender, data = ansur)
#' cohen_d(age ~ gender, data = schizophrenia)
#' @export
cohen_d <- function(...){
  effsize::cohen.d(...)
}

#' Show the dummy code of a categorical variable
#' 
#' For each value of a categorical variables, show the binary
#' code used in a regression model to represent its value.
#' This is wrapper to the [fastDummies::dummy_cols()] function.
#' @param Df A data frame
#' @param variable A categorical variable (e.g. character vector or factor).
#' @return A data frame whose rows provide the dummy code for 
#'    each distinct value of `variable`.
#' @examples 
#' get_dummy_code(PlantGrowth, group)
#' @export
get_dummy_code <- function(Df, variable){
  
  var <- rlang::enquo(variable)
  
  tmp_df <- fastDummies::dummy_cols(Df, remove_first_dummy = TRUE)
  tmp_df <- dplyr::select(tmp_df, dplyr::starts_with(rlang::as_label(var)))
  dplyr::distinct(tmp_df)
  
}



#' Analysis of variance
#' 
#' This is wrapper to the [ez::ezANOVA()] function.
#' @param data Data frame containing the data to be analyzed.
#' @param dv Name of the column in `data` that contains the dependent variable. Values in this column must be numeric.
#' @param wid Name of the column in `data` that contains the variable specifying the case/Ss identifier. This should be a unique value per case/Ss.
#' @param within Names of columns in `data` that contain predictor variables that are manipulated (or observed) within-Ss.  
#' @param within_full Same as within, but intended to specify the full within-Ss design in cases where the data have not already been collapsed to means per condition specified by `within` and when `within` only specifies a subset of the full design.
#' @param within_covariates Names of columns in `data` that contain predictor variables that are manipulated (or observed) within-Ss and are to serve as covariates in the analysis.  
#' @param between Names of columns in `data` that contain predictor variables that are manipulated (or observed) between-Ss.  
#' @param between_covariates Names of columns in `data` that contain predictor variables that are manipulated (or observed) between-Ss and are to serve as covariates in the analysis.  
#' @param observed Names of columns in `data` that are already specified in either `within` or `between` that contain predictor variables that are observed variables (i.e. not manipulated).  
#' @param diff Names of any variables to collapse to a difference score. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.  
#' @param reverse_diff Logical. If TRUE, triggers reversal of the difference collapse requested by `diff`. Take care with variables with more than 2 levels.
#' @param type Numeric value (either `1`, `2` or `3`) specifying the Sums of Squares type to employ when data are unbalanced (eg. when group sizes differ). 
#' @param white.adjust  Only affects behaviour if the design contains only between-Ss predictor variables. If not FALSE, the value is passed as the white.adjust argument to Anova, which provides heteroscedasticity correction.
#' @param detailed Logical. If TRUE, returns extra information (sums of squares columns, intercept row, etc.) in the ANOVA table.
#' @param return_aov Logical. If TRUE, computes and returns an aov object corresponding to the requested ANOVA (useful for computing post-hoc contrasts).
#' @return A list containing one or more components as returned by [ez::ezANOVA()].
#' @examples 
#' ez_anova(data = selfesteem2_long,
#'             dv = score,
#'             wid = id,
#'             within = c(time, treatment),
#'             detailed = TRUE,
#'             return_aov = TRUE)
#' @importFrom ez ezANOVA             
#' @export
ez_anova <- ez::ezANOVA

