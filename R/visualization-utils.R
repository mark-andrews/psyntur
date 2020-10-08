#' A two dimensional scatterplot
#'
#' This function is a wrapper around the typical `ggplot` command to create two
#' dimensional scatterplots, i.e. using `geom_point`. It provides the option of
#' colouring point by a third variable, one that is usually, though not
#' necessarily categorical. Also, it provides the option of placing the line of
#' best fit on the scatterplot. If points are coloured by a categorical
#' variable, the a different line of best for each value of the categorical
#' variable is provided.
#' 

#' @param x A numeric variable in `data`. Its values are plotted on the x axis.
#' @param y A numeric variable in `data`. Its values are plotted on the y axis.
#' @param data A data frame with the `x` and `y` variables.
#' @param by An optional variable, usually categorical (factor or
#'   character), by which the points in the scatterplot are byed and
#'   coloured.
#' @param best_fit_line A logical variable indicating if the line of best fit
#'   should shown or not.
#' @examples 
#' scatterplot(x = attractive, y = trustworthy, data = faithfulfaces)
#' scatterplot(x = attractive, y = trustworthy, data = faithfulfaces,
#'             by = face_sex)
#' scatterplot(x = trustworthy, y = faithful, data = faithfulfaces,
#'             by = face_sex, best_fit_line = TRUE)
#' @import ggplot2
#' @export
scatterplot <- function(x, y, data, by = NULL, best_fit_line = FALSE){
  
  if (is.null(enexpr(by))) {
    the_aes <- aes(x = {{ x }}, y = {{ y }})
  } else {
    the_aes <- aes(x = {{ x }}, y = {{ y }}, colour = {{ by }})
  }
  p1 <- ggplot(data = data, mapping = the_aes ) + geom_point()
  
  if (best_fit_line){
    p1 <- p1 + stat_smooth(method = 'lm', se = FALSE, fullrange = TRUE, formula = 'y ~ x')
  }
  
  p1 + theme_classic() + scale_colour_brewer(palette = "Set1")#ggthemes::scale_colour_colorblind()
}

#' A Tukey box-and-whisker plot
#'
#' This function is a wrapper around a typical `ggplot` based box-and-whisker
#' plot, i.e. using `geom_boxplot`, which implements the Tukey variant of the
#' box-and-whisker plot. The `y` variable is the outcome variable whose
#' distribution is represented by the box-and-whisker plot. If the `x` variable
#' is missing, then a single box-and-whisker plot using all values of `y` is
#' shown. If an `x` variable is used, this is used an the independent variable
#' and one box-and-whisker plot is provided for each set of `y` values that
#' correspond to each unique value of `x`. For this reason, `x` is usually a
#' categorical variable. If `x` is a continuous numeric variable, it ideally
#' should have relatively few unique values, so that each value of `x`
#' corresponds to a sufficiently large set of `y` values.
#'


#' @param y The outcome variable
#' @param x The optional independent/predictor/grouping variable
#' @param data The data frame with the `y` and (optionally) `x` values.
#' @param by An optional variable, usually categorical (factor or character), by
#'   which the points in the box-and-whisker plots are grouped and coloured.
#' @param jitter A logical variable, defaulting to `FALSE`, that indicates if
#'   all points in each box-and-whisker plot should be shown as jittered points.
#' @param box_width The width of box in each box-and-whisker plot. The default
#'   used, `box_width = 1/3`, means that boxes will be relatively narrow.
#' @param jitter_width The width of the jitter relative to box width. For
#'   example, set `jitter_width = 1` if you want the jitter to be as wide the
#'   box.
#' @return A `ggplot` object, which may be modified with further `ggplot2`
#'   commands.
#' @examples
#' # A single box-and-whisker plot
#' tukeyboxplot(y = time, data = vizverb)
#' # One box-and-whisker plot for each value of a categorical variable
#' tukeyboxplot(y = time, x = task, data = vizverb)
#' # Box-and-whisker plots with jitters
#' tukeyboxplot(y = time, x = task, data = vizverb,  jitter = TRUE)
#' # `tukeyboxplot` can be used with a continuous numeric variable too
#' tukeyboxplot(y = len, x = dose, data = ToothGrowth)
#' tukeyboxplot(y = len, x = dose, data = ToothGrowth,
#'              by = supp, jitter = TRUE, box_width = 0.5, jitter_width = 1)
#' @import ggplot2
#' @export
tukeyboxplot <- function(y, x, data, 
                         by = NULL,
                         jitter = FALSE, 
                         box_width = 1/3,
                         jitter_width = 1/5){
  
  # If `x` is missing, and so we have one boxplot, use an empty `x` variable
  # with x = ''.
  if (missing(x)){
    the_aes <- aes(x = '', y = {{ y }})
  } else {
    the_aes <- aes(x = {{ x }}, y = {{ y }})
  }
  
  # If we have a `by`, set that as the "colour" aesthetic
  if (!is.null(enexpr(by))) {
    the_aes$colour <- enexpr(by)
  }
  
  # If we have a continuous `x` variable, we need to use aes(group = ...)
  if (!missing(x)){

      # If we have a `by` variable, we need to group by an interaction
      if (!is.null(enexpr(by))) {
        the_aes$group <- quo(interaction(!!enexpr(x), !!enexpr(by)))
      } else {
        the_aes$group <- enexpr(x)
      }
  }
  
  # The basic plot
  p1 <- ggplot(data, mapping = the_aes)
  
  # With jitter, 
  # the jitter
  # Set jitter to a fraction of box_width.
  if (jitter) {
    
    # outliers should be removed as they will be shown in the jitter
    p1 <- p1 + geom_boxplot(width = box_width, outlier.shape = NA)
    
    if (!is.null(enexpr(by))) {
      p1 <- p1 + geom_jitter(position = position_jitterdodge(dodge.width = box_width, jitter.width = box_width * jitter_width/2), size = 0.85, alpha = 0.75)
    } else {
      p1 <- p1 + geom_jitter(width = box_width * jitter_width/2, size = 0.85, alpha = 0.75)
    }
               
  } else {
    p1 <- p1 + geom_boxplot(width = box_width, outlier.size = 0.75)
  }
  
  # If `x` is missing, we don't want any ticks or labels on 'x' axis.
  if (missing(x)) p1 <- p1 + xlab(NULL) + theme(axis.ticks = element_blank()) 
  
  p1 + theme_classic() + scale_colour_brewer(palette = "Set1")
}

#' A histogram
#'
#' This is a wrapper to the typical `ggplot` based histogram, i.e., using
#' `geom_histogram`. A continuous variable, `x`, is required as an input.
#' Optionally, a `by` categorical variable can be provided.
#'
#' @param x The numeric variable that is to be histogrammed.
#' @param data A data frame with at least one numeric variable (the `x`
#'   variable).
#' @param by A categorical variable by which to group the `x` values. If
#'   provided there will be one histogram for each set of `x` values grouped by
#'   the values of the `by` variable.
#' @param position If the `by` variable is provided, there are three ways these
#'   multiple histograms can be positioned: stacked (`position = 'stack'`), side
#'   by side (`position = 'dodge'`), superimposed (`position = identity'`).
#' @param facet A character string or character vector. If provided, we
#'   `facet_wrap` (by default) the histogram by the variables. This is
#'   equivalent to the `facet_wrap(variables)` in `ggplot2`.
#' @param facet_type By default, this takes the value of `wrap`, and `facet`
#'   leads to a facet wrap. If `facet_type` is `grid`, then `facet` gives us a
#'   `facet_grid`.
#' @param bins The number of bins to use in the histogram.
#' @param alpha The transparency to for the filled histogram bars. This is
#'   probably only required when using `position = 'identity'`.
#' @examples
#' histogram(x= age, data = schizophrenia, by = gender, bins = 20)
#' histogram(x= age, data = schizophrenia, by = gender, position = 'identity', bins = 20, alpha = 0.7)
#' histogram(x= age, data = schizophrenia, by = gender, position = 'dodge', bins = 20)
#' histogram(x = weight, bins = 20, data = ansur, facet = height_tercile)
#' histogram(x = weight, bins = 20, data = ansur, 
#'           facet = c(height_tercile, age_tercile), facet_type = 'grid')
#' @import ggplot2 dplyr
#' @export histogram
histogram <- function(x, data, by = NULL, position = 'stack', facet = NULL, facet_type = 'wrap', bins = 10, alpha = 1.0){
  
  if (is.null(enexpr(by))) {
    the_aes <- aes(x = {{ x }})
  } else {
    the_aes <- aes(x = {{ x }}, fill = {{ by }})
  }
  
  p1 <- ggplot(data, mapping = the_aes) + geom_histogram(bins = bins,
                                                         colour = 'white',
                                                         position = position,
                                                         alpha = alpha)
  
  if (!is.null(enexpr(facet))) {
    
    # this monstrosity to deal with unquoted, possibly vector, arguments
    # to facet
    facet_expr <- enexpr(facet)
    
    if(length(facet_expr) == 1) {
      facet_vars <- as.list(facet_expr)
    } else {
      facet_vars <- as.list(facet_expr)[-1]
    }
    
    quoted_facet_vars <- sapply(facet_vars, rlang::quo_name)
    
    if (facet_type == 'wrap'){
      p1 <- p1 + facet_wrap(quoted_facet_vars, labeller = label_both)
    } else if (facet_type == 'grid'){
      p1 <- p1 + facet_grid(quoted_facet_vars, labeller = label_both)
    } else {
      stop(sprintf('facet_type should be "wrap" or "grid" not %s', facet_type))
    }
  }

  # minimal looks better than classic in a faceted plot
  if (is.null(enexpr(facet))) {
    p1 + theme_classic() + scale_fill_brewer(palette = "Set1")
  } else {
    p1 + theme_minimal() + scale_fill_brewer(palette = "Set1")
  }
 
}
