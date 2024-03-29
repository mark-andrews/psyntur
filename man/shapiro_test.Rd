% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics-utils.R
\name{shapiro_test}
\alias{shapiro_test}
\title{Shapiro-Wilk normality test}
\usage{
shapiro_test(y, by = NULL, data)
}
\arguments{
\item{y}{A numeric variable whose normality is being tested.}

\item{by}{An optional grouping variable}

\item{data}{A data frame containing \code{y} and the \code{by} variable}
}
\value{
A tibble data frame with one row for each value of the \code{by} variable,
or one row overall if there is no \code{by} variable. For the \code{y} variable whose
normality is being tested, for each subset of values corresponding to the
values of they \code{by} variable, or for all values if there is no \code{by}
variable, return the Shapiro-Wilk statistic, and the corresponding p-value.
}
\description{
This function is a wrapper around \code{\link[stats:shapiro.test]{stats::shapiro.test()}}.
It implements the Shapiro-Wilk test that tests the null hypothesis that a sample of values is a sample from a normal distribution.
Thie function can be applied to single vectors or groups of vectors.
}
\examples{
shapiro_test(faithful, data = faithfulfaces)
shapiro_test(faithful, by = face_sex, data = faithfulfaces)
}
