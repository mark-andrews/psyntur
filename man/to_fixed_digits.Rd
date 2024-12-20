% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangling-utils.R
\name{to_fixed_digits}
\alias{to_fixed_digits}
\title{Format Numeric Columns to Fixed Digits}
\usage{
to_fixed_digits(data, ..., .digits = 3)
}
\arguments{
\item{data}{A data frame or tibble containing the columns to format.}

\item{...}{<\code{\link[dplyr:select]{tidy-select}}> Columns to apply the fixed digit formatting to.
If no columns are specified, all numeric columns are selected.}

\item{.digits}{An integer specifying the number of decimal places to format to.
Default is 3.}
}
\value{
A data frame with the selected numeric columns formatted to the specified number of decimal places.
}
\description{
This function formats specified numeric columns in a data frame to a fixed number of decimal places.
}
\details{
Tibble data frames display numeric values to a certain number of significant
figures, determined by the \code{pillar.sigfig} option. Sometimes it
may be useful or necessary to see values to a fixed number of digits. This
can be accomplished with \link[tibble]{num}. This function is a convenience function that applies
\link[tibble]{num} to all, or a specified subset, of the numeric vectors in a
tibble.
}
\examples{
# Format all numeric columns to 3 decimal places
mtcars_df <- tibble::as_tibble(mtcars)
to_fixed_digits(mtcars_df)

# Format columns mpg to qsec to 3 decimal places
to_fixed_digits(mtcars_df, mpg:qsec)

# Format specific columns to 2 decimal places
to_fixed_digits(mtcars_df, mpg, hp, .digits = 2)

}
