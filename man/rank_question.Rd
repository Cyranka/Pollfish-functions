% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rank_question.R
\name{rank_question}
\alias{rank_question}
\title{Creating a basic crosstab for a single rank question from a pollfish file}
\usage{
rank_question(d_frame, x_var, y_var)
}
\arguments{
\item{d_frame:}{Data frame created from a pollfish file}

\item{x_var:}{Variable that represent the categories}

\item{y_var:}{Variable that represent the ranking}
}
\description{
Function that creates the crosstabulation between the x_var and the y_var for a single rank choice question. Output is a list with rank choices for all factors of x_var.
}
\examples{
x <- read_pollfish_file("Pollfish_Survey.xls")
rank_question(x, "age", "Q8")
}
\keyword{Pollfish,}
\keyword{basic}
\keyword{crosstab}
