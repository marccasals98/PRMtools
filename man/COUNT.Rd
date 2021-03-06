% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPSS.R
\name{count}
\alias{count}
\title{count}
\usage{
count(x, codes)
}
\arguments{
\item{x}{the vector of the variables whose values we want to count}

\item{codes}{The array of codes that we want to count}
}
\value{
returns an integer with the value of the count.
}
\description{
It counts the times that
some codes appear in some variables and adds them up.
}
\examples{
p15_1 <- c(0, 2, 3, 99, 99, 99, 2)
p15_2 <- c(1, 2, 5, 3, 4, 52, 2)
data_frame <- data.frame(p15_1, p15_2)
count(data_frame[, c("p15_1","p15_2")], c(99,2))
}
