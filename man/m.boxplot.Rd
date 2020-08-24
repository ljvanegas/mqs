\name{mqs.boxplot}
\alias{mqs.boxplot}
\title{Multiscale box plot}
\description{Returns a multiscale box plot for given observations.}
\usage{mqs.boxplot(y, q = NULL, alpha = 0.05, type = "runs", intersect = FALSE)}
\arguments{
  \item{y}{numeric vector containing the serial data}
  \item{q}{numeric vector containing the threshold parameters to be used for the lower, middle and upper quantiles. If missing it calculates the threshold for the given level alpha.}
  \item{alpha}{numeric confidence level in (0,1).}
  \item{type}{specifies loss function for final estimate with multiscale constraint. Currently "runs" and "koenker" available}
  \item{intersect}{set to FALSE if quantiles should not intersect.}
}
\value{list of three numeric vectors of same length as y, containing the fitted estimats for the 0.25- 0.5- and 0.75- quantiles respectively.}
\seealso{\code{\link{fitted.mqs}}, \code{\link{mqse}}, \code{\link{MCmqs}}}
\examples{
## create a random example
y <- c(rnorm(100), rnorm(100, 1))

## Calculate threshold values for alpha = 0.1 and beta = 0.25, 0.5 and 0.75
path.low <- MCmqs(1000, length(y), 0.25)
path.med <- MCmqs(1000, length(y), 0.5)
path.high <- MCmqs(1000, length(y), 0.75)

q.low <- quantile(path.low, 1-0.1)
q.med <- quantile(path.med, 1-0.1)
q.high <- quantile(path.high, 1-0.1)

## Calculate Multiscale box plot
bp <- mqs.boxplot(y, c(q.low, q.med, q.high))

plot(y)
lines(bp)
}
