\name{MCmqs}
\alias{MCmqs}
\title{Monte Carlo simulation}
\description{Performs Monte Carlo simulations of the multiscale statistic for given number of observations and given segmentation quantile beta.}
\usage{MCmqs(N, n, beta)}
\arguments{
  \item{N}{integer value giving number of simulations.}
  \item{n}{integer value giving length of observations.}
  \item{beta}{numeric value in (0,1) giving segmentation quantile.}
}
\value{Numeric vector containing N monte-carlo simulations.}
\seealso{\code{\link{fitted.mqs}}, \code{\link{mqs.boxplot}}, \code{\link{mqse}}}
\examples{
## Threshold for n = 20, beta = 0.5 and alpha=0.05
stat <- MCmqs(1000, 20, 0.5)

q <- quantile(stat, 1-0.05)
}
