\name{mqs}
\alias{mqs}
\title{Multiscale Quantile Segmentation}
\description{Computes the mqs estimator for a given segmentation quantile beta.}
\usage{mqs(Y, beta, q = NULL, alpha = 0.05, type = "runs")}
\arguments{
  \item{Y}{numeric vector containing the serial data.}
  \item{beta}{numeric value in (0,1) giving the desired segmentation quantile.}
  \item{q}{numeric threshold parameter.}
  \item{alpha}{numeric confidence level in (0,1).}
  \item{type}{specifies loss function for final estimate with multiscale constraint. Currently "runs" and "koenker" available.}
}
\value{A {list} with components
\item{value}{numeric vector containing the fitted function values on each constant segment of the mqs estimator.}
\item{left}{numeric vector of the same length as value containing the left end of each segment.}
\item{n}{integer value which equals the length of Y.}
}
\note{For q = NULL and length(Y)<2000 the function performs Monte Carlo simulations as in \code{\link{MCsimulation}} to find an appropriate value of q. If length(Y)>2000, it takes the asymptotic Monte Carlo simulations as in \code{\link{MCasym}}.}
\seealso{\code{\link{fitted.mqs}}, \code{\link{mqs.boxplot}}, \code{\link{MCsimulation}}, \code{\link{MCasym}}}
\examples{
## create a random example
y <- c(rnorm(100), 1+rnorm(100))

## Obtain threshold value for alpha = 0.1 and beta = 0.5
path.med <- MCsimulation(1000, length(y), 0.5)

q.med <- quantile(path.med, 1-0.1)

## Calculate the median estimator
sol.med <- mqs(y, beta = 0.5, q = q.med)

## This gives the same result as

sol.med2 <- mqs(y, beta = 0.5, alpha = 0.1)

plot(y)
lines(fitted(sol.med), type = "s", col = "red")
}
