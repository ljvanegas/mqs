\name{mqs-package}
\alias{mqs-package}
\alias{mqs-package}
\docType{package}
\title{Multiscale Quantile Segmentation}
\description{Segments univariate serial data into regions of constant beta-quantile, for given beta, where neither the number nor the location of jumps are known.}
\details{
\tabular{ll}{
Package: \tab mqs\cr
Type: \tab Package\cr
Version: \tab 1.0-1\cr
Date: \tab 2018-04-09\cr
License: \tab The GNU General Public License\cr
}
Index:
\preformatted{
fitted.mqs       fit of orginal sampling points from \code{mqs} estimate.
lines.mqs        plots solutions of \code{mqs} over data.
lines.mqsBox     plots solutions of \code{mqs.boxplot} over data.
MCasym           numeric vector with asymptotic monte-carlo simulations for the multiscale statistic.
MCsimulation     numeric vector with monte-carlo simulations from the multiscale statistic.
mqs              solution path of mqs estimator.
mqs.boxplot      fitted multiscale box plot.
}
}
\author{Laura Jula Vanegas. <ljulava@smathematik.uni-goettingen.de>
}
\seealso{
\code{\link[stepR]{stepFit}} 
}
\keyword{ package }
\examples{
## Estimating the median
## Create a random example
y <- c(rnorm(50), 2+rnorm(50))

## Obtain threshold value for alpha = 0.1 and beta = 0.5
path.med <- MCsimulation(1000, length(y), 0.5)

q.med <- quantile(path.med, 1-0.1)

## Calculate the median estimator
sol.med <- mqs(y, 0.5, q.med)

plot(y)
lines(sol.med, col = "red")

## Multiscale Box Plot
## Calculate threshold values for alpha = 0.1 and beta = 0.25 and beta = 0.75
path.low <- MCsimulation(1000, length(y), 0.25)
path.high <- MCsimulation(1000, length(y), 0.75)

q.low <- quantile(path.low, 1-0.1)
q.high <- quantile(path.high, 1-0.1)

## Calculate Multiscale box plot
bp <- mqs.boxplot(y, c(q.low, q.med, q.high))

plot(y)
lines(bp, col = "red")
}
