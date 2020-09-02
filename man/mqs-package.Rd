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
Version: \tab 2.0-1\cr
Date: \tab 2020-02-09\cr
License: \tab The GNU General Public License\cr
}
Index:
\preformatted{
fitted.mqs       fit of orginal sampling points from \code{mqs} estimate.
lines.mqs        plots solutions of \code{mqs} over data.
lines.mqsBox     plots solutions of \code{mqs.boxplot} over data.
MCasym           numeric vector with asymptotic monte-carlo simulations for the multiscale statistic.
MCmqs            numeric vector with monte-carlo simulations from the multiscale statistic.
mqse             solution path of mqs estimator.
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
path.med <- MCmqs(1000, length(y), 0.5)

q.med <- quantile(path.med, 1-0.1)

## Calculate the median estimator
sol.med <- mqse(y, 0.5, q.med)

plot(y)
lines(sol.med, col = "red")

## calculate the median estimator + confidence statements

sol.med.conf <- mqse(y, beta = 0.5, q = q.med, conf = TRUE)

# plot data, estimator and confidence statements

plot(y)

polygon(c(1:length(y), rev(1:length(y))), c(sol.med.conf$lowerCB, rev(sol.med.conf$upperCB)), 
        col = rgb(169,169,169, 100, maxColorValue = 255), border = NA)

lines(fitted(sol.med.conf), col="red", lwd=2, type="s")

#plot confidence intervals
if(length(sol.med.conf$left)>1){
  for(i in 1:nrow(sol.med.conf$confInt)){
    axis(1, at = c(sol.med.conf$confInt[i,1], sol.med.conf$confInt[i,2]), labels = c("", ""), 
         lwd = 2, tck=0.01, col="blue")
    axis(1, at = c(sol.med.conf$confInt[i,1], sol.med.conf$confInt[i,2]), labels = c("", ""), 
         lwd = 2, tck=-0.01, col="blue")
  }
}

## Multiscale Box Plot
## Calculate threshold values for alpha = 0.1 and beta = 0.25 and beta = 0.75
path.low <- MCmqs(1000, length(y), 0.25)
path.high <- MCmqs(1000, length(y), 0.75)

q.low <- quantile(path.low, 1-0.1)
q.high <- quantile(path.high, 1-0.1)

## Calculate Multiscale box plot
bp <- mqs.boxplot(y, c(q.low, q.med, q.high))

plot(y)
lines(bp)
}
