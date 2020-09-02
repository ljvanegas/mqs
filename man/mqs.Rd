\name{mqse}
\alias{mqse}
\title{Multiscale Quantile Segmentation Estimator}
\description{Computes the mqs estimator for a given segmentation quantile beta.}
\usage{mqse(Y, beta = 0.5, q = NULL, alpha = 0.05, conf = FALSE, type = "runs")}
\arguments{
  \item{Y}{numeric vector containing the serial data.}
  \item{beta}{numeric value in (0,1) giving the desired segmentation quantile.}
  \item{q}{numeric threshold parameter.}
  \item{alpha}{numeric confidence level in (0,1).}
  \item{conf}{set to true for the calculation of confidence intervales and confidence bands.}
  \item{type}{specifies loss function for final estimate with multiscale constraint. Currently "runs" and "koenker" available.}
}
\value{A {list} with components
\item{value}{numeric vector containing the fitted function values on each constant segment of the mqs estimator.}
\item{left}{numeric vector of the same length as value containing the left end of each segment.}
\item{n}{integer value which equals the length of Y.}
\item{confInt}{if conf = TRUE, numeric matrix with 2 columns and length(value)-1 rows. Each row represents the left and right confidence bounds for the confidence interval asociated to a change point.}
\item{lowerCB}{numeric vector of length n containing the lower bound of the confidence band.}
\item{upperCB}{numeric vector of length n containing the upper bound of the confidence band.}
}
\note{For q = NULL and length(Y)<2000 the function performs Monte Carlo simulations as in \code{\link{MCmqs}} to find an appropriate value of q. If length(Y)>2000, it takes the asymptotic Monte Carlo simulations as in \code{\link{MCasym}}.}
\seealso{\code{\link{fitted.mqs}}, \code{\link{mqs.boxplot}}, \code{\link{MCmqs}}, \code{\link{MCasym}}}
\examples{
## create a random example
y <- c(rnorm(100), 1+rnorm(100))

## Obtain threshold value for alpha = 0.1 and beta = 0.5
path.med <- MCmqs(1000, length(y), 0.5)

q.med <- quantile(path.med, 1-0.1)

## Calculate the median estimator
sol.med <- mqse(y, beta = 0.5, q = q.med)

## This gives the same result as

sol.med2 <- mqse(y, beta = 0.5, alpha = 0.1)

plot(y)
lines(fitted(sol.med), type = "s", col = "red")

## same example with confidence bands

sol.med.conf <- mqse(y, beta = 0.5, q = q.med, conf = TRUE)

# plot data, estimator and confidence statements

plot(y)

#Plot confidence bands
polygon(c(1:length(y), rev(1:length(y))), c(sol.med.conf$lowerCB, rev(sol.med.conf$upperCB)), 
        col = rgb(169,169,169, 100, maxColorValue = 255), border =NA)

#Plot confidence intervals, if there is any c.p.
if(length(sol.med.conf$left)>1){
lines(fitted(sol.med.conf), col="red", lwd=2)
  for(i in 1:nrow(sol.med.conf$confInt)){
    axis(1, at = c(sol.med.conf$confInt[i,1], sol.med.conf$confInt[i,2]), labels = c("", ""), 
         lwd = 2, tck=0.01, col="blue")
    axis(1, at = c(sol.med.conf$confInt[i,1], sol.med.conf$confInt[i,2]), labels = c("", ""), 
         lwd = 2, tck=-0.01, col="blue")
  }
}
}
