\name{fitted.mqs}
\alias{fitted.mqs}
\title{Fitted step function}
\description{Gives a fit of the original sampling points from the solution path of \code{\link{mqse}}.}
\usage{\method{fitted}{mqs}(object, ...)}
\arguments{
  \item{object}{solution path of \code{\link{mqse}}.}
  \item{...}{as in generic method.}
}
\value{A numeric vector of length sol$n.}
\seealso{\code{\link{mqse}}, \code{\link{mqs.boxplot}}, \code{\link{MCmqs}}}
\examples{
## create a random example
y <- c(rnorm(100), rnorm(100, 1))

## Calculate the median estimator
sol.med <- mqse(y, 0.5, q = 1.1543)

estimator.median = fitted(sol.med)

plot(y)
lines(estimator.median, type = "s", col = "red")
}
