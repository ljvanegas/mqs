\name{lines.mqs}
\alias{lines.mqs}
\title{Plot lines for mqs object}
\description{
Plots a line with the mqs estimator for given serial data.
}
\usage{
\method{lines}{mqs}(x,...)
}
\arguments{
  \item{x}{solution of \code{\link{mqse}}}
  \item{...}{as in generic method.}
}

\seealso{\code{\link{mqse}}, \code{\link{mqs.boxplot}}, \code{\link{MCmqs}}}
\examples{
## create a random example
y <- c(rnorm(100), rnorm(100, 1))

## Calculate the median estimator
sol.med <- mqse(y, 0.5, q = 1.1543)

#Plot it
plot(y)
lines(sol.med, col = "red")
}
