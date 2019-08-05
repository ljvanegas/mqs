\name{lines.mqsBox}
\alias{lines.mqsBox}
\title{Plot multiscale box plot}
\description{
Plots lines with the mqs multiscale boxplot for given serial data.
}
\usage{
\method{lines}{mqsBox}(x,...)
}
\arguments{
  \item{x}{solution of \code{\link{mqs}}}
  \item{...}{as in generic method.}
}

\seealso{\code{\link{mqs}}, \code{\link{mqs.boxplot}}, \code{\link{MCsimulation}}}
\examples{
## create a random example
y <- c(rnorm(100), rnorm(100, 2))

## Calculate the multiscale boxplot
sol <- mqs.boxplot(y, q = c(1.2452, 1.2671, 1.2983))

#Plot it
plot(y)
lines(sol, lwd = 2, col ="red")
}
