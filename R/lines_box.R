#plot the multiscale boxplot

lines.mqsBox = function(x, ...){
  lines(x$lowerQ, type = "s", lty = 6, ...)
  lines(x$median, type = "s", ...)
  lines(x$higherQ, type = "s", lty = 6, ...)
}
