#plot the multiscale boxplot

lines.mqsBox = function(x, ...){
  lines(x$lowerQ, type = "s", col = "light salmon", ...)
  lines(x$median, type = "s", col = "red", ...)
  lines(x$higherQ, type = "s", col = "light salmon", ...)
}

