#lines to a MQS object

lines.mqs = function(x, ...){
  lines(fitted(x), type = "s", ...)
}