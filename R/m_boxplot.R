mqs.boxplot <- function(y, q = NULL, alpha = 0.05, type = "runs"){
  
  sol.low <- mqs(y, beta = 0.25, q[1], alpha, type = type)
  sol.med <- mqs(y, 0.5, q[2], alpha, type = type)
  sol.high <- mqs(y, 0.75, q[3], alpha, type = type)
  
  ans = list(lowerQ = fitted(sol.low), median = fitted(sol.med), higherQ = fitted(sol.high))
  attr(ans, "class") =  "mqsBox"
  return(ans)
}