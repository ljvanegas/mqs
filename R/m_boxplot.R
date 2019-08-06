mqs.boxplot <- function(y, q = NULL, alpha = 0.05, type = "runs"){
  
  sol.low <- mqse(y, beta = 0.25, q[1], alpha, type = type)
  sol.med <- mqse(y, beta = 0.5, q[2], alpha, type = type)
  sol.high <- mqse(y, beta = 0.75, q[3], alpha, type = type)
  
  ans = list(lowerQ = fitted(sol.low), median = fitted(sol.med), higherQ = fitted(sol.high))
  attr(ans, "class") =  "mqsBox"
  return(ans)
}