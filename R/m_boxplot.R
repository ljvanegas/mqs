mqs.boxplot <- function(y, q = NULL, alpha = 0.05, type = "runs", intersect = FALSE){
  
  sol.low <- mqse(y, beta = 0.25, q[1], alpha, type = type, conf = TRUE)
  sol.med <- mqse(y, beta = 0.5, q[2], alpha, type = type, conf = TRUE)
  sol.high <- mqse(y, beta = 0.75, q[3], alpha, type = type, conf = TRUE)
  
  if(intersect == FALSE){
    aux <- .Iint(sol.low, sol.med, sol.high, y)
  }
  else{
    aux <- list(sol.low, sol.med, sol.high)
  }
  
  ans = list(lowerQ = fitted(aux[[1]]), median = fitted(aux[[2]]), higherQ = fitted(aux[[3]]))
  attr(ans, "class") =  "mqsBox"
  return(ans)
}
