# extracts estimator from solution

fitted.mqs = function(object, ...){
  value=object[[1]]
  bounds=object[[2]]
  l=length(object[[1]])
  bounds[l+1]=object[[3]]
  estimator=numeric(object[[3]])
  
  for(i in 1:l){
    for(j in bounds[i]:bounds[i+1]){
      estimator[j]=value[i]
    }
  }
  return(estimator)
}
