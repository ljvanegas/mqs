.Iint <- function (s1, s2, s3, data){
  
  n <- length(data)
  
  i1 <- s1$confInt
  i2 <- s2$confInt
  i3 <- s3$confInt
  
  changed1 <- logical(nrow(i1))
  changed2 <- logical(nrow(i2))
  changed3 <- logical(nrow(i3))
  
  #intersect the conf intervals and assign new c.p.s
  for(k in 1:nrow(i1)){
    #check intersections i1 i2
    for(i in 1:nrow(i2)){
      for(l in 1:nrow(i3)){
        #check intersections i1 i2
        
        if(((i2[i,1]<=i1[k,1] && i2[i,2]>=i1[k,1]) || (i2[i,1]<=i1[k,2] && i2[i,1]>=i1[k,1])) && changed1[k] == 0 && changed2[i] == 0){
          i1[k,1] <- max(i1[k,1],i2[i,1])
          i1[k,2] <- min(i1[k,2],i2[i,2])
          i2[i,] <- i1[k,]
          
          #check intersection of the three
            if(((i3[l,1]<=i1[k,1] && i3[l,2]>=i1[k,1])||(i3[l,1]<=i1[k,2] && i3[l,1]>=i1[k,1])) && changed3[l] == 0){
              i1[k,1] <- max(i1[k,1],i3[l,1])
              i1[k,2] <- min(i1[k,2],i3[l,2])
              i2[i,] <- i1[k,]
              i3[l,] <- i1[k,]
              
              changed1[k] <- TRUE
              changed2[i] <- TRUE
              changed3[l] <- TRUE
              
              #if there is intersection asign mean c.p.
              leftaux <- floor(mean(c(s1$left[k+1], s2$left[i+1], s3$left[l+1])))
              
            
            if(leftaux < i1[k,1]){
              s1$left[k+1] <- i1[k,1]
              s2$left[i+1] <- i1[k,1]
              s3$left[l+1] <- i1[k,1]
            }
            
            else if(leftaux > i1[k,2]){
              s1$left[k+1] <- i1[k,2]
              s2$left[i+1] <- i1[k,2]
              s3$left[l+1] <- i1[k,2]
            }
            else{
              s1$left[k+1] <- leftaux
              s2$left[i+1] <- leftaux
              s3$left[l+1] <- leftaux
            }
            break
            }
          
          #if there is only int of i1, i2
         
          leftaux <- floor(mean(c(s1$left[k+1], s2$left[i+1])))
        
          if(leftaux < i1[k,1]){
            s1$left[k+1] <- i1[k,1]
            s2$left[i+1] <- i1[k,1]
          }
          else if(leftaux > i1[k,2]){
            s1$left[k+1] <- i1[k,2]
            s2$left[i+1] <- i1[k,2]
          }
          else{
            s1$left[k+1] <- leftaux
            s2$left[i+1] <- leftaux
          }
        
        }
        #check interesctions of i1 i3 only
        
        if(((i3[l,1]<=i1[k,1] && i3[l,2]>=i1[k,1])||(i3[l,1]<=i1[k,2] && i3[l,1]>=i1[k,1])) && changed1[k] == 0 && changed3[l] == 0){
          
          i1[k,1] <- max(i1[k,1],i3[l,1])
          i1[k,2] <- min(i1[k,2],i3[l,2])
          i3[l,] <- i1[k,]
          
          #if there is intersection asign mean c.p.
          leftaux <- floor(mean(c(s1$left[k+1], s3$left[l+1])))
          
          if(leftaux < i1[k,1]){
            s1$left[k+1] <- i1[k,1]
            s3$left[l+1] <- i1[k,1]
          }
          
          else if(leftaux > i1[k,2]){
            s1$left[k+1] <- i1[k,2]
            s3$left[l+1] <- i1[k,2]
          }
          else{
            s1$left[k+1] <- leftaux
            s3$left[l+1] <- leftaux
          }
          break
        }
        
        #check intersections of i2 i3 only
        
        if(((i3[l,1]<=i2[i,1] && i3[l,2]>=i2[i,1])||(i3[l,1]<=i2[i,2] && i3[l,1]>=i2[i,1])) && changed2[i] == 0 && changed3[l] == 0){
          
          i2[i,1] <- max(i2[i,1],i3[l,1])
          i2[i,2] <- min(i2[i,2],i3[l,2])
          i3[l,] <- i2[i,]
          
          #if there is intersection asign mean c.p.
          leftaux <- floor(mean(c(s2$left[i+1], s3$left[l+1])))
          
          if(leftaux < i2[i,1]){
            s2$left[i+1] <- i2[i,1]
            s3$left[l+1] <- i2[i,1]
          }
          
          if(leftaux > i2[i,2]){
            s2$left[i+1] <- i2[i,2]
            s3$left[l+1] <- i2[i,2]
          }
          else{
            s2$left[i+1] <- leftaux
            s3$left[l+1] <- leftaux
          }
          break
        }
        
      }
    }
  }
  
  #asign new quantile values
  
  #med int
  for(i in 2:length(s2$left)){
    v <- quantile(data[s2$left[i-1]:(s2$left[i]-1)], 0.5)
    for(k in (s2$left[i-1]):(s2$left[i]-1)){
      if(v < s2$lowerCB[k]){
        s2$value[i-1] <- s2$lowerCB[k]
      }
      else if(v > s2$upperCB[k]){
        s2$value[i-1] <- s2$upperCB[k]
      }
      else{
        s2$value[i-1]  <- v
      }
    }
  }

  #last
  v <- quantile(data[s2$left[length(s2$left)]:n], 0.5)
  for(k in s2$left[length(s2$left)]:n){
    if(v < s2$lowerCB[k]){
      s2$value[length(s2$left)] <- max(v, s2$lowerCB[k])
    }
    else if(v > s2$upperCB[k]){
      s2$value[length(s2$left)] <- min(v, s2$upperCB[k])
    }
    else{
      s2$value[length(s2$left)]  <- v
    }
  }

  #low int

  for(i in 2:length(s1$left)){
    v <- quantile(data[s1$left[i-1]:(s1$left[i]-1)], 0.25)
    for(k in s1$left[i-1]:(s1$left[i]-1)){
      if(v < s1$lowerCB[k]){
        s1$value[i-1] <- s1$lowerCB[k]
      }
      else if(v > s1$upperCB[k]){
        s1$value[i-1] <- s1$upperCB[k]
      }
      else{
        s1$value[i-1]  <- v
      }
    }
  }

  #last
  v <- quantile(data[s1$left[length(s1$left)]:n], 0.25)
  for(k in s1$left[length(s1$left)]:n){
    if(v < s1$lowerCB[k]){
      s1$value[length(s1$left)] <- s1$lowerCB[k]
    }
    else if(v > s1$upperCB[k]){
      s1$value[length(s1$left)] <- s1$upperCB[k]
    }
    else{
      s1$value[length(s1$left)]  <- v
    }
  }

  # for(i in 1:n){
  #   if(fitted.mqs(s1)[i]>fitted.mqs(s2)[i] && fitted.mqs(s2)[i]>s1$lowerCB[i]){
  #     for(k in 2:length(s1$left)){
  #       if(i>s1$left[k-1] && i<s1$left[k]){
  #         s1$value[k-1] <- fitted.mqs(s2)[i]
  #       }
  #       else{
  #         s1$value[length(s1$left)] <- fitted.mqs(s2)[i]
  #       }
  #     }
  #   }
  # }

  #upp int
  for(i in 2:length(s3$left)){
    v <- quantile(data[s3$left[i-1]:(s3$left[i]-1)], 0.75)
    for(k in s3$left[i-1]:(s3$left[i]-1)){
      if(v < s3$lowerCB[k]){
        s3$value[i-1] <- s3$lowerCB[k]
      }
      else if(v > s3$upperCB[k]){
        s3$value[i-1] <- s3$upperCB[k]
      }
      else{
        s3$value[i-1]  <- v
      }
    }
  }

  #last
  v <- quantile(data[s3$left[length(s3$left)]:n], 0.75)
  for(k in s3$left[length(s3$left)]:n){
    if(v < s3$lowerCB[k]){
      s3$value[length(s3$left)] <- s3$lowerCB[k]
    }
    else if(v > s3$upperCB[k]){
      s3$value[length(s3$left)] <- s3$upperCB[k]
    }
    else{
      s3$value[length(s3$left)]  <- v
    }
  }

  # for(i in 1:n){
  #   if(fitted.mqs(s3)[i]<fitted.mqs(s2)[i] && fitted.mqs(s2)[i]<s3$upperCB[i]){
  #     for(k in 2:length(s3$left)){
  #       if(i>s3$left[k-1] && i<s3$left[k]){
  #         s3$value[k-1] <- fitted.mqs(s2)[i]
  #       }
  #       else{
  #         s3$value[length(s3$left)] <- fitted.mqs(s2)[i]
  #       }
  #     }
  #   }
  # }
  
  return(list(s1,s2,s3))
}