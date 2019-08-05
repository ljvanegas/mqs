
#include <Rcpp.h>

using namespace Rcpp;

//swap two indices in integer vector
void Intswap(IntegerVector& V, int i, int j){
  int aux = V[i];
  V[i] = V[j];
  V[j] = aux;
}

//swap two indices in numeric vector
void Numswap(NumericVector& V, int i, int j){
  double aux = V[i];
  V[i] = V[j];
  V[j] = aux;
}

//Use to go from leaf to root in treeSmall
NumericVector btrL(NumericVector& Varr, IntegerVector& Vidx, IntegerVector& VwhichPos, IntegerVector& VwhichTree, 
                   int n, int i)
{
  int parent = (i+1)/2-1;
  
  while(parent >= 0){
    if(Varr[i] > Varr[parent]){
      Numswap(Varr,i,parent);
      Intswap(Vidx,i,parent);
      Intswap(VwhichPos,Vidx[i],Vidx[parent]);
      Intswap(VwhichTree,Vidx[i],Vidx[parent]);
      i = parent;
      parent = (i+1)/2-1;
    }
    else{
      break;
    }
  }
  return Varr;
}

//Use to go from root to leaf in treeLarge
NumericVector rtbL(NumericVector& Varr, IntegerVector& Vidx, IntegerVector& VwhichPos, IntegerVector& VwhichTree, 
                   int n, int i)
{
  int right, left;
  right = 2*i+1;
  left = 2*i+2;
  
  while(right < n){
    if(left < n){
      if(right < n){
        if(Varr[left] > Varr[i]){
          if(Varr[right] > Varr[i]){
            if(Varr[right] > Varr[left]){
              Numswap(Varr,i,right);
              Intswap(Vidx,i,right);
              Intswap(VwhichPos,Vidx[i],Vidx[right]);
              Intswap(VwhichTree,Vidx[i],Vidx[right]);
              i = right;
              right = 2*i+1;
              left = 2*i+2;
            }
            else{
              Numswap(Varr,i,left);
              Intswap(Vidx,i,left);
              Intswap(VwhichPos,Vidx[i],Vidx[left]);
              Intswap(VwhichTree,Vidx[i],Vidx[left]);
              i = left;
              right = 2*i+1;
              left = 2*i+2;
            }
          }
          else{
            Numswap(Varr,i,left);
            Intswap(Vidx,i,left);
            Intswap(VwhichPos,Vidx[i],Vidx[left]);
            Intswap(VwhichTree,Vidx[i],Vidx[left]);
            i = left;
            right = 2*i+1;
            left = 2*i+2;
          }
        }
        else if(Varr[right] > Varr[i]){
          Numswap(Varr,i,right);
          Intswap(Vidx,i,right);
          Intswap(VwhichPos,Vidx[i],Vidx[right]);
          Intswap(VwhichTree,Vidx[i],Vidx[right]);
          i = right;
          right = 2*i+1;
          left = 2*i+2;
        }
        else{
          break;
        }
      }
      else{
        if(Varr[left] > Varr[i]){
          Numswap(Varr,i,left);
          Intswap(Vidx,i,left);
          Intswap(VwhichPos,Vidx[i],Vidx[left]);
          Intswap(VwhichTree,Vidx[i],Vidx[left]);
          i = left;
          right = 2*i+1;
          left = 2*i+2;
        }
        else{
          break;
        }
      }
    }
    else{
      if(Varr[right] > Varr[i]){
        Numswap(Varr,i,right);
        Intswap(Vidx,i,right);
        Intswap(VwhichPos,Vidx[i],Vidx[right]);
        Intswap(VwhichTree,Vidx[i],Vidx[right]);
        i = right;
        right = 2*i+1;
        left = 2*i+2;
      }
      else{
        break;
      }
    }
  }
  return Varr;
}

//Use to go from leaf to root in treeLarge
NumericVector btrS(NumericVector& Varr, IntegerVector& Vidx, IntegerVector& VwhichPos, IntegerVector& VwhichTree, 
                   int n, int i)
{
  int parent = (i+1)/2-1;
  
  while(parent >= 0){
    if(Varr[i] < Varr[parent]){
      Numswap(Varr,i,parent);
      Intswap(Vidx,i,parent);
      Intswap(VwhichPos,Vidx[i],Vidx[parent]);
      Intswap(VwhichTree,Vidx[i],Vidx[parent]);
      i = parent;
      parent = (i+1)/2-1;
    }
    else{
      break;
    }
  }
  return Varr;
}

//Use to go from toot to leaf in treelarge
NumericVector rtbS(NumericVector& Varr, IntegerVector& Vidx, IntegerVector& VwhichPos, IntegerVector& VwhichTree, 
                   int n, int i)
{
  int right, left;
  right = 2*i+1;
  left = 2*i+2;
  
  while(right < n){
    if(left < n){
      if(right < n){
        if(Varr[left] < Varr[i]){
          if(Varr[right] < Varr[i]){
            if(Varr[right] < Varr[left]){
              Numswap(Varr,i,right);
              Intswap(Vidx,i,right);
              Intswap(VwhichPos,Vidx[i],Vidx[right]);
              Intswap(VwhichTree,Vidx[i],Vidx[right]);
              i = right;
              right = 2*i+1;
              left = 2*i+2;
            }
            else{
              Numswap(Varr,i,left);
              Intswap(Vidx,i,left);
              Intswap(VwhichPos,Vidx[i],Vidx[left]);
              Intswap(VwhichTree,Vidx[i],Vidx[left]);
              i = left;
              right = 2*i+1;
              left = 2*i+2;
            }
          }
          else{
            Numswap(Varr,i,left);
            Intswap(Vidx,i,left);
            Intswap(VwhichPos,Vidx[i],Vidx[left]);
            Intswap(VwhichTree,Vidx[i],Vidx[left]);
            i = left;
            right = 2*i+1;
            left = 2*i+2;
          }
        }
        else if(Varr[right] < Varr[i]){
          Numswap(Varr,i,right);
          Intswap(Vidx,i,right);
          Intswap(VwhichPos,Vidx[i],Vidx[right]);
          Intswap(VwhichTree,Vidx[i],Vidx[right]);
          i = right;
          right = 2*i+1;
          left = 2*i+2;
        }
        
        else{
          break;
        }
      }
      else{
        if(Varr[left] < Varr[i]){
          Numswap(Varr,i,left);
          Intswap(Vidx,i,left);
          Intswap(VwhichPos,Vidx[i],Vidx[left]);
          Intswap(VwhichTree,Vidx[i],Vidx[left]);
          i = left;
          right = 2*i+1;
          left = 2*i+2;
        }
        else{
          break;
        }
      }
    }
    else{
      if(Varr[right] < Varr[i]){
        Numswap(Varr,i,right);
        Intswap(Vidx,i,right);
        Intswap(VwhichPos,Vidx[i],Vidx[right]);
        Intswap(VwhichTree,Vidx[i],Vidx[right]);
        i = right;
        right = 2*i+1;
        left = 2*i+2;
      }
      else{
        break;
      }
    }
  }
  return Varr;
}

//add new element x to the double heap, [length] is the length of the interval without the new element
void addonestep(NumericVector& Vtreesmall, IntegerVector& Vidxsmall, NumericVector& Vtreelarge, 
                IntegerVector& Vidxlarge, IntegerVector& VwhichPos, IntegerVector& VwhichTree, 
                IntegerVector& nS, IntegerVector& nL, double x, int idx, double q , int length) {

  if((1-q)*(nS[length]) > q*nL[length]){
    
    int nLl = nL[length];
    
    Vtreelarge[nLl] = x;
    Vidxlarge[nLl] = idx;
    VwhichPos[idx] = nL[length];
    if(length == 0){
      VwhichTree[idx] = 0;
      nL[1] = 1;
      nS[1] = 1;
    }
    else{
      VwhichTree[idx] = 1;
      nL[length+1] = nL[length]+1;
      nS[length+1] = nS[length];
    }
    
    btrS(Vtreelarge, Vidxlarge, VwhichPos, VwhichTree, nL[length+1], nL[length]);
    if(Vtreelarge[0] != Vtreesmall[0]){
      Vtreesmall[0] = Vtreelarge[0];
      Vidxsmall[0] = Vidxlarge[0];
      rtbL(Vtreesmall, Vidxsmall, VwhichPos, VwhichTree, nS[length+1], 0);
      
      
      Vtreelarge[0] = Vtreesmall[0];
      Vidxlarge[0] = Vidxsmall[0];
    }
    
  }
  else{
    int nSl = nS[length];
    Vtreesmall[nSl] = x;
    Vidxsmall[nSl] = idx;
    VwhichPos[idx] = nS[length];
    VwhichTree[idx] = -1;
    
    if(length == 0){
      VwhichTree[idx] = 0;
      nL[1] = 1;
      nS[1] = 1;
    }
    else{
      VwhichTree[idx] = -1;
      nS[length+1] = nS[length]+1;
      nL[length+1] = nL[length];
    }
    
    btrL(Vtreesmall, Vidxsmall, VwhichPos, VwhichTree, nS[length+1], nS[length]);
    
    
    if(Vtreesmall[0] != Vtreelarge[0]){
      Vtreelarge[0] = Vtreesmall[0];
      Vidxlarge[0] = Vidxsmall[0];
      
      rtbS(Vtreelarge, Vidxlarge, VwhichPos, VwhichTree, nL[length+1], 0);
      
      Vtreesmall[0] = Vtreelarge[0];
      Vidxsmall[0] = Vidxlarge[0];
    }
  }
  
  
  
}

//Naive count runs
int countruns(IntegerVector y, int k, int l){
  int count = 0, i;
  
  for(i = k; i < l; i++){
    if(y[i] == 0){
      if(y[i+1] == 1)
        ++count;
    }
    else if(y[i] == -1){
      if(y[i+1] == 1){
        ++count;
      }
    }
    else if(y[i] !=y [i+1]){
      ++count;
    }
  }
  return count;
}

//Count runs only on a region [check]
int checkruns(IntegerVector y, IntegerVector check){
  int k, max, min = 0, count = 0;
  
  for(k = 0; k < check.length()-1; ++k){
    if((check[k]+1) != check[k+1]){
      max = k;
      count = count+countruns(y, check[min], check[max]);
      min = k+1; 
    }
  }
  return count;
}

// erase the elements of a vector that are not within a range [low, high]
void inRange(IntegerVector& x, double low, double high) {
  while(x[0] < low){
    x.erase(0);
  }
  while(x[x.length()-1] > high){
    x.erase(x.length()-1);
  }
  
}

// Counts the new runs after adding a new observation
int add_runs(NumericVector VtreeSmall, IntegerVector Vidxsmall, IntegerVector VwhichTree, IntegerVector nS, 
             IntegerVector nL, double Yold, int old, double q , int j, int i, int runs){
  int length = i-j+1;
  int countA = 0, countN = 0;
  double v1 = 0, v2;
  IntegerVector check(8);
  
  check = IntegerVector::create(Vidxsmall[0]-1, Vidxsmall[0], Vidxsmall[0]+1, old-1, old, 
                              old+1, j,j+1);
  check = sort_unique(check);
  inRange(check, j, i);
  
  countN = checkruns(VwhichTree, check);

  if(Yold > VtreeSmall[0]){
    v1 = VwhichTree[old];
    VwhichTree[old] = -1;
  }
  v2 = VwhichTree[j];
  
  if((1-q)*(nS[length-1]) > q*nL[length-1]){
    VwhichTree[j] = 1;
    if(VwhichTree[j+1] != 1){
      runs = runs+1;
    }
  }
  else{
    VwhichTree[j] = -1;
    if(VwhichTree[j+1] == 1){
      runs = runs+1;
    }
  }
  
  countA = checkruns(VwhichTree, check);
  
  if(Yold > VtreeSmall[0]){
    VwhichTree[old] = v1;
  }
  VwhichTree[j] = v2;
  
  return runs+countN-countA;
}

//penalization
double pens(int n, int lens, double q){
  return  q + sqrt(2 + 2*log(n/double(lens)));
}

//auxiliary function for log likelihood ratio
double f(double b,double x){
  return x*log(x/b)+(1-x)*log((1-x)/(1-b));
}

//solve the above fuction for a given level a (newton method)
double soleq(double b, double a, int l) {
  const double acc = 1e-5;
  double diff, x1, x0;
  diff = 1;
  if(l == 0){
    if(a >= log(1/(1-b))){
      return -1;
    }
    x0 = b;
    while(diff > acc) {
      x1 = (a+log(1-b))*x0/(f(b,x0)+log(1-b));
      diff = std::abs(x0-x1);
      x0 = x1;
    }
  }
  else{
    if(a >= log(1/b)){
      return -1;
    }
    x0 = b;
    while(diff > acc) {
      x1 = (a+log(b))*(x0-1)/(f(b,x0)+log(b))+1;
      diff = std::abs(x1-x0);
      x0 = x1;
    }
  }
  
  return x1;
}

// returns a vector of lower bounds for all lengths j-i+1
NumericVector lowQ(int n, double q, double b) {
  int i;
  double a;
  
  NumericVector lowQ(n+1);
  
  //find all lower bounds
  for(i=1; i<n+1; ++i){
    a = pow(pens(n, i, q),2)/(2*i);
    
    lowQ[i] = soleq(b,a,0);
    if(lowQ[i] == -1){
      lowQ[i] = 0;
    }
  }
  
  lowQ[0] = 0;
  
  return(lowQ);
}

//same as above but for the upper bounds
NumericVector uppQ(int n, double q, double b) {
  int i;
  double a;
  
  NumericVector uppQ(n+1);
  
  //find all upper bounds
  for(i=1; i<n+1; ++i){
    a=pow(pens(n, i, q),2)/(2*i);
    
    uppQ[i] = soleq(b,a,1);
    if(uppQ[i] == -1){
      uppQ[i] = 1;
    }
  }
  
  uppQ[0] = 1;
  
  return(uppQ);
}

double statistic(int n, double b) {
  int i,j;
  double sum, m, localT, T;
  T=-INFINITY;
  
  NumericVector ber = rbinom(n,1,b);
  
  for (i = 0; i < n; ++i) {
    sum = 0;
    for (j = i; j > 0; --j) {
      sum=ber[j]+sum;
      m=sum/(i-j+1);
      localT=(i-j+1)*(m*log(m/b)+(1-m)*log((1-m)/(1-b)));
      T=std::max(T, sqrt(2*localT)-sqrt(2 + 2*log(n/double(i-j+1))));
    }
  }
  
  return T;
}

// [[Rcpp::export]]
NumericVector MCmqs(int N, int n, double beta){
  NumericVector path(N);
  int i;
  for(i=0;i<N; i++){
    path[i]=statistic(n,beta);
  }
  return path;
}

//main function
List mqs(NumericVector Y, double beta = 0.5, double q = 0, std::string type = "runs"){
  
  int n = Y.length();
  double q_aux;
  
  q_aux = q;
  
  int i,j,nseg, lstar, trackLow = -1, trackUpp = -1, idxLow = -1, idxUpp = -1, s, u, cnt, 
    runs;
  double low,upp, lbound, ubound, mu, sigma2, aux_c;
  double const pi = 3.141592;
  
  NumericVector lQ = lowQ(n,q_aux,beta);
  NumericVector uQ = uppQ(n,q_aux,beta);
  
  IntegerVector R(n+1);       // right bound of jump location
  IntegerVector L(n+1);       // left bound of jump location
  NumericVector loB(n+1);     // auxilary memory for lower bounds
  NumericVector upB(n+1);     // auxilary memory for upper bounds
  IntegerVector U(n+1);       // saves total number of runs
  IntegerVector S(n+1);       // saves total number of 1 in the transformed vector
  NumericVector qu(n);        // optimal quantile 
  NumericVector C(n+1);       // optimal cost
  IntegerVector leE(n);       // leftmost end points
  NumericVector aux_qu(n+1);  // auxiliary memory for quantiles
  IntegerVector aux_idx(n+1); // auxiliary memory for quantile index
  IntegerVector aux_pos(n+1); // auxiliary memory for relative position of the quantile
  //Koenker
  NumericVector sum(n+1);     // cumulative sum
  NumericVector aux_sum(n+1); // aux sum
  
  if(type == "koenker"){
    sum[0] = 0;
    for (i = 0; i < n; ++i)
      sum[i+1] = sum[i] + Y[i];
  }
  // initialization
  nseg = 0;
  R[0] = 0;
  R[1] = 0;
  C[0] = 0;
  L[0] = 0;
  
  for (i = 0; i < n+1; ++i)
    upB[i] = INFINITY;
  for (i = 0; i < n+1; ++i)
    loB[i] = -INFINITY;
  for (i = 0; i < n+1; ++i)
    S[i] = 0;
  for (i = 0; i < n+1; ++i)
    U[i] = 0;
  for (i = 0; i < n; ++i)
    C[i+1] = INFINITY;
  
  
  //Initial upper trees
  NumericVector U_lowtrees(n);
  IntegerVector U_idxlow(n);
  NumericVector U_upptrees(n);
  IntegerVector U_idxupp(n);
  IntegerVector U_whichPos(n);
  IntegerVector U_whichTree(n);
  
  IntegerVector U_nS(n+1); //number of points in the lower tree (small)
  IntegerVector U_nL(n+1); //number of points in the upper tree (large)
  for (i = 0; i < n+1; ++i)
    U_nS[i] = 0;
  for (i = 0; i < n+1; ++i)
    U_nL[i] = 0;
  
  //Initial lower trees
  NumericVector L_lowtrees(n);
  IntegerVector L_idxlow(n);
  NumericVector L_upptrees(n);
  IntegerVector L_idxupp(n);
  IntegerVector L_whichPos(n);
  IntegerVector L_whichTree(n);
  
  IntegerVector L_nS(n+1);
  IntegerVector L_nL(n+1);
  for (i = 0; i < n+1; ++i)
    L_nS[i] = 0;
  for (i = 0; i < n+1; ++i)
    L_nL[i] = 0;
  
  //Initial b trees
  NumericVector b_lowtrees(n);
  IntegerVector b_idxlow(n);
  NumericVector b_upptrees(n);
  IntegerVector b_idxupp(n);
  IntegerVector b_whichPos(n);
  IntegerVector b_whichTree(n);
  
  IntegerVector b_nS(n+1);
  IntegerVector b_nL(n+1);
  for (i = 0; i < n+1; ++i)
    b_nS[i] = 0;
  for (i = 0; i < n+1; ++i)
    b_nL[i] = 0;


  while (std::isinf(C[n])) {

    ++nseg; //number segment 
    lstar = R[nseg-1]; 
    
    for(i = R[nseg]; i < n; ++i){
      
      runs = 0;

      //look all the intervals without adding a cp
      for(j = i; j > R[nseg]; --j){
        
        upp = uQ[i-j+1];
        low = lQ[i-j+1];
        

        addonestep(U_lowtrees, U_idxlow, U_upptrees, U_idxupp,U_whichPos, U_whichTree, U_nS,U_nL, Y[j],j, upp, i-j);
        addonestep(L_lowtrees, L_idxlow, L_upptrees, L_idxupp,L_whichPos, L_whichTree, L_nS,L_nL, Y[j],j, low, i-j);
        addonestep(b_lowtrees, b_idxlow, b_upptrees, b_idxupp,b_whichPos, b_whichTree, b_nS,b_nL, Y[j],j, beta, i-j);
        
        //update the bounds and track the locations
        ubound = U_lowtrees[0];
        lbound = L_lowtrees[0];
        
        if(upp < 1){
          upB[j] = fmin(fmin(upB[j], ubound), upB[j+1]);
          if(upB[j] == ubound){ //if the bound is attained save:
            trackUpp = U_nS[i-j+1]-1; //relative postion (order in interval [j,i])
            idxUpp = j; //index
          }
          if(upB[j] == upB[j+1]){
            if(Y[j] > upB[j]){
              ++trackUpp;
            }
          }
          else{
            if(Y[i] > upB[j]){
              ++trackUpp;
            }
          }
        }
        
        if(low > 0){
          loB[j] = fmax(fmax(loB[j], lbound), loB[j+1]);
          if(loB[j] == lbound){
            trackLow = L_nS[i-j+1]-1;
            idxLow = j;
          }
          if(loB[j] == loB[j+1]){
            if(Y[j] > loB[j]){
              ++trackLow;
            }
          }
          else{
            if(Y[i] > loB[j]){
              ++trackLow;
            }
          }
        }
        
        //update the quantiles and save indices
        aux_qu[j] = b_lowtrees[0];
        aux_idx[j] = b_idxlow[0];
        aux_pos[j] = b_nS[i-j+1]-1;
        
        if(aux_qu[j] > upB[j]){
          aux_qu[j] = upB[j];
          aux_idx[j] = idxUpp;
          aux_pos[j] = trackUpp;
        }
        else if(aux_qu[j] < loB[j]){
          aux_qu[j] = loB[j];
          aux_idx[j] = idxLow;
          aux_pos[j] = trackLow;
        }
        
        //update the runs
        if(j != i){
          runs = add_runs(b_lowtrees, b_idxlow, b_whichTree, b_nS, b_nL, aux_qu[j+1], aux_idx[j+1], beta, j, i, runs);
        }
        
        //only Koenker
        if(type == "koenker"){
          aux_sum[j] = aux_sum[j+1];
          if(i!=j){
            if(Y[j] < aux_qu[j])
              aux_sum[j] = aux_sum[j]+Y[j];
            
            if(aux_qu[j] > aux_qu[j+1]){
              aux_sum[j] = aux_sum[j]+aux_qu[j+1];
            }
            
            if(aux_qu[j] < aux_qu[j+1] && Y[j] != aux_qu[j]){
              aux_sum[j] = aux_sum[j]-aux_qu[j];
            }
          }
        }
      }
      // here may have to add a c.p.

      for(j = R[nseg]; j >= lstar; --j){

        upp = uQ[i-j+1];
        low = lQ[i-j+1];
        
        
        addonestep(U_lowtrees, U_idxlow, U_upptrees, U_idxupp,U_whichPos, U_whichTree, U_nS,U_nL, Y[j],j, upp, i-j);
        addonestep(L_lowtrees, L_idxlow, L_upptrees, L_idxupp,L_whichPos, L_whichTree, L_nS,L_nL, Y[j],j, low, i-j);
        addonestep(b_lowtrees, b_idxlow, b_upptrees, b_idxupp,b_whichPos, b_whichTree, b_nS,b_nL, Y[j],j, beta, i-j);
        
        //update the betaounds and track the locations
        ubound = U_lowtrees[0];
        lbound = L_lowtrees[0];
        
        if(upp < 1){
          upB[j] = fmin(fmin(upB[j], ubound), upB[j+1]);
          if(upB[j] == ubound){
            trackUpp = U_nS[i-j+1]-1;
            idxUpp = j;
          }
          if(upB[j] == upB[j+1]){
            if(Y[j] > upB[j]){
              ++trackUpp;
            }
          }
          else{
            if(Y[i] > upB[j]){
              ++trackUpp;
            }
          }
        }
        
        if(low > 0){
          loB[j] = fmax(fmax(loB[j], lbound), loB[j+1]);
          if(loB[j] == lbound){
            trackLow = L_nS[i-j+1]-1;
            idxLow = j;
          }
          if(loB[j] == loB[j+1]){
            if(Y[j] > loB[j]){
              ++trackLow;
            }
          }
          else{
            if(Y[i] > loB[j]){
              ++trackLow;
            }
          }
        }
        
        //update the quantiles and save indices
        aux_qu[j] = b_lowtrees[0];
        aux_idx[j] = b_idxlow[0];
        aux_pos[j] = b_nS[i-j+1]-1;
        
        if(aux_qu[j] > upB[j]){
          aux_qu[j] = upB[j];
          aux_idx[j] = idxUpp;
          aux_pos[j] = trackUpp;
        }
        else if(aux_qu[j] < loB[j]){
          aux_qu[j] = loB[j];
          aux_idx[j] = idxLow;
          aux_pos[j] = trackLow;
        }
        
        //update the runs
        if(j != i){
          runs = add_runs(b_lowtrees, b_idxlow, b_whichTree, b_nS, b_nL, aux_qu[j+1], aux_idx[j+1], beta, j, i, runs);
        }
        
        //only koenker
        if(type == "koenker"){
          aux_sum[j] = aux_sum[j+1];
          if(i!=j){
            if(Y[j] < aux_qu[j])
              aux_sum[j] = aux_sum[j]+Y[j];
            
            if(aux_qu[j] > aux_qu[j+1]){
              aux_sum[j] = aux_sum[j]+aux_qu[j+1];
            }
            
            if(aux_qu[j] < aux_qu[j+1] && Y[j] != aux_qu[j]){
              aux_sum[j] = aux_sum[j]-aux_qu[j];
            }
          }
        }
        
        //if the we don't add a c.p.
        if(loB[j] <= upB[j]){ //check if the interval is empty
          
          s = aux_pos[j]+S[j];
          

          if(j > 0){
            u = runs+U[j]+((Y[j-1] > qu[j-1]) == (Y[j] > aux_qu[j]));
          }
          else{
            u = runs+U[j];
          }
          
          //Koenker 
          if(type == "koenker"){
            aux_c = beta*(sum[i+1]-sum[j]-(i-j+1)*aux_qu[j])-aux_sum[j]+aux_qu[j]*(aux_pos[j])+C[j];
            //printf("i %d j %d 1 %f auxsum %f othersum %f aux_c %f \n",i,j,beta*(sum[i+1]-sum[j]-(i-j+1)*aux_qu[j]), aux_sum[j], aux_qu[j]*(aux_pos[j]), aux_c);
            
            if (aux_c < C[i+1]) {
              
              C[i+1] = aux_c;
              qu[i] = aux_qu[j];
              leE[i] = j + 1;
            }
          }
          else{
            mu = 1+(2*(s)*(i-s+1))/double(i+1);

            sigma2 = (mu-1)*(mu-2)/(i+1);
            if(mu < 3){
              aux_c = - R::dbinom(s, i+1, beta,1);
            }else aux_c = log(sqrt(2*pi*sigma2))+pow((u+1-mu), 2)/sigma2-(R::dbinom(s, i+1, beta, 1)); //update the cost
            //if(i == n-1){
            //printf("i %d j %d s %d u %d aux_c %f\n ",i,j, s, u, aux_c);
            //}
          
            if (aux_c < C[i+1]) {
            
              C[i+1] = aux_c;

              S[i] = s;
              U[i] = u;
              qu[i] = aux_qu[j];
              leE[i] = j + 1;
            }
          }
          
        }
        else{
          lstar = j + 1;

          break;
        }
        
      }
      
      if (i == R[nseg]){
        L[nseg] = j + 1 + (loB[j] > upB[j]);
        
      }
      if (std::isinf(C[i+1]))
        break;
    }

    R[nseg+1] = i;
    L[nseg+1] = R[nseg];
  }
  NumericVector value(nseg);
  IntegerVector left(nseg);
  IntegerMatrix confInt(nseg-1,2);
  
  cnt = n-1;
  for (i = nseg-1; i >= 0; --i) {
    left[i] = leE[cnt];
    value[i] = qu[cnt];
    if(i != nseg-1){
    confInt(i,0) = L[i+2]+1;
    confInt(i,1) = R[i+2]+1;
    }
    cnt = leE[cnt] - 2;
  }
  
  
  List ans = List::create(Named("value") = value, Named("left") = left, Named("n") = n, Named("confInt")=confInt);
  ans.attr("class") = "mqs";
  
  
  //Koenker
  // if(type = "koenker"){
  //   NumericVector value_k(nseg);
  //   IntegerVector left_k(nseg);
  // 
  //   cnt = n-1;
  //   for (i = nseg-1; i >= 0; --i) {
  //     left_k[i] = leE_k[cnt];
  //     value_k[i] = qu_k[cnt];
  //     cnt = leE[cnt] - 2;
  //   }
  //   
  //   ans.push_back(value_k);
  //   ans.push_back(left_k);
  // }
  // 
  return ans;
}

List mqsConf(NumericVector Y, double beta = 0.5, double q = 0, std::string type = "runs"){
  
  int n = Y.length();
  
  NumericVector Yrev = clone(Y);
  std::reverse(Yrev.begin(), Yrev.end());
  List ansR = mqs(Yrev, beta, q, type);
  IntegerMatrix confInt = ansR[3];
  
  NumericMatrix valConfB(confInt.nrow()*2+2,2);
  
  for(int i = 0; i<confInt.length(); i++){
    confInt[i] = n-confInt[i];
  }
  
  std::reverse(confInt.begin(), confInt.end());

  int i,j,nseg, lstar, trackLow = -1, trackUpp = -1, idxLow = -1, idxUpp = -1, s, u, cnt, 
    runs;
  int revB, B;
  double low,upp, lbound, ubound, mu, sigma2, aux_c;
  double const pi = 3.141592;
  
  NumericVector lQ = lowQ(n,q,beta);
  NumericVector uQ = uppQ(n,q,beta);
  
  IntegerVector R(n+1);       // right bound of jump location
  IntegerVector L(n+1);       // left bound of jump location
  NumericVector loB(n+1);     // auxilary memory for lower bounds
  NumericVector upB(n+1);     // auxilary memory for upper bounds
  IntegerVector U(n+1);       // saves total number of runs
  IntegerVector S(n+1);       // saves total number of 1 in the transformed vector
  NumericVector qu(n);        // optimal quantile 
  NumericVector C(n+1);       // optimal cost
  IntegerVector leE(n);       // leftmost end points
  NumericVector aux_qu(n+1);  // auxiliary memory for quantiles
  IntegerVector aux_idx(n+1); // auxiliary memory for quantile index
  IntegerVector aux_pos(n+1); // auxiliary memory for relative position of the quantile
  
  NumericVector auxCB_lo(n+1);   // auxilary memory for confidence bands
  NumericVector auxCB_up(n+1);   // auxilary memory for confidence bands
  
  NumericVector auxCB_lo2(n+1);   // auxilary memory for confidence bands
  NumericVector auxCB_up2(n+1);   // auxilary memory for confidence bands
  
  NumericVector loCB(n);   // auxilary memory for confidence bands
  NumericVector upCB(n);   // auxilary memory for confidence bands
  
  NumericVector loCB1(n+1);   // auxilary memory for confidence bands
  NumericVector upCB1(n+1);   // auxilary memory for confidence bands
  
  NumericVector loCB2(n+1);   // auxilary memory for confidence bands
  NumericVector upCB2(n+1);   // auxilary memory for confidence bands
  
  for (i = 0; i < n+1; ++i)
    upCB1[i] = INFINITY;
  for (i = 0; i < n+1; ++i)
    loCB1[i] = -INFINITY;
  
  for (i = 0; i < n+1; ++i)
    upCB2[i] = INFINITY;
  for (i = 0; i < n+1; ++i)
    loCB2[i] = -INFINITY;
  
  //Koenker
  NumericVector sum(n+1);     // cumulative sum
  NumericVector aux_sum(n+1); // aux sum
  
  if(type == "koenker"){
    sum[0] = 0;
    for (i = 0; i < n; ++i)
      sum[i+1] = sum[i] + Y[i];
  }
  // initialization
  nseg = 0;
  R[0] = 0;
  R[1] = 0;
  C[0] = 0;
  L[0] = 0;
  
  for (i = 0; i < n+1; ++i)
    upB[i] = INFINITY;
  for (i = 0; i < n+1; ++i)
    loB[i] = -INFINITY;
  for (i = 0; i < n+1; ++i)
    S[i] = 0;
  for (i = 0; i < n+1; ++i)
    U[i] = 0;
  for (i = 0; i < n; ++i)
    C[i+1] = INFINITY;
  
  
  //Initial upper trees
  NumericVector U_lowtrees(n);
  IntegerVector U_idxlow(n);
  NumericVector U_upptrees(n);
  IntegerVector U_idxupp(n);
  IntegerVector U_whichPos(n);
  IntegerVector U_whichTree(n);
  
  IntegerVector U_nS(n+1);
  IntegerVector U_nL(n+1);
  for (i = 0; i < n+1; ++i)
    U_nS[i] = 0;
  for (i = 0; i < n+1; ++i)
    U_nL[i] = 0;
  
  //Initial lower trees
  NumericVector L_lowtrees(n);
  IntegerVector L_idxlow(n);
  NumericVector L_upptrees(n);
  IntegerVector L_idxupp(n);
  IntegerVector L_whichPos(n);
  IntegerVector L_whichTree(n);
  
  IntegerVector L_nS(n+1);
  IntegerVector L_nL(n+1);
  for (i = 0; i < n+1; ++i)
    L_nS[i] = 0;
  for (i = 0; i < n+1; ++i)
    L_nL[i] = 0;
  
  //Initial b trees
  NumericVector b_lowtrees(n);
  IntegerVector b_idxlow(n);
  NumericVector b_upptrees(n);
  IntegerVector b_idxupp(n);
  IntegerVector b_whichPos(n);
  IntegerVector b_whichTree(n);
  
  IntegerVector b_nS(n+1);
  IntegerVector b_nL(n+1);
  for (i = 0; i < n+1; ++i)
    b_nS[i] = 0;
  for (i = 0; i < n+1; ++i)
    b_nL[i] = 0;
  
  
  while (std::isinf(C[n])) {
    
    ++nseg;
    lstar = R[nseg-1];
    
    for(i = R[nseg]; i < n; ++i){
      
      runs = 0;
      
      //look all the intervals without adding a cp
      for(j = i; j > R[nseg]; --j){
        
        upp = uQ[i-j+1];
        low = lQ[i-j+1];
        
        
        addonestep(U_lowtrees, U_idxlow, U_upptrees, U_idxupp,U_whichPos, U_whichTree, U_nS,U_nL, Y[j],j, upp, i-j);
        addonestep(L_lowtrees, L_idxlow, L_upptrees, L_idxupp,L_whichPos, L_whichTree, L_nS,L_nL, Y[j],j, low, i-j);
        addonestep(b_lowtrees, b_idxlow, b_upptrees, b_idxupp,b_whichPos, b_whichTree, b_nS,b_nL, Y[j],j, beta, i-j);
        
        //update the bounds and track the locations
        ubound = U_lowtrees[0];
        lbound = L_lowtrees[0];
        
        if(upp < 1){
          upB[j] = fmin(fmin(upB[j], ubound), upB[j+1]);
          if(upB[j] == ubound){
            trackUpp = U_nS[i-j+1]-1;
            idxUpp = j;
          }
          if(upB[j] == upB[j+1]){
            if(Y[j] > upB[j]){
              ++trackUpp;
            }
          }
          else{
            if(Y[i] > upB[j]){
              ++trackUpp;
            }
          }
        }
        
        if(low > 0){
          loB[j] = fmax(fmax(loB[j], lbound), loB[j+1]);
          if(loB[j] == lbound){
            trackLow = L_nS[i-j+1]-1;
            idxLow = j;
          }
          if(loB[j] == loB[j+1]){
            if(Y[j] > loB[j]){
              ++trackLow;
            }
          }
          else{
            if(Y[i] > loB[j]){
              ++trackLow;
            }
          }
        }
        
        //update the quantiles and save indices
        aux_qu[j] = b_lowtrees[0];
        aux_idx[j] = b_idxlow[0];
        aux_pos[j] = b_nS[i-j+1]-1;
        
        if(aux_qu[j] > upB[j]){
          aux_qu[j] = upB[j];
          aux_idx[j] = idxUpp;
          aux_pos[j] = trackUpp;
        }
        else if(aux_qu[j] < loB[j]){
          aux_qu[j] = loB[j];
          aux_idx[j] = idxLow;
          aux_pos[j] = trackLow;
        }
        
        //update the runs
        if(j != i){
          runs = add_runs(b_lowtrees, b_idxlow, b_whichTree, b_nS, b_nL, aux_qu[j+1], aux_idx[j+1], beta, j, i, runs);
        }
        
        //only Koenker
        if(type == "koenker"){
          aux_sum[j] = aux_sum[j+1];
          if(i!=j){
            if(Y[j] < aux_qu[j])
              aux_sum[j] = aux_sum[j]+Y[j];
          
            if(aux_qu[j] > aux_qu[j+1]){
              aux_sum[j] = aux_sum[j]+aux_qu[j+1];
            }
          
            if(aux_qu[j] < aux_qu[j+1] && Y[j] != aux_qu[j]){
              aux_sum[j] = aux_sum[j]-aux_qu[j];
            }
          }
        }
      }
      
      auxCB_up[i] = upB[R[nseg]+1];
      auxCB_lo[i] = loB[R[nseg]+1];
      // if(auxCB_lo[i]==-INFINITY){
      //   auxCB_lo[i] =auxCB_lo[i+1];
      // }
      
      // if(auxCB_lo[i] ==-INFINITY){
      //   printf(" i %d\n", i);
      // }
      
      // here may have to add a c.p.
      
      for(j = R[nseg]; j >= lstar; --j){
        
        upp = uQ[i-j+1];
        low = lQ[i-j+1];
        
        
        addonestep(U_lowtrees, U_idxlow, U_upptrees, U_idxupp,U_whichPos, U_whichTree, U_nS,U_nL, Y[j],j, upp, i-j);
        addonestep(L_lowtrees, L_idxlow, L_upptrees, L_idxupp,L_whichPos, L_whichTree, L_nS,L_nL, Y[j],j, low, i-j);
        addonestep(b_lowtrees, b_idxlow, b_upptrees, b_idxupp,b_whichPos, b_whichTree, b_nS,b_nL, Y[j],j, beta, i-j);
        
        //update the betaounds and track the locations
        ubound = U_lowtrees[0];
        lbound = L_lowtrees[0];
        
        if(upp < 1){
          upB[j] = fmin(fmin(upB[j], ubound), upB[j+1]);
          if(upB[j] == ubound){
            trackUpp = U_nS[i-j+1]-1;
            idxUpp = j;
          }
          if(upB[j] == upB[j+1]){
            if(Y[j] > upB[j]){
              ++trackUpp;
            }
          }
          else{
            if(Y[i] > upB[j]){
              ++trackUpp;
            }
          }
        }
        
        if(low > 0){
          loB[j] = fmax(fmax(loB[j], lbound), loB[j+1]);
          if(loB[j] == lbound){
            trackLow = L_nS[i-j+1]-1;
            idxLow = j;
          }
          if(loB[j] == loB[j+1]){
            if(Y[j] > loB[j]){
              ++trackLow;
            }
          }
          else{
            if(Y[i] > loB[j]){
              ++trackLow;
            }
          }
        }
        
        //update the quantiles and save indices
        aux_qu[j] = b_lowtrees[0];
        aux_idx[j] = b_idxlow[0];
        aux_pos[j] = b_nS[i-j+1]-1;
        
        if(aux_qu[j] > upB[j]){
          aux_qu[j] = upB[j];
          aux_idx[j] = idxUpp;
          aux_pos[j] = trackUpp;
        }
        else if(aux_qu[j] < loB[j]){
          aux_qu[j] = loB[j];
          aux_idx[j] = idxLow;
          aux_pos[j] = trackLow;
        }
        
        //update the runs
        if(j != i){
          runs = add_runs(b_lowtrees, b_idxlow, b_whichTree, b_nS, b_nL, aux_qu[j+1], aux_idx[j+1], beta, j, i, runs);
        }
        
        //only Koenker
        if(type == "koenker"){
          aux_sum[j] = aux_sum[j+1];
          if(i!=j){
            if(Y[j] < aux_qu[j])
              aux_sum[j] = aux_sum[j]+Y[j];
            
            if(aux_qu[j] > aux_qu[j+1]){
              aux_sum[j] = aux_sum[j]+aux_qu[j+1];
            }
            
            if(aux_qu[j] < aux_qu[j+1] && Y[j] != aux_qu[j]){
              aux_sum[j] = aux_sum[j]-aux_qu[j];
            }
          }
        }
        
        //if the we don't add a c.p.
        if(loB[j] <= upB[j]){
          
          s = aux_pos[j]+S[j];
          
          
          if(j > 0){
            u = runs+U[j]+((Y[j-1] > qu[j-1]) == (Y[j] > aux_qu[j]));
          }
          else{
            u = runs+U[j];
          }
          
          //Koenker 
          if(type == "koenker"){
            aux_c = beta*(sum[i+1]-sum[j]-(i-j+1)*aux_qu[j])-aux_sum[j]+aux_qu[j]*(aux_pos[j])+C[j];
            
            if (aux_c < C[i+1]) {
              
              C[i+1] = aux_c;
              qu[i] = aux_qu[j];
              leE[i] = j + 1;
            }
          }
          else{
            mu = 1+(2*(s)*(i-s+1))/double(i+1);
            
            sigma2 = (mu-1)*(mu-2)/(i+1);
            if(mu < 3){
              aux_c = - R::dbinom(s, i+1, beta,1);
            }else aux_c = log(sqrt(2*pi*sigma2))+pow((u+1-mu), 2)/sigma2-(R::dbinom(s, i+1, beta, 1));
            
            if (aux_c < C[i+1]) {
              
              C[i+1] = aux_c;
              
              S[i] = s;
              U[i] = u;
              qu[i] = aux_qu[j];
              leE[i] = j + 1;
            }
          }
          
        }
        else{
          lstar = j + 1;
  
          break;
        }
        
      }
      
      if (i == R[nseg]){
        L[nseg] = j + 1 + (loB[j] > upB[j]);

      }
      if (std::isinf(C[i+1]))
        break;
    }
    
    R[nseg+1] = i;
    L[nseg+1] = R[nseg];
  }
  NumericVector value(nseg);
  IntegerVector left(nseg);
  
  
  cnt = n-1;
  for (i = nseg-1; i >= 0; --i) {
    left[i] = leE[cnt];
    value[i] = qu[cnt];
    cnt = leE[cnt] - 2;
    
    if(i != nseg-1){
      revB = confInt(i,0);
      B = L[i+2]+1;
      confInt(i,0) = std::max( revB, B);
      
     revB = confInt(i,1);
     B = R[i+2]+1;
     confInt(i,1) = std::min( revB, B);
    }
  }
  
  IntegerVector vecBounds(confInt.length()+2);
  
  vecBounds[0]=1;
  vecBounds[confInt.length()+1] = n;
  
  for(i = 1; i<vecBounds.length()/2; i++){
    vecBounds[2*i-1] = confInt(i-1,0);
    vecBounds[2*i] = confInt(i-1,1);
  }
  
  for(i = 0; i < nseg-1; i++){
    for(j = vecBounds[2*i]-1; j < vecBounds[2*i+1]; j++){
      loCB[j] = auxCB_lo[confInt(i,0)];
      upCB[j] = auxCB_up[confInt(i,0)];
      // if(loCB[j] ==-INFINITY){
      //   printf(" j %d i %d auxCB_lo %f conf %d\n", j, i, auxCB_lo[confInt(i,0)], confInt(i,0));
      // }
    }
  }
  
  for(j=vecBounds[vecBounds.length()-2]-1; j<n; j++){
    loCB[j] = auxCB_lo[n-1];
    upCB[j] = auxCB_up[n-1];
  }
  
 //new bounds
  
  int k;
  
  for (k = 0; k < confInt.length()/2-1; k++){
    for(i = confInt(k+1,0)-1; i > confInt(k,0)-1; i--){
      for(j = i; j < confInt(k+1,0); ++j){
        upp = uQ[j-i+1];
        low = lQ[j-i+1];
        
        addonestep(U_lowtrees, U_idxlow, U_upptrees, U_idxupp,U_whichPos, U_whichTree, U_nS,U_nL, Y[j],j, upp, j-i);
        addonestep(L_lowtrees, L_idxlow, L_upptrees, L_idxupp,L_whichPos, L_whichTree, L_nS,L_nL, Y[j],j, low, j-i);
        
        //update the bounds and track the locations
        ubound = U_lowtrees[0];
        lbound = L_lowtrees[0];
        
        if(upp < 1){
          upCB1[j] = fmin(fmin(upCB1[j], ubound), upCB1[j+1]);
        }

        if(low > 0){
          loCB1[j] = fmax(fmax(loCB1[j], lbound), loCB1[j+1]);
        }
        // if(i==1201){
        //   printf("i %d j%d locb1 %f confint 1 %d 1. %d\n", i, j,loCB1[j], confInt(k,0)-1, confInt(k,1)+1);
        // }
    }
      upCB2[i] = upCB1[confInt(k+1,0)-1];
      loCB2[i] = loCB1[confInt(k+1,0)-1];

   }
}
  
//last interval

for(i = n-1; i > confInt(confInt.length()/2-1,0)-1; i--){
  for(j = i; j < n; ++j){
    upp = uQ[j-i+1];
    low = lQ[j-i+1];

    addonestep(U_lowtrees, U_idxlow, U_upptrees, U_idxupp,U_whichPos, U_whichTree, U_nS,U_nL, Y[j],j, upp, j-i);
    addonestep(L_lowtrees, L_idxlow, L_upptrees, L_idxupp,L_whichPos, L_whichTree, L_nS,L_nL, Y[j],j, low, j-i);

    //update the bounds and track the locations
    ubound = U_lowtrees[0];
    lbound = L_lowtrees[0];

    if(upp < 1){
      upCB1[j] = fmin(fmin(upCB1[j], ubound), upCB1[j+1]);

    }

    if(low > 0){
      loCB1[j] = fmax(fmax(loCB1[j], lbound), loCB1[j+1]);
    }
  }
  upCB2[i] = upCB1[n-1];
  loCB2[i] = loCB1[n-1];
}
 

  for(i = 0; i < vecBounds.length()/2-1; i++){
    for(j = vecBounds[2*i+1]; j < vecBounds[2*i+2]-1; j++){
      loCB[j] = fmax(fmin(auxCB_lo[j], loCB2[j]), fmin(loCB[vecBounds[2*i+1]-1],loCB[vecBounds[2*i+2]]));

      if(loCB[j]==-INFINITY){
        loCB[j] = loCB[j-1];
      }
      upCB[j] = fmin(fmax(auxCB_up[j], upCB2[j]), fmax(upCB[vecBounds[2*i+1]-1],upCB[vecBounds[2*i+2]]));
      if(upCB[j]==INFINITY){
        upCB[j] = upCB[j-1];
      }
      // if(loCB[j]==-INFINITY){
      //   printf("j %d 1. %f 2. %f\n", j, auxCB_lo[j], loCB2[j]);
      // }
    }
  }
  
  for(i = vecBounds.length()/2-2; i >=0; i--){
    for(j = vecBounds[2*i+2]; j > vecBounds[2*i+1]-1; j--){
      if(loCB[j]==-INFINITY){
        loCB[j] = loCB[j+1];
      }
      if(upCB[j]==INFINITY){
        upCB[j] = upCB[j+1];
      }
    }
  }
  
  for(i = 0; i < nseg-1; i++){
    for(j = vecBounds[2*i]-1; j < vecBounds[2*i+1]; j++){
      if(loCB[j]==-INFINITY){
        loCB[j] = fmin(loCB[vecBounds[2*i]-2], loCB[vecBounds[2*i+1]+2]);
      }
      if(upCB[j]==INFINITY){
        upCB[j] = fmax(upCB[vecBounds[2*i]-2], upCB[vecBounds[2*i+1]+2]);
      }
    }
  }
  
  List ans = List::create(Named("value") = value, Named("left") = left, Named("n") = n, Named("confInt")=confInt, Named("lowerCB")=loCB,
                                Named("upperCB") =upCB);
  ans.attr("class") = "mqs";

  return ans;
}

// [[Rcpp::export]]
List mqse(NumericVector Y, double beta = 0.5, Rcpp::Nullable<double>  q = R_NilValue, double alpha = 0.05, bool conf = false, std::string type = "runs"){
  int n = Y.length();
  double q_aux;
  
  if(q == R_NilValue){
    NumericVector p(10000);
    if(n>2000){
      Environment env("package:mqs");
      p = env["MCasym"];
    }
    else{p = MCmqs(10000,n,beta);}
    
    int pos = 1001*(1-alpha);
    q_aux = p[pos-1];
  }
  else{
    q_aux = as<double>(q);
  }
  
  if(conf){
    return mqsConf(Y, beta, q_aux, type);
  }
  else{
    return mqs(Y, beta, q_aux, type);
  }
}

