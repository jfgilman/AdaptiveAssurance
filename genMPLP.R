


# for generating modulated power law failure times
genMPLP <- function(eta, phi, kappa, numFail=NULL, totTime=NULL, withCen=T){

  if(!is.null(numFail)){
    times <- rep(0, numFail)
    times[1] <- eta*rgamma(1, kappa, 1)^(1/phi)
    for(i in 2:numFail){
      times[i] <- eta*((times[i-1]/eta)^phi + rgamma(1, kappa, 1))^(1/phi)
    }
    return(times)
  }else if(!is.null(totTime)){
    times <- c()
    times[1] <- eta*rgamma(1, kappa, 1)^(1/phi)
    if(times[1] > totTime){
      print("No Failures")
      return(NULL)
    }else{
      while(tail(times, n=1) < totTime){
        times <- append(times, eta*((tail(times, n=1)/eta)^phi + rgamma(1, kappa, 1))^(1/phi))
      }
      if(withCen){
        return(c(head(times, -1), totTime))
      }else{
        return(head(times, -1)) 
      }
    }
  }else{
    print("No number of failures or total time included")
    return(NULL)
  }
}

# genMPLP(2, .5, .5, 10)
# 
# genMPLP(2, .5, .5,totTime=30)