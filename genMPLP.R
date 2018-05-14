


# for generating modulated power law failure times
genMLPL <- function(numFail, eta, phi, kappa){
  times <- rep(0, numFail)
  times[1] <- eta*rgamma(1, kappa, 1)^(1/phi)
  for(i in 2:numFail){
    times[i] <- eta*((times[i-1]/eta)^phi + rgamma(1, kappa, 1))^(1/phi)
  }
  return(times)
}

# genMLPL(10, 2, .5, .5)