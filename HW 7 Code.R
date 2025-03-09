pois.prob <- function(x, size, prob, type="<="){
  # Use dpois and ppois to conditionally return the correct probability
  #dpois is for PMF
  #ppois is for CDF
  
  #cdf of anything negative should just be zero
  if (x-1 < 0){
    sub <- 0
  }
  else{
    sub=prob-1
  }
  
  #prob that x=__ is just the pmf(__)
  equal <- dpois(x,prob)
  #prob that x!=__ is 1-pmf(__)
  not.equal <- 1- dpois(x,prob)
  #prob that x<__ is cdf(__ - 1)
  less <- ppois(sub,prob,TRUE)
  #prob that x <= __ is cdf(__)
  less.or.equal <- ppois(x,prob,TRUE)
  #prob that x> __ is 1-cdf(__)
  greater <- 1-ppois(x,prob,TRUE)
  #prob that x>= __ is 1-cdf(__-1)
  greater.or.equal <- 1-ppois(x,sub,TRUE)
  
  to.return <- data.frame(c("Equal", "Not Equal", "Less Than", "Less than or Equal to",
                            "Greater Than", "Greater Than or Equal to"),
                          c(equal,not.equal,less,less.or.equal,greater,greater.or.equal))
  return(to.return)
}
