#  Likelihood 2/5/2019 in class

Lbin_11_7 <-  function(p){
  p^7 * (1-p)^4
}
curve(Lbin_11_7, from = 0, to = 1)

Lbin_11_7 <- function(p){
  p^7 * (1-p)^4
}

#  Homework 3

#  squate of the derivative
DerivL <- function(p){
  ((7-11*p)/(p*(1-p)))^2
}

DerivL(0.63635) # check for ~0 squared derivative

curve(DerivL, from = 0.1, to = 0.9)

var(DerivL(seq(from = 0.1, to = 0.9, by = 0.001)))
# large variance of slopes is 529350

DerivL(seq(from = 0.1, to = 0.9, by = 0.001))
mean(DerivL(seq(from = 0.1, to = 0.9, by = 0.001)))
# mean (average) squared slope is 417.4362


