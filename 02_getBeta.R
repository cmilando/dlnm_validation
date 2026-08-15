# so now the trick is
# i need to get back into 
# RR = exp(beta * crossbasis)


## no no there is a pred step

## so you fit BETA with y = exp(beta * crossbasis) in some Poisson solve

## then you get the surface by  making every combination of x and lag
## right so the surface you have is basically cumfit
## and instead of going from cumfit = exp(XpredAll * beta)
## you have cumfit and Xpredall and you want to get beta

## because if you have beta true, you can assess how well / what the
## 1stage, 2stage, SB and INLA are doing 

## where do population size and confidence intervals come in? 
## the confidence intervals are 


## so working backwards from that if you have the surface
## and you have basis var and 