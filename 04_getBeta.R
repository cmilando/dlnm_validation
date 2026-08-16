# so now the trick is
# i need to get back into 
# RR = exp(beta * crossbasis)


## no no there is a pred step

## so you fit BETA with y = exp(beta * crossbasis) in some Poisson solve

## then you get the surface by  making every combination of x and lag
## right so the surface you have is basically cumfit
## and instead of going from cumfit = exp(XpredAll * beta)
## you have cumfit and Xpredall and you want to get beta

## remember that xpredall is made on the minimum set of x and l
## so not the full time-series just the range of each

## so then you have betaTrue. 

## Once you have beta true, you can 
## (1) create the simulated data, right because then you can create
##     y = baseline_deaths * exp(beta * crossbasis) 
##     the is the SUMPRODUCT
## 
##
##     death_updated[i] = round(death_baseline[i] * exp(sum(getlogy(x_mat[i, ]) * ww)))


## Then  you can
## (1) get the confidence intervals
## (2) then assess how well / what the
## 1stage, 2stage, SB and INLA are doing 

