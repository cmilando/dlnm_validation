# you want 3 in a row

# and you want to work from a combined surface
# which i thnk you can get from a lag graph and a temp graph
# and you just need the centerline and then
# the confidence intervals scale with the population

# you'll then want to show the cross reduce object too

# which then means i think you get work 

## I thnk it should start with just two right next to eachother

## and then it starts with analysis is just 1 section

## so RR surface = exp(crossbasis * Beta)
## so we have crossbasis 
## so I think yo ushould be able to define the RR by the product of he

library(ggcube)
library(ggplot2)
library(data.table)

## so what you need to draw is the is the RR function
## and then interpolate what the values are below it
## the trick is getting them to multiply together? 

## eh this isn't exactly right because what i want to draw is the 
## cumulative curves, and whats here is the underlying curves
## you could just say, adjust the underlying curves until the
## cumulative curves look like what you want

## but how are you definining how the two curves multiply in 3d space
## I suppose if assume there is no relationahip at the longest lag
## so maybe you need 3 lines -- that would make the most sense sonny jim
## (1) shape of the cumulative exposure RR (i.e., at max lag)
## (2) shape of the initial exposure RR (i.e., at lag = 0)
## (3) shape of the lag curve between its max and min as a function of days
## and then basically for each exposure level 
##        you have a min (#2), and a max(#1), and the shape of how it
##        goes between each one (#3)

N = 10
x = seq(0, 30, length.out = N)
f_exp_initial = function(x) ifelse(x<20, 1, (x^3.5)/6e5+0.95)
f_exp_cumulative = function(x) ifelse(x<20, 1, (x^4)/6e5+0.75)
plot(x, f_exp_initial(x), type = 'l', col = 'blue', ylim = c(1, 2))
lines(x, f_exp_cumulative(x), type = 'l', col = 'red')

l = seq(0, 5, length.out = N)
f_lag = function(l) rev(scales::rescale(exp(l), to = c(1, 1.5)))
f_lag_base = f_lag(l)
f_lag_x = function(x) scales::rescale(
  f_lag_base, to = c(f_exp_initial(x), f_exp_cumulative(x)))

plot(l, f_lag_x(21), type = 'l', ylim = c(1, 2))
lines(l, f_lag_x(30), type = 'l', col = 'red')

RR <- function(x)  {
  f_lag_base = f_lag(l)
  scales::rescale(
    f_lag_base, 
    to = c(f_exp_initial(x), 
           f_exp_cumulative(x)))
}

lapply(x, RR)

RR_mat <- do.call(cbind, lapply(x, RR))
RR_df <- as.data.table(RR_mat)
RR_df$l <- l
names(RR_df) <- as.character(c(x, "l"))

RR_df <- melt(RR_df, id.vars = c('l'), variable.factor = F)
RR_df$variable <- type.convert(RR_df$variable, as.is = T)
names(RR_df)[2:3] <- c('x', 'RR')
RR_df

## in 2d
ggplot(RR_df) +
  geom_line(aes(x = x, y = RR, color = factor(l)))

## in 3d
ggplot(RR_df) +
  geom_surface_3d(mapping= aes(x = x, y = l, z = RR, 
                               fill = RR)) +
  coord_3d() + scale_fill_gradient2()


