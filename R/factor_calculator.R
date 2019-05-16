# max = maximum concentration
# min = minimum concentration
# n = number of concentrations
# m = dilution factor
# c = constant

#To calculate the dilution factor from known max, min and n
max <- 600
min <- 3
n <- 9

m <- (max/min)^(1/(n-1))

m

#To calculate min from known m, max and n
m <- 1.25

max <- 1250

n <- 9

min <- max/(m^n)

min

#To calculate max from known m, min and n
m <- 1.5

max <- min/(m^n)

max

#To calculate the constant
c <- (log(min)/log(m)) - 1

c

#To calculate the concentrations
concentrations <- function(x){
  print(m^(x+1+c))
}

conc <- concentrations(1:9)

round(conc,2)
