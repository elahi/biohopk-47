## 14 May 2019
## R Elahi
## Working on Jacqui Vogel's data

dat <- read.csv("class_2019/vogel/pool_xyz.csv")
head(dat)

i <- 1
j <- 2


pool_i <- dat[i, ]
pool_j <- dat[j, ]

d <- sqrt((pool_j$E - pool_i$E)^2 + (pool_j$N - pool_i$N)^2)
d

i

d <- 0
for(j in 1:20){
  pool_i <- dat[i, ]
  pool_j <- dat[j, ]
  d_ij <- sqrt((pool_j$E - pool_i$E)^2 + (pool_j$N - pool_i$N)^2)
  d <- d + d_ij
}
d

## Initialize a vector of summed distances for each pool
distances_i <- vector(length = 20)

## Nested for loop to calculate the summed distance between pool i and all other pools
for(i in 1:20){
  pool_i <- dat[i, ]
  d <- 0
  for(j in 1:20){
    pool_j <- dat[j, ]
    d_ij <- sqrt((pool_j$E - pool_i$E)^2 + (pool_j$N - pool_i$N)^2)
    d <- d + d_ij
    distances_i[i] <- d
  }
}
distances_i




# if I were motivated, try to generalize the above code:
# function to calculate distance between two points
# input must be a two column matrix of x and y values for each
get_distance <- function(point_i, point_j){
  d <- sqrt((pool_j$E - pool_i$E)^2 + (pool_j$N - pool_i$N)^2)
  
}


