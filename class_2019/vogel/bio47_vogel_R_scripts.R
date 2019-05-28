#summed pool calculations

pool_i <- dat[i, ]
pool_j <- dat[j, ]

d <- sqrt((pool_j$E - pool_i$E)^2 + (pool_j$N - pool_i$N)^2)


d<- 0
for (j in 1:20){
    pool_i <- dat[i,]
    pool_j <- dat[j,]
    d_ij <- sqrt((pool_j$E- pool_i$E)^2 + (pool_j$N-pool_i$N)^2)
    d <- d + d_ij
}

distances_i <- vector(length = 20)
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


#AverageDistance

average_distance <- distances_i/20
 

#Connectivity

connectivity <- 1/average_distance


#Regressions

dat$log_area <- log(dat$pool_area_m2)

fit_log_area <- lm(species_count ~ log_area, data = dat) 
summary(fit_log_area)

fit_connectivity <- lm(species_count ~ connectivity, data = dat)
summary(fit_connectivity)

fit_distance <- lm(species_count ~ average_distance, data = dat)
summary(fit_distance)

fit_dispersal <- lm(species_count ~ disperal, data = dat)
summary(fit_dispersal)


#Multiple Regressions

fit_area_conn <- lm(species_count ~ pool_area_m2 + connectivity, data = dat)
summary(fit_area_conn)

fit_log_conn <- lm(species_count ~ log_area + connectivity, data = dat)
summary(fit_log_conn)


#Dispersal Calcs

dat <- area_species_data

A <- dat$pool_area_m2
C <- dat$connectivity
C2 <- C^2


D1 <- A[1]*C2[1]*sum(A, 2:20)

D2 <- A[2]*C2[2]*sum(A, 1,3:20)

D3 <- A[3]*C2[3]*sum(A, 1:2,4:20)

D4 <- A[4]*C2[4]*sum(A, 1:3,5:20)

D5 <- A[5]*C2[5]*sum(A, 1:4,6:20)

D6 <- A[6]*C2[6]*sum(A, 1:5,7:20)

D7 <- A[7]*C2[7]*sum(A, 1:6,8:20)

D8 <- A[8]*C2[8]*sum(A, 1:7,9:20)

D9 <- A[9]*C2[9]*sum(A, 1:8,10:20)

D10 <- A[10]*C2[10]*sum(A, 1:9,11:20)

D11<- A[11]*C2[11]*sum(A, 1:10,12:20)

D12<- A[12]*C2[12]*sum(A, 1:11,13:20)

D13 <- A[13]*C2[13]*sum(A, 1:12,14:20)

D14 <- A[14]*C2[14]*sum(A, 1:13,15:20)

D15<- A[15]*C2[15]*sum(A, 1:14,16:20)

D16<- A[16]*C2[16]*sum(A, 1:15,17:20)

D17<- A[17]*C2[17]*sum(A, 1:16,18:20)

D18<- A[18]*C2[18]*sum(A, 1:17,19:20)

D19<- A[19]*C2[19]*sum(A, 1:18,20:20)

D20<- A[20]*C2[20]*sum(A, 1:19)


D <- c(D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15, D16, D17, D18, D19, D20)

D <- cbind(c(D))


data_new <- cbind(area_species_data, D)

                         
                        