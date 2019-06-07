library(dplyr)
library(tidyverse)
source("http://stat.duke.edu/courses/Fall12/sta101.001/labs/inference.R")
library(readr)
Book4 <- read_csv("~/Documents/Book4.csv")
View(Book4) #Seperate Data Sheet that seperates the anemone's into the two sites, making it a categorical variable#

#mean(exposed_mean_num)
#h <- mean(exposed_num)
#i <- mean(shaded_num)
#t.test(h,i)
#t.test(shaded_mean_num,exposed_mean_num)

exposed_num <- c(455,86,46,109,345,573,765,236,197,398,0,0,0,0)
shaded_num <- c(332,77,610,64,47,733,533,145,252,542,0,0)

hist(exposed_num); hist(shaded_num) # Non-normal but so few data points that it doesn't matter #

t.test(exposed_num,shaded_num, var.equal = TRUE)

inference(Book4$Site, Book4$num, est = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical") #ht for Site and number of Algae 


p<-ggplot(data=Book4, aes(x=num, y=X2)) +
  geom_bar(stat="identity")
p

#n <- mean(Book4$num)
#o <- mean(Book4$X2)
#t.test(o,n)

