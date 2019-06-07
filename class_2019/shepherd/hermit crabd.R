library(tidyverse)
hermitdat <- read_csv("hermitdat.csv")

samdat <- hermitdat %>%
  filter(Crab_Species == "sam")
hirsdat <- hermitdat %>% 
  filter(Crab_Species == "hirs")
grandat <- hermitdat %>%
  filter(Crab_Species == "gran")

num_shells_crab <- hermitdat %>% 
  group_by(Crab_Species, Final_Shell,.drop = FALSE) %>% 
  summarize(n=n())
num_shells_crab
num_crab <- hermitdat %>% count(Crab_Species) %>% rename(N=n)
num_crab
num_shells_crab2 <- left_join(x=num_shells_crab, y=num_crab, by="Crab_Species")
num_shells_crab2

num_shells_crab2<- num_shells_crab2 %>% mutate(proportion = n/N)
num_shells_crab2

ggplot(data=num_shells_crab2, aes(x = Crab_Species, y=proportion, fill=Final_Shell)) + 
  geom_bar(stat="identity")

num_shells_crabfir <- hermitdat %>% count(Crab_Species, First_Shell) 
num_shells_crabfir
num_shells_crab2fir <- left_join(x=num_shells_crabfir, y=num_crab, by="Crab_Species")
num_shells_crab2fir

num_shells_crab2fir<- num_shells_crab2fir %>% mutate(proportion = n/N)
num_shells_crab2fir

ggplot(data=num_shells_crab2fir, aes(x = Crab_Species, y=proportion, fill=First_Shell)) + 
  geom_bar(stat="identity")

num_shells_crabor <- hermitdat %>% count(Crab_Species, Original_Shell) 
num_shells_crabor
num_shells_crab2or <- left_join(x=num_shells_crabor, y=num_crab, by="Crab_Species")
num_shells_crab2or

num_shells_crab2or<- num_shells_crab2or %>% mutate(proportion = n/N)
num_shells_crab2or

ggplot(data=num_shells_crab2or, aes(x = Crab_Species, y=proportion, fill=Original_Shell)) + 
  geom_bar(stat="identity")

samshells <- samdat %>% count(Crab_Species, Final_Shell) 
samshells
num_sam <- samdat %>% count(Crab_Species) %>% rename(N=n)
num_crab
samshells2 <- left_join(x=samshells, y=num_sam, by="Crab_Species")
samshells2

samshells2 <- samshells2 %>% mutate(proportion = n/N)
samshells2

hirsshells <- hirsdat %>% count(Crab_Species, Final_Shell) 
hirsshells
num_hirs <- hirsdat %>% count(Crab_Species) %>% rename(N=n)
num_hirs
hirsshells2 <- left_join(x=hirsshells, y=num_hirs, by="Crab_Species")
hirsshells2

hirsshells2 <- hirsshells2 %>% mutate(proportion = n/N)
hirsshells2

granshells <- grandat %>% count(Crab_Species, Final_Shell) 
granshells
num_gran <- grandat %>% count(Crab_Species) %>% rename(N=n)
num_gran
granshells2 <- left_join(x=granshells, y=num_gran, by="Crab_Species")
granshells2

granshells2 <- granshells2 %>% mutate(proportion = n/N)
granshells2

granadd <- data.frame("gran", "Littorina", 0, 15, 0)
names(granadd) <- c("Crab_Species", "Final_Shell", "n", "N", "proportion")
granshells3 <- rbind(granshells2, granadd)

granshells4 <- granshells3 %>% mutate(expected = N/6)
granshells4 <- granshells4 %>% mutate(chisq=(n-expected)^2/expected)
granchisq<- sum(granshells4$chisq)
granchisq
pchisq(q=granchisq, df=5, lower.tail=FALSE)

hirsadd <- data.frame("hirs", "Olivella", 0, 15, 0)
names(hirsadd) <- c("Crab_Species", "Final_Shell", "n", "N", "proportion")
hirsshells3 <- rbind(hirsshells2, hirsadd)

hirsshells4 <- hirsshells3 %>% mutate(expected = N/6)
hirsshells4 <- hirsshells4 %>% mutate(chisq=(n-expected)^2/expected)
hirschisq <- sum(hirsshells4$chisq)
hirschisq
pchisq(q=hirschisq, df=5, lower.tail=FALSE)

samadd <- data.frame("sam", Final_Shell = c("Cerratotoma", "Olivella", "Littorina"), 0, 15, 0)
names(samadd) <- c("Crab_Species", "Final_Shell", "n", "N", "proportion")
samshells3 <- rbind(samshells2, samadd)

samshells4 <- samshells3 %>% mutate(expected = N/6)
samshells4 <- samshells4 %>% mutate(chisq=(n-expected)^2/expected)
samchisq<- sum(samshells4$chisq)
samchisq
pchisq(q=samchisq, df=5, lower.tail=FALSE)

