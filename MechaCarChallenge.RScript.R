M <- read.csv("MechaCar_mpg.csv")
Mregresion <- lm(mpg~vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=M)
summary(Mregresion)
S <- read.csv("Suspension_Coil.csv")
library(magrittr)
library(tidyverse)
S_total <- S %>% 
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
S_total
Smanufacture_total <- S %>% group_by(Manufacturing_Lot) %>% 
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
Smanufacture_total

t.test(S$PSI,MU=1500)
t.test(subset(S,Manufacturing_Lot =="Lot1")$PSI,mu=1500)

t.test(subset(S,Manufacturing_Lot =="Lot2")$PSI,mu=1500)

t.test(subset(S,Manufacturing_Lot =="Lot3")$PSI,mu=1500)

