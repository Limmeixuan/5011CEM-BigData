#use data.table package
install.packages("tidyverse")
library (tidyverse)
library (data.table)

#-----------------------------------------------------------------------Chapter2--------------------------------------------------------------
df <- function(i){
  list.files(path = "C:/Users/cvs59/Downloads/7796666 (1)/May_osward_grocery/", pattern = "*.csv") %>% 
    map_df(~fread(.))
}
df

#sq processing 
install.packages("microbenchmark")
library(microbenchmark)
mbm
autoplot(mbm)

#sequential
seq <- system.time(save1 <- lapply(1:100, df))

#Parallel Processing
numCores <- detectCores()
numCores

cl <- parallel::makeCluster(numCores)
clusterEvalQ(cl,{
  library(parallel)
  library(tidyverse)
})

df2 <-function(i){
  parLapply(cl,1:100,df)
  stopCluster(cl)
}

#Microbenchmark - parallel processing 
mbm <- microbenchmark(parLapply(cl,1:100,df))
mbm
autoplot(mbm)

system.time({
  parLapply(cl,1:100,df)
  stopCluster(cl)
})

#---------------------------------------------------------------------Chapter 3------------------------------------------------------------
#Import the data  read .csv
ward<-read.csv("C:/Users/cvs59/Downloads/7796666 (1)/May_osward_grocery.csv")
View(ward)

# Print the first 10 rows
head(ward, 10)

#Print the last 10 rows
tail(ward, 10)

# Description of the data
str(ward)

# Statistical summary of the data
summary(ward)

# Change the value of digits
summary(ward, digits = 1)


#Descriptive Statistics
# Compute the maximum value
max(ward$sugar)

# Compute the minimum value
min(ward$sugar)

# Compute the mean value
mean(ward$sugar)

# Compute the median value
median(ward$sugar)

# Standard deviation
sd(ward$sugar)

# Variance
var(ward$sugar)

# Compute the median absolute deviation
mad(ward$sugar)

# Compute the mode
# install modeest package
install.packages("modeest")
library (modeest)
mfv(ward$sugar)

# Range
range(ward$sugar)

# Compute range for each column
sapply(ward[, -5], range)

# Quartile
quantile(ward$sugar)

# Deciles
quantile(ward$sugar, seq(0, 1, 0.1))

# Interquartile
IQR(ward$sugar)

# Compute IQR for each column
#install pastecs package
install.packages("pastecs")
library (pastecs)
res <- stat.desc(ward[, -5])
round(res, 2)

#Graphical Display of Distributions
#Install package ggpubr
install.packages('ggpubr')
library (ggpubr)

#box plots based on sugar
ggboxplot(ward, y = "sugar", width = 0.5)+
  labs (y="sugar",title="Box Plot of sugar")


#a histogram based on sugar
gghistogram(ward, x = "sugar", bins = 9, add = "mean")+
  labs (x="sugar",y="Frequency",title="Histogram")


#Calculate Empirical Cumulative Distribution Function (ECDF)
ggecdf(ward, x = "sugar")+
  labs (x="sugar",y="Frequency",title="Calculate Empirical Cummulative Distribution Function")


#Q-Q plots
ggqqplot(ward, x = "sugar")+
  labs (x="sugar",y="Frequency",title="Q-Q plots")


#box plots based on fat
ggboxplot(ward, y = "fat", width = 0.5)+
  labs (y="fat",title="Box Plot of fat")


#a histogram based on fat
gghistogram(ward, x = "fat", bins = 9, add = "mean")+
  labs (x="fat",y="Frequency",title="Histogram")


#Calculate Empirical Cummulative Distribution Function (ECDF)
ggecdf(ward, x = "fat")+
  labs (x="fat",y="Frequency",title="Calculate Empirical Cummulative Distribution Function")


#Q-Q plots
ggqqplot(ward, x = "fat")+
  labs (x="fat",y="Frequency",title="Q-Q plots")

#create correlation matrix of (rounded to 2 decimal places)
round(cor(ward[c("sugar", "fat")]), 2)




#Multiple linear regression 
#Using ward dataset 
input <- ward [1:50, c("sugar", "fat")]

# Create regression model
model <- lm(sugar~ fat,
            data = input)
model 

# Print regression model
print(model)
print(summary(model))

#Plot the graph 
plot(model)

# Create regression model
model <- lm(fat~sugar,
            data = input)
model 






