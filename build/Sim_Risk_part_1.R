# package install and load ----------------------------------------------------------------------

# install.packages("quantmod")
# install.packages("graphics")
# install.packages("ks")
# install.packages("gtools")
# install.packages("rio")
# install.packages("readxl")
# install.packages("scales")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("triangle")

library(quantmod)
library(graphics)
library(ks)
library(gtools)
library(rio)
library(readxl)
install_formats()
library(scales)
library(dplyr)
library(tidyr)
library(triangle)
ibrary(ggplot2)
library(reshape)
library(stringr)

# bring in data and clean ----------------------------------------------------------------------- 

# set working directory to be homework folder
setwd("C:/Users/Andrew/Google Drive/Simulation and  Risk/HW 1")

# import data and convert it to CSV
df = read_excel("Analysis_Data.xlsx", sheet = 2)
# remove the first two rows with the link and no values
df = df[-c(1:2),]
# change the column names
colnames(df) = c("Dates", 
                 "Crude Oil",
                 "Natural Gas",
                 "Dry Well",
                 "Return Crude Oil",
                 "Return Natural Gas",
                 "Return Dry Well")

# convert rest to be int and percent
df <- data.frame(apply(df, 2, function(x) as.numeric(as.character(x))))

# convert dates to be a only the year from excel format
df$Dates = as.Date(df$Dates, origin = "1899-12-30") 
df$Dates = substring(df$Dates, 0, 4)

# df$Return.Crude.Oil = percent(df$Return.Crude.Oil)
# df$Return.Natural.Gas = percent(df$Return.Natural.Gas)
# df$Return.Dry.Well = percent(df$Return.Dry.Well)


# Analysis --------------------------------------------------------------------------------------

# Add average column
df$CostsMean = with(df, (Crude.Oil+Natural.Gas+Dry.Well)/3)


# # Add change in average column 
# rtrnz = diff(df$CostsMean)/df$CostsMean[-length(df$CostsMean)]  
# rtrnz = percent(rtrnz)
# df$ChangeInMean = c('.', rtrnz)

# subset data to returns from 1991 to 2006
df1 = df%>% select(Dates, Return.Crude.Oil, Return.Natural.Gas, Return.Dry.Well) %>% filter(Dates > 1990 & Dates < 2007)
df1 = df1%>% select(Return.Crude.Oil, Return.Natural.Gas, Return.Dry.Well)
df1 = data.frame(returns = unlist(df1, use.names = FALSE))

# Check qqplot for normal D on returns
qqnorm(df1$returns, pch = 1, frame = FALSE)
qqline(df1, col = "steelblue", lwd = 2)

# """
# Returns D IS NORMAL 
# """


# Initial simulation
set.seed(112358)
P21 <- rep(0,10000)
for(i in 1:10000){
  P0 <- 2279.80000
  r <- rnorm(n=1, mean=mean(df1$returns), sd=sd(df1$returns))
  
  Pt <- P0*(1 + r)
  
  for(j in 1:5){
    r <- rnorm(n=1, mean=mean(df1$returns), sd=sd(df1$returns))
    Pt <- Pt*(1+r)
  }
  
  for(k in 1:3){
   r <- rtriangle(n = 1, b = -.07, a = -0.22, c = -.0917) 
   Pt <- Pt*(1+r)
  }
  
  for(l in 1:6){
    r <- rtriangle(n = 1, b = .06, a = .02, c = .05) 
    Pt <- Pt*(1+r)
  }
  P21[i] <- Pt
}


hist(P21, breaks=50, main='2021 Year Cost Distribution', xlab='Final Value')
abline(v = P0, col="red", lwd=2)
mtext("Initial Inv.", at=P0, col="red")



# Kernel density
returnsDensity = density(df1$returns)

set.seed(112358)
estReturns <- rkde(fhat=kde(df1$returns, h=returnsDensity$bw), n=10000)
hist(estReturns, breaks=50, main='KDE of Return Values', xlab='Returns(%)')


set.seed(112358)
P21KDE <- rep(0,10000)
for(i in 1:10000){
  P0 <- 2279.80000
  r <- sample(estReturns, 1, replace = TRUE)
  
  Pt <- P0*(1 + r)
  
  for(j in 1:5){
    r <- sample(estReturns, 1, replace = TRUE)
    Pt <- Pt*(1+r)
  }
  
  for(k in 1:3){
    r <- rtriangle(n = 1, b = -.07, a = -0.22, c = -.0917) 
    Pt <- Pt*(1+r)
  }
  
  for(l in 1:6){
    r <- rtriangle(n = 1, b = .06, a = .02, c = .05) 
    Pt <- Pt*(1+r)
  }
  P21KDE[i] <- Pt
}

hist(P21KDE, breaks=50, main='2021 Year Cost Distribution KDE', xlab='Final Value')
abline(v = P0, col="red", lwd=2)
mtext("Initial Inv.", at=P0, col="red")




################# Graphs ############################

# Creat quantiles for both simulations
P21Q = quantile(P21, probs = seq(0, 1, by= 0.05))
P21KDEQ = quantile(P21KDE, probs = seq(0, 1, by= 0.05))


#Reformat Quantiles as dataframes to make them easier to work with
P21Q=as.data.frame(P21Q, col.names=names(x))
P21KDEQ=as.data.frame(P21KDEQ,col.names=names(x))


#create plots dataframe with both simulation columns and calculate difference between them
plots=data.frame("P21KDEQ"=P21KDEQ,"P21Q"=P21Q)
plots$difference=plots$P21Q-plots$P21KDEQ


#Take quantiles from axes and create a row with them
plots$quantile <- row.names(plots)



#Remove the % and convert to a number
plots$num_quantile =as.numeric(as.character(str_sub(plots$quantile, 1, str_length(plots$quantile)-1)))




#Plot both simulations against each other
plots_sub=plots %>%  select(P21Q,P21KDEQ,num_quantile)
d <- melt(plots_sub, id.vars="num_quantile")
ggplot(data=d, aes(num_quantile, value, col=variable,group=2)) + geom_line()

#Plot difference between two simulations
ggplot(data=plots, aes(num_quantile, difference,group=1)) + geom_line()+ ylab("Normal Minus KDE")+xlab("Quantiles")




