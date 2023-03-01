##This is an unfinished script and did not work well at all. 
#Laura created a script to plot raster values in a raster :)


setwd("C:/Users/schafstall/Desktop/R iLand/Input script")

library(dplyr)
library(dbplyr)
library(RSQLite)

PWD50yrstest <- read.csv("C:\\Users\\schafstall\\Desktop\\R iLand\\Input script\\pwd_1500_2500_yliv.csv")

PWD50yrstest <- PWD50yrstest[-c(1:5),]

PWD50yrstest <- as.data.frame(PWD50yrstest)

##now I have 25 rows, one column. How to split the column?

library(dplyr)
##tidyR???

#PWD50yrstest <- PWD50yrstest %>% separate(PWD50yrstest, sep = " ", into = colmn, remove = FALSE)
