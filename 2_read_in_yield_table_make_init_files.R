#
#
# This script reads in the yield table excel and based on that we can select species, Site index and make a list of trees for iLand initialization for artifical landscapes.

# Laura Dobor, CULS,
# 2022.12.05


library("readxl")
library(dplyr)

rm(list=ls()) #what is this?


#--------------------------------------------------------------   Read in the standgrid to know what ids we have:
grids.dataroot<-"C:/Users/schafstall/Desktop/iLand program/Artifical_landscape/input_data_prep/"


S.grid<-read.table(paste0(grids.dataroot,"stand_grid.asc"),skip=6)
class(S.grid)

# make a single array of the stand ids:
Stand.ids<-as.numeric(as.matrix(S.grid))
class(Stand.ids)

# Number of unique stands:
n.stands<-length(unique(Stand.ids))
print(paste("Number of stands: ",n.stands))

unis<-unique(Stand.ids)
#--------------------------------------------------------------  READ in the yield table:

# Yield table data:
f<-"C:/Users/schafstall/Desktop/iLand program/Artifical_landscape/YieldTabs_NoTrees_v2_ACTUAL.xlsx"

excel_sheets(f)

spec<-"pisy"    # SET the SPECIES


#  handle column names (its a bit wierd excel) --> here we read it only for the column names and to be able to produce them
data <- read_excel(f,spec, col_names=T)   
names<-colnames(data)[seq(2,ncol(data),by=3)]   
names2<-rep(names, each=3)
cols<-rep(c("HP", "ZP", "PP"), length(names2)/3)

colnames<-c("age" ,   paste(names2,cols, sep="_"))


# Here we really read in the excel data, skipping the first 3 lines where is this mess, and giving there directly the good column names
data <- data.frame(read_excel(f,spec, col_names=colnames,skip=3))
summary(data)
head(data)


# there are HP, ZP and PP columns, we need to use the HP columns

#Select the Site index that we want:- (this will get more complicated if we have more stanbd IDs)
SI=unis     # I put the stand id here as SI
 

colnames.mySImycol<-colnames[grep(colnames, pattern=paste0("SI",SI,"_HP"))]   #filter the columnnames to have SI number inside

print(colnames.mySImycol)

colnames.mySImycol<-c("age",colnames.mySImycol)  # we need also age column

data.we.need<-data[ ,match(colnames.mySImycol,colnames)]

head(data.we.need)


# Which age we want to initialize:
Init.age<-35


a<-data.we.need %>% filter(age==Init.age)

print(a)
colnames(a)<-c("age","count","dbh","h","BA")

DBH.avg<-a$dbh
H.avg<-a$h

n<-a$count
print(paste(spec, "SI:",SI, "DBH:", DBH.avg, "H:", H.avg, "No trees:",n , "age:", Init.age))


#-----------------------------------------------------------------------------------------------------------------------



# -------------------------------------------------    Create the distribution for this stand:

bigtrees.stepwiseranges<-c(0.1, .3, .5, .7, .9, 1.1, 1.3, 1.5, 1.7, 1.9, 2.1)      # this value*STD around the mean gives the limits of the ranges

DBH.std<-0.05*DBH.avg


Std.Scales.Poz<-bigtrees.stepwiseranges
Std.Scales.Neg<-(-1)*Std.Scales.Poz

Std.Scales <- sort(as.vector(rbind(Std.Scales.Neg,Std.Scales.Poz)))    # merge the - and + side, and sort it ascending
Std.Scales <- unique(Std.Scales)       # 0 will be there twice
Range.Delim <- Std.Scales*DBH.std             # delimiter values
Range.Delim

Ranges<-DBH.avg+Range.Delim

print("These are the ranges: ")
print(Ranges)

# Check are there any neg. values, if yes, skip those
if (min(Ranges) <= 0) {
  #print("There are negative values in the ranges!!!!!!")
  g<-which(Ranges<=0)
  gn<-length(g)
  #print(paste("number of neg. range boarders:", gn))
  Ranges<-Ranges[(gn+1):length(Ranges)]
  
}
nr<-length(Ranges)-1

##now putting a normal distribution to these DBH distribution to match 

p<- pnorm(Ranges, mean = DBH.avg, sd = DBH.std)   # use normal distribution 


pn<-length(p)


for (i in 1:pn){
  if (i==1) inc=0
  if (i>1) inc[i]=p[i]-p[i-1]
}

#inc
inc2<-inc+(1-sum(inc))/(pn-1)
inc3<-inc2[2:pn]

Tree.Numbers=floor((inc3*n))      # round the number to the lower end.. and later sort the rest of the trees to the center
Tree.Numbers
print(sum(Tree.Numbers))
plot(Tree.Numbers)

is.even <- function(x) x %% 2 == 0 ##what is this?

# I check here if the distributed trees are really summing up to our "n", the number of trees that we want to put here, and giving one extra to some of the ranges
if (sum(Tree.Numbers) != n) {
  #print("Correcting the roundings....")
  difference=n-sum(Tree.Numbers)
  # print(paste("DIFF:",difference))
  if (is.even(difference) == TRUE) {
    #  print("EVEN")
    diff=difference-1
    from=floor((nr-diff)/2)+1
    to=floor(from+diff)-1
    #  print(paste(from,"->",to, "add 1"))
    Tree.Numbers[from:to]=Tree.Numbers[from:to]+1
    Tree.Numbers[floor(nr/2)+1]=Tree.Numbers[floor(nr/2)+1]+1
    
  }
  if (is.even(difference) == FALSE) {
    #   print("NOT EVEN")
    from=(nr-difference)/2+1
    to=from+difference-1
      print(paste(from,"->",to, "add 1"))
    Tree.Numbers[from:to]=Tree.Numbers[from:to]+1
  }
  
}
Tree.Numbers
# Last check....
if (sum(Tree.Numbers) != n) {
  print("STILL WRONG....!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  difference=n-sum(Tree.Numbers)
  difference
}

######IT WORKED UNTIL HERE#####
plot(Ranges[2:pn],Tree.Numbers, type="h", xlab="dbh ranges")

# info on dbh:
dbh.info <- data.frame(dbh.min=Ranges[1:pn-1], dbh.max=Ranges[2:pn], tree.num=Tree.Numbers, dbh.avg=(Ranges[1:pn-1]+Ranges[2:pn])/2,
                       age=Init.age)


# LOOK Height, diameter ratios, there are pre-defined functions from literture that we will use, but it is dbh dependent so each range will have different h:d

## HD

Hd.Eqs <- function(spec, dbh) {  # I define here a function
  hd<-dbh
  hd[]<-NA
  
  Richards<- function(a,b,c,d,x) {   
    y<- a / ((1 + exp(b+c*x))^(1/d))
    return(y)
  }
  
  hd<- Richards(81.8,-100.35,3.18,187.16, dbh)   # use piab as default (2017.01.06)
  print(hd)
  print(dbh)
  if (spec=="lade") hd<-Richards(85, -4.88, 0.18, 15.05, dbh)
  
  if (spec=="piab") hd<- Richards(81.8,-100.35,3.18,187.16, dbh)
  if (spec=="abal") hd<- Richards(82.1, -10.07, 0.32,16.8, dbh )
  if ((spec=="pisy")|(spec=="pini")) hd<-(1.0046-0.0076*dbh)*100
  
  # Do some randomization
  N<-length(hd)
  std<- abs( -0.0429*dbh+3)  #5.4286)
  hd.rand<- rnorm(N,hd,std)
  return(hd.rand)
} # end of function Hd.Eqs




hd.calc<-Hd.Eqs(spec,dbh.info$dbh.avg)   # here I am using the function above
# CREATE INIT FILE

final <- data.frame(stand_id=as.numeric(SI),
                    species=spec,
                    count=dbh.info$tree.num,
                    dbh_from=round(dbh.info$dbh.min,2),
                    dbh_to=round(dbh.info$dbh.max,2),
                    hd=round(hd.calc,2) , #,hd=round(hd,2),
                    age=dbh.info$age)

final
# the ordering is important: from big trees to small trees

final<-final [with(final,order(-dbh_to)),]

print(final)




# write 
out.dataroot<-grids.dataroot    # use the same place

write.table(final, file=paste(out.dataroot,spec,"_init.txt",sep=""), append = FALSE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=TRUE)








