#---------------------------------------------------------------------------------------------------------------
#Max Carnes-Mason
#7 August, 2018
#Script to run histogram and chi square on ping pong light penetration data for the completion of MA at Uark
#---------------------------------------------------------------------------------------------------------------

#Reset R brain
rm(list=ls())

#show current wd
getwd()

#set wd to my raw data file of my masters material
setwd("C://Users/carne/Desktop/R_Analyses/MA_Thesis/Data_Files")

#double check correct location
getwd()

#read in the file and save as a variable
pingpong.dat <- read.csv("pingponglight.csv")

#check the data for completeness and correct file selection:
names(pingpong.dat)
head(pingpong.dat)
dim(pingpong.dat)
str(pingpong.dat)

#orient the page so that multiple histograms can be displayed
par(mfcol=c(3,4))

#save all histograms to xxxhist
Con1hist <- hist(pingpong.dat$Con1, breaks = 4, main = "Con1", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
Con2hist <- hist(pingpong.dat$Con2, breaks = 4, main = "Con2", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
Con3hist <- hist(pingpong.dat$Con3, breaks = 4, main = "Con3", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
Cut1hist <- hist(pingpong.dat$Cut1, breaks = 4, main = "Cut1", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
Cut2hist <- hist(pingpong.dat$Cut2, breaks = 4, main = "Cut2", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
Cut3hist <- hist(pingpong.dat$Cut3, breaks = 4, main = "Cut3", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
Burn1hist <- hist(pingpong.dat$Burn1, breaks = 4, main = "Burn1", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
Burn2hist <- hist(pingpong.dat$Burn2, breaks = 4, main = "Burn2", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
Burn3hist <- hist(pingpong.dat$Burn3, breaks = 4, main = "Burn3", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
BC1hist <- hist(pingpong.dat$BC1, breaks = 4, main = "BC1", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
BC2hist <- hist(pingpong.dat$BC2, breaks = 4, main = "BC2", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
BC3hist <- hist(pingpong.dat$BC3, breaks = 4, main = "BC3", freq = TRUE, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")

#make objects out of all the columns in the data file
Con1col <- pingpong.dat$Con1
Con2col <- pingpong.dat$Con2
Con3col <- pingpong.dat$Con3
Cut1col <- pingpong.dat$Cut1
Cut2col <- pingpong.dat$Cut2
Cut3col <- pingpong.dat$Cut3
Burn1col <- pingpong.dat$Burn1
Burn2col <- pingpong.dat$Burn2
Burn3col <- pingpong.dat$Burn3
BC1col <- pingpong.dat$BC1
BC2col <- pingpong.dat$BC2
BC3col <- pingpong.dat$BC3

#make a variable containing these hist
#sites <- c(Con1hist, Con2hist, Con3hist, Cut1hist, Cut2hist, Cut3hist, Burn1hist, Burn2hist, Burn3hist, BC1hist, BC2hist, BC3hist)
sites <- list(Con1col, Con2col, Con3col, Cut1col, Cut2col, Cut3col, Burn1col, Burn2col, Burn3col, BC1col, BC2col, BC3col)


#write a for loop to run chisq on each hist against each hist
#outer loop begin========================
for (fo in sites) {
#inner loop begin========================
  for (fi in sites) {
    
   results <- chisq.test(table(fi, fo))
    print(fi)
    print(fo)
    print(results)
   
  }
#end of inner loop=============================
  
  
  }
#end of outer loop===========================




#######this apply attempt doesnt allow all the names to be stored as labels

#use apply function to iterate over every column of the pingpong.dat set, MARGIN = 2 give colunms, the function is hist
#save as an object, pphist, and print the object
#pphist <-apply(pingpong.dat, 2, hist, breaks = 4, xlim = c(0,100), ylim = c(0,80), xlab = "Light Density", ylab = "Number")
#pphist
#apply(pingpong.dat, 2, hist)

#weird sapply try to get column names
#colnames(pingpong.dat) <- c("Con1", "Con2", "Con3","Cut1","Cut2","Cut3","Burn1","Burn2","Burn3","BC1","BC2","BC3")
#sapply(colnames(pingpong.dat), hist(pingpong.dat) sum(pingpong.dat[,1]))
