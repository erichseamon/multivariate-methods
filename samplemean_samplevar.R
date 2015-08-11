#------------------------------------------------------------------------#
# TITLE:        Stat519 - HM1-3.14.R
#
# COURSE:       Methods of Multivariate Analysis (FOR519)
#             
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         January 26. 2015
#
# STAGE:        
#
# COMMENTS:    
#                
#              
#
#--Setting the working directory and clearing the workspace-----------


#----SETUP SECTION----------------------------------------------------#

#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists------#
cat("\14")

#----set the working directory.  In order to run this script from any UI-#
#----network location - mount \\CALS-DDVJ9YR1\climatevariables as -------#
#----your Z: drive.------------------------------------------------------#

options(warn=0)

#---code for problem 3.14

Table3 <- matrix(c(35,35,40,10,6,20,35,35,35,30,3.5,4.9,30.0, 2.8, 2.7, 2.8, 4.6, 10.9, 8.0, 1.6, 2.80, 2.70, 4.38, 3.21, 2.73, 2.81, 2.88, 2.90, 3.28, 3.20), ncol = 3)
z <- numeric(0)
z <- c(z, 1:10)
obs <- c(1:10)
vars <- c(1:3)
for (i in obs) {
  for (j in vars) {
  z[i] <- ((Table3[i,1]) * 3) - (Table3[i,2]) + ((Table3[i,3]) * 2)
}}
mean(z)
var(z)

a <- matrix(c(3,-1,2))
a <- t(a)
meany <- matrix(c(28.1, 7.180, 3.089))
a %*% meany


#----code for problem 3.20

Table36 <- matrix(c(47.8, 46.4, 46.3, 45.1, 47.6, 52.5, 51.2, 49.8, 48.1, 45.0, 51.2, 48.5, 52.1, 48.2, 49.6, 50.7, 47.2, 53.3, 46.2, 46.3, 48.8, 47.3, 46.8, 45.3, 48.5, 53.2, 53.0, 50.0, 50.8, 47.0, 51.4, 49.2, 52.8, 48.9, 50.4, 51.7, 47.7, 54.6, 47.5, 47.6, 49.0, 47.7, 47.8, 46.1, 48.9, 53.3, 54.3, 50.3, 52.3, 47.3, 51.6, 53.0, 53.7, 49.3, 51.2, 52.7, 48.4, 55.1, 48.1, 51.3, 49.7, 48.4, 48.5, 47.2, 49.3, 53.7, 54.5, 52.7, 54.4, 48.3, 51.9, 55.5, 55.0, 49.8, 51.8, 53.3, 49.5, 55.3, 48.4, 51.8), ncol = 4)
z <- numeric(0)
z1 <- c(z, 1:20)
z2 <- c(z, 1:20)
z3 <- c(z, 1:20)

obs <- c(1:20)
vars <- c(1:4)
for (i in obs) {
  for (j in vars) {
    z1[i] <- ((Table36[i,1]) * 2) + ((Table36[i,2]) * 3) - (Table36[i,3]) + ((Table36[i,4]) * 4)
    z2[i] <- -((Table36[i,1]) * 2) - (Table36[i,2]) + ((Table36[i,3]) * 4) - ((Table36[i,4]) * 2)
    z3[i] <- ((Table36[i,1]) * 3) - ((Table36[i,2]) * 2) - (Table36[i,3]) + ((Table36[i,4]) * 3)
    
  }}

meanz <- c(mean(z1), mean(z2), mean(z3))
