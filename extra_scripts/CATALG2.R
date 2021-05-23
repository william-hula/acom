#install.packages(c('irtoys', 'ltm', 'catR'))
library(catIrt)
library(irtoys)
library(ltm)
library(catR)
library(readxl)
#Set working directory
setwd("~/GitHub/PNT-CAT/pnt")

#load data
data <- read_excel("ItemParameters.xlsx")

pars = data.frame(a = data$Discrimination,
                  b = data$`Item Difficulty`,
                  c = rep(0), #1PL has no guessing parameter ,
                  d = rep(0), #1PL has no innatention parameter,
                  cbGroup = rep(1))

#format to a CATR structure
prov = breakBank(pars)
bank = prov$itemPar
#Select next item based on current ability estimate
nextItem(bank, 
         theta = 0)



#Estimate ability, theta -- not working yet
x <- rep(NA, 175)
x[123] = 1
str(x)


parsMatrix = as.matrix(pars[,1:4])
thetaEst(parsMatrix, x, method = "ML")
#Error: Error in uniroot(f, c(pr2$maximum, pr$minimum)) : 
#lower < upper  is not fulfilled