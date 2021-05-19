#install.packages(c('irtoys', 'ltm', 'catR'))
library(catIrt)
library(irtoys)
library(ltm)
library(catR)
library(readxl)


#load data
data <- read_excel("www/ItemParameters.xlsx")

pars = data.frame(a = data$Discrimination,
                  b = data$`Item Difficulty`,
                  c = rep(1), #1PL has no guessing parameter ,
                  d = rep(0), #1PL has no innatention parameter,
                  cbGroup = rep(1))

#format to a CATR structure
prov = pars
#Select next item based on current ability estimate
nextItem(bank, 
         theta = 0)



#Estimate ability, theta -- not working yet
x <- rep(NA, 175)
x[50:123] = 0
x[1:40] = 1

str(x)


parsMatrix = bank# as.matrix(pars[,1:4])
thetaEst(parsMatrix, x, method = "BM")
#Error: Error in uniroot(f, c(pr2$maximum, pr$minimum)) : 
#lower < upper  is not fulfilled



irt_function <- function(all_items){

pars = data.frame(a = all_items$discrimination,
                  b = all_items$itemDifficulty,
                  c = rep(1), #1PL has no guessing parameter ,
                  d = rep(0), #1PL has no innatention parameter,
                  cbGroup = rep(1))

bank = breakBank(prov$itemPar)
x = all_items$response
ability = thetaEst(parsMatrix, x, method = "BM")
next_item = nextItem(bank, ability)
  

}

irt_function(data, )


