library(ggplot2)
nbiter<-as.numeric()
typesim<-as.character()
percentvictory<-as.numeric()

optPMC = as.numeric(policyMC[,1])
optP = as.numeric(policy[,1])

for(i in c(10,50,100,500,1000,5000,10000,50000,100000)){
  polTTT = policyTestK(i, optP)
  #Exemple of results for Qpolicy
  won = sum(polTTT[polTTT == 1])
  won/i
  nbiter<-c(nbiter,i)
  typesim<-c(typesim,"qlearning")
  percentvictory<-c(percentvictory,won/i)
  
  
  polTTT = policyTestK(i, optPMC)
  #Exemple of results for Qpolicy
  wonMC = sum(polTTT[polTTT == 1])
  wonMC/i
  nbiter<-c(nbiter,i)
  typesim<-c(typesim,"qlearningMC")
  percentvictory<-c(percentvictory,wonMC/i)
  
  
  randPolSim = randPolTestK(i)
  wonRand = sum(randPolSim[randPolSim == 1])
  wonRand/i
  nbiter<-c(nbiter,i)
  typesim<-c(typesim,"random")
  percentvictory<-c(percentvictory,wonRand/i)
  
}
resultmatrix2<-NULL
resultmatrix2 <- data.frame(typesim,nbiter,percentvictory)

ggplot(data=resultmatrix2, aes(x = nbiter, y=percentvictory, color=typesim, group=typesim)) +geom_line()
ggsave(file="simcomparison", plot=last_plot(), device="png", width = 15, height=15,units="cm")
