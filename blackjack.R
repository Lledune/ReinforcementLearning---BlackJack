#Application reinforcement learning for black jack. We will suppose here that the croupier only has 1 pack of cards

#Initial tabs 
packinit = c(rep(1,4), rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),
            rep(9,4),rep(10,16))
#In our game and for simplicifaction of the problem, aces will always count as 1. Other figures are worth 10.
#If both player and croupier have same score, then player looses.
#Croupier will draw cards until he has 17 or more. 
#if both have a score > 21, then the lowest value wins

            
handPinit = NULL # will contain hand of player
handCinit = NULL # will contain hand of the croupier 

list = list(handPinit, handCinit, packinit)

# Methods #################################################################################### 
##############################################################################################

#Random integer, returns an integer to choose card 
randInt = function(pack){
  int = runif(1) * length(pack)
  int = int+1
  int = round(int, 0)
  return(int)
}

#Picks a card, asimResults it to the desired hand and deletes it from the package.
pickC = function(hand, pack){
  
  int = randInt(pack)
  hand = c(hand, pack[int])
  pack = pack[-int]
  
  return(list(hand, pack))
}

score = function(handC){
  sum = 0
  for(i in 1:length(handC)){
    if(is.na(handC[i]) == F){
      sum = sum + handC[i]
    }
  }
  
  return(sum)
}

printWinner = function(resultList){
  res = resultList[[4]]
  p = res[1]
  c = res[2]
  
  if((p < c && c > 21) || (p <= 21 && p > c)){
    cat("Player has won with ", p, ", croupier has ", c, ".\n", sep = "")
  }else{
    cat("Player has lost with ", p, ", croupier has ", c, ".\n", sep = "")
    
  }
}

#Black jack sim : 
simulation = function(handP, handC, pack){
  
  #Matrix to stock choice and next state, 1st is state, 2nd is choice, 3rd is reward, 4th is start state
  cs = NULL
  
  #pick first card 
  temp = NULL
  temp = pickC(handP, pack)
  handP = temp[[1]]
  pack = temp[[2]]
  
  temp = pickC(handC, pack)
  handC = temp[[1]]
  pack = temp[[2]]

  #stock result
  cs = rbind(cs, c(score(handP), 1, 5, 0))
  
  #pick second card 
  temp = pickC(handP, pack)
  handP = temp[[1]]
  pack = temp[[2]]
  
  temp = pickC(handC, pack)
  handC = temp[[1]]
  pack = temp[[2]]
  
  #stock result
  cs = rbind(cs, c(score(handP), 1, 5, cs[length(cs[,1]), 1]))

  #reward stock final
  reward = NULL
  
  #to change with algo decision 
  while(score(handP) < 21){
    #rand number to choose action, 1 = draw
    rand = round(2*runif(1),0)
    #if a = 1, draw a card
    if(rand == 1 && score(handP) < 21){
      temp = pickC(handP, pack)
      handP = temp[[1]]
      pack = temp[[2]]
      cs = rbind(cs, c(score(handP), 1, 5, cs[length(cs[,1]), 1]))
    }else{
      cs = rbind(cs, c(score(handP), 0, 0, cs[length(cs[,1]), 1]))
      if(score(handP) >= 17){
        break
      }
    }
    #if croupier < 17, he draws a card
    if(score(handC) < 17){
      temp = pickC(handC, pack)
      handC = temp[[1]]
      pack = temp[[2]]
    }
  }
  
  #get scores
  scores = c(score(handP), score(handC))
  resultList = list(handP, handC, pack, scores)
  
  #get reward
  res = resultList[[4]]
  p = res[1]
  c = res[2]
  if((p < c && c > 21) || (p <= 21 && p > c)){
    reward = 100
  }else{
    reward = -25
  }
  
  #AsimResults reward as the reward of the last line of cs
  cs[length(cs[,1]), 3] = reward
  
  #return full list 
  resultList = list(handP, handC, pack, scores, cs)
  return(resultList)
}


#Function for simulation, outputs tab containins states, actions and choices 
simRand = function(k){
  resultsRand = NULL
  for(i in 1:k){
    #init pack and hands
    pack = c(rep(1,4), rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),
                 rep(9,4),rep(10,16))
    handC = NULL
    handP = NULL
    #simulation k
    res = simulation(handP, handC, pack)
    resultsRand = rbind(resultsRand, res[[5]])
    
    #resets for next iteration 
    pack = c(rep(1,4), rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),
             rep(9,4),rep(10,16))
    handC = NULL
    handP = NULL
  }
  return(resultsRand)
}

#test 
for(i in 1:10){
  results = simulation(handPinit, handCinit, packinit)
  printWinner(results)
}
#used to max the Qvalue decision
getRowMax = function(tab){
  temp = tab[1]
  for(i in 2:length(tab)){
    if(tab[i] > temp){
      temp = tab[i]
    }
  }
  return(temp)
}

#converts QValues tab to optimal policy 
Qpolicy = function(Qvalues){
  policy = as.data.frame(matrix(ncol = 1, nrow = 22))
  colnames(policy) = "policy"
  rownames(policy) = 0:21
  for(i in 1:22){
    if(Qvalues[i, 1] > Qvalues[i, 2]){
      policy[i,1] = 0
    }else{
      policy[i,1] = 1
    }
  }
  #If player has 21, never draw. (as the case won't be treated because of a condition in our simulation for the player to stop at 21, which is rational)
  policy[22, 1] = 0
  return(policy)
}

#We use the monte carlo function to try to better approximate the optimal policy. We use empirical mean which is 1/n*sum(g(x))
monteCarloSim = function(m, alpha, discount){
  QvaluesMC = matrix(0, nrow = 35, ncol = 2)
  for(i in 1:m){
    Qvalues = matrix(0, nrow = 35, ncol = 2)
    #Represent sets of Q(s, a)
    simResults = simRand(10000)
    
    simResults[simResults[,1] == 0, 1] = 29
    #for all rows simulated, update qvalues.
    for(i in 1:length(simResults[,1])){
      st = simResults[i, 4] #st
      a = simResults[i, 2] #a
      stPlusOne = simResults[i, 1] #st+1
      Qvalues[st+1, a+1] = (1-alpha) * Qvalues[st+1, a+1] + alpha * (simResults[i, 3] + discount * getRowMax(Qvalues[stPlusOne+1, ]))
    }
    QvaluesMC = QvaluesMC + Qvalues
  }
  QvaluesMC = QvaluesMC/m
}


#####################################################################
#Q-learning
#####################################################################

#Represent sets of Q(s, a)
Qvalues = matrix(0, nrow = 40, ncol = 2)
#initialisation of goal 
Qvalues[21,1] = 100
simResults = simRand(10000)
#Hyperparameters
alpha = 0.5
discount = 0.25

simResults[simResults[,1] == 0, 1] = 27
#for all rows simulated, update qvalues.
for(i in 1:length(simResults[,1])){
  st = simResults[i, 4] #st
  a = simResults[i, 2] #a
  stPlusOne = simResults[i, 1] #st+1
  Qvalues[st+1, a+1] = (1-alpha) * Qvalues[st+1, a+1] + alpha * (simResults[i, 3] + discount * getRowMax(Qvalues[stPlusOne+1, ]))
}
policy = Qpolicy(Qvalues)

#Using montecarlo
QvaluesMC = monteCarloSim(5, 0.25, 0.5)
policyMC = Qpolicy(QvaluesMC)

#Hyperparameters : 
#alpha is kept small, so if a player makes a bad move that result in a lucky win it doesn't affect too much the Qvalues 
#discount is pretty high since the max reward/loss comes after the game has ended, which is usually after 2-3 games.
#the policy still seems to vary a little bit between each run, which is obviously because our training strategy is chosen randomly, and our sets can't be too big because of computing time limitations.
#it seems that the algorithm want to draw is it has 16 or less score most of the time. 
####################################################################
#Results
####################################################################

#For the result section i will use the optimal policy derived from our Q-learning method and see how well it does against the croupier basic strategy. 

optP = as.matrix(policyMC)

policyTest = function(optP){
  ###########
  pack = c(rep(1,4), rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),
           rep(9,4),rep(10,16))
  handC = NULL
  handP = NULL
  ###########
  #to change with algo decision 
  while(score(handP) < 21){
    choice = optP[score(handC)+1, 1]
    
    #if a = 1, draw a card
    if(choice == 1 && score(handP) < 21){
      temp = pickC(handP, pack)
      handP = temp[[1]]
      pack = temp[[2]]
    }
    
    #if croupier < 17, he draws a card
    if(score(handC) < 17){
      temp = pickC(handC, pack)
      handC = temp[[1]]
      pack = temp[[2]]
    }
  }
  
  #get scores
  scores = c(score(handP), score(handC))
  
  #result
  result = NULL
  if(scores[1] > scores[2]){
    result = 1
  }else{
    result = 0
  }
  return(result)
}

policyTestK = function(k, optPT){

  resultt = NULL
  
  for(i in 1:k){
    resultt = c(resultt, policyTest(optPT))
  }
  return(resultt)
}

policytest = policyTest(optP)
polTTT = policyTestK(100, optP)
