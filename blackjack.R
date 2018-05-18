#Application reinforcement learning for black jack. We will suppose here that the croupier only has 1 pack of cards

#Initial tabs 
packinit = c(rep(1,4), rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),
            rep(9,4),rep(10,16))
#In our game and for simplicifaction of the problem, aces will always count as 1. Other figures are worth 10.
#If both player and croupier have same score, then player looses.
#Croupier will draw cards until he has 17 or more. 

            
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

#Picks a card, add it to the desired hand and deletes it from the package.
pickC = function(hand, pack){
  
  int = randInt(pack)
  hand = c(hand, pack[int])
  pack = pack[-int]
  
  return(list(hand, pack))
}

score = function(handC){
  return(sum(handC, na.rm = T))
}

printWinner = function(resultList){
  res = resultList[[4]]
  p = res[1]
  c = res[2]
  
  if((p > c && p <= 21) || (p <= 21 && c > 21)){
    cat("Player has won with ", p, ", croupier has ", c, ".\n", sep = "")
  }else{
    cat("Player has lost with ", p, ", croupier has ", c, ".\n", sep = "")
    
  }
}

#Black jack sim : 
simulation = function(handP, handC, pack){
  
  #Matrix to stock choice and state, 1st is state, 2nd is choice, 3rd is reward.
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
  cs = rbind(cs, c(score(handP), 1, 0))
  
  #pick second card 
  temp = pickC(handP, pack)
  handP = temp[[1]]
  pack = temp[[2]]
  
  temp = pickC(handC, pack)
  handC = temp[[1]]
  pack = temp[[2]]
  
  #stock result
  cs = rbind(cs, c(score(handP), 1, 0))

  #reward stock final
  reward = NULL
  
  #to change with algo decision 
  while(score(handC) < 17){
    #rand number to choose action, 1 = draw
    rand = round(2*runif(1),0)
    #if a = 1, draw a card
    if(rand == 1){
      temp = pickC(handP, pack)
      handP = temp[[1]]
      pack = temp[[2]]
      cs = rbind(cs, c(score(handP), 1, 0))
    }else{
      cs = rbind(cs, c(score(handP), 0, 0))
      
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
  if((p > c && p <= 21) || (p <= 21 && c > 21)){
    reward = 100
  }else{
    reward = -50
  }
  
  #Add reward as the reward of the last line of cs
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


for(i in 1:10){
  results = simulation(handPinit, handCinit, packinit)
  printWinner(results)
}

#####################################################################
#Q-learning
#####################################################################

#Represent sets of Q(s, a)
Qvalues = matrix(0, nrow = 30, ncol = 2)
dd = simRand(10)
