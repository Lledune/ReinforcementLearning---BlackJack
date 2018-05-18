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

#Black jack sim
simulation = function(handP, handC, pack){
  #pick first card 
  temp = NULL
  temp = pickC(handP, pack)
  handP = temp[[1]]
  pack = temp[[2]]
  
  temp = pickC(handC, pack)
  handC = temp[[1]]
  pack = temp[[2]]
  
  #pick second card 
  temp = pickC(handP, pack)
  handP = temp[[1]]
  pack = temp[[2]]
  
  temp = pickC(handC, pack)
  handC = temp[[1]]
  pack = temp[[2]]
  
  #to change with algo decision 
  while(score(handP) < 17 || score(handC) < 17){
    if(score(handP) < 17){
      temp = pickC(handP, pack)
      handP = temp[[1]]
      pack = temp[[2]]
    }
    if(score(handC) < 17){
      temp = pickC(handC, pack)
      handC = temp[[1]]
      pack = temp[[2]]
    }
  }
  scores = c(score(handP), score(handC))
  return(list(handP, handC, pack, scores))
}

for(i in 1:10){
results = simulation(handPinit, handCinit, packinit)
printWinner(results)
}

