#TutorialFindUtilities
#Author: Preston Biro

#This is a tutorial for Biro and Walker's Reinforcement Learning for Football 
#     Play Calling Paper, or whatever it gets titled in the end

#This is the third script in a set of three (or maybe four or five, not sure 
#     yet) that will help you optimize play calling for a football team using
#     NFL data.

#This script will establish the functions that complete Algorithm 3 in the paper
#     that allow for optimization of a semi-terminal MDP

#Let's start by loading the libraries we plan to use

#Set your working directory properly, and run the previous script
setwd('~/GithubTutorial')
source('TutorialFindTransitionProbs.R')

#Load some data used for special team events (essentially give probabilities for
#   making a field goal at a given spot or a kicking a punt a certain distance)
load('FieldGoalProbs.RData')
load('PuntProbs.RData')

#Create some arrays that we need to store things. The first two are for storing
#   the calculated utilities so they don't have to be recalculated every time 
#   they are referenced. staticTurnOverStates is for the semi-terminal states,
#   keeping the first and 10 state utilities so we don't have a recursive loop
#   when we have a turnover event referenced. We initialize to be be a monotone
#   but relatively unrealistic values that will be cleaned out in the math. They
#   are recorded from the offense's perspective, meaning they are just the basic
#   first and ten (or goal) utilities from yard lines 1 to 99. For example, 
#   staticTurnOverStates[12] should be the utility of DOWN = 1, DIST = 10, and 
#   LOS = 12. This should also be the same as findStateUtil(1,10,12,'Offense'), 
#   but will probably be slightly different (off by a rounding error)
stateStore <- array(NA,4,99,99)
actionStore <- array(NA,4,4,99,99)
staticTurnOverStates <- seq(6,-1,length.out = 99)

#Muffed punt percentage, taken directly from the following link
#https://www.footballperspective.com/more-on-fumbling-and-recovery-rates-defense-and-special-teams/
muffPuntPerc = .0115 

#First function, which pulls out the probabilities for a specific state, and 
#   connects them to the future states that it connects to
findFutureStates <- function(curDown,curDist,curLOS){
  #Making a dataframe that connects the current state to all of its potential
  #   future states along with its probability of occurring given its possible
  #   actions.
  #So far, we've viewed everything in terms of the original offense's 
  #   perspective. Here, we are going to report things in terms of whoever has 
  #   the ball, but will explicitely state who has it in the future state.
  
  #Finds current line to gain
  curL2G = curLOS - curDist
  
  #Future State Data Frame
  storedNextStates = data.frame()
  
  #Get Those Probabilities
  futureProbs = advGetAndGiveProb(curDown,curDist,curLOS)
  runProbs = futureProbs$RunProb
  passProbs = futureProbs$PassProb
  
  #Add TD, safety, and Def TD states
  tdState = list(DOWN = NA,DIST = NA,LOS = 0,SCORE = 7,NextTeam = 'Score',
                 RunProb = runProbs[runProbs$X == 0,'Prob'],
                 PassProb = passProbs[passProbs$X == 0,'Prob'])
  safeState = list(DOWN = NA,DIST = NA,LOS = 0,SCORE = -2,NextTeam = 'Score',
                   RunProb = runProbs[runProbs$X == 100,'Prob'],
                   PassProb = passProbs[passProbs$X == 100,'Prob'])
  defTDState = list(DOWN = NA,DIST = NA,LOS = 0,SCORE = -7,NextTeam = 'Score',
                    RunProb = runProbs[runProbs$X == -100,'Prob'],
                    PassProb = passProbs[passProbs$X == -100,'Prob'])
  storedNextStates <- rbind(storedNextStates,tdState,safeState,defTDState)
  
  #Future states to loop through
  moveOpts = c(-99:-1,1:99)
  
  if(curDown < 4){
    #If not 4th down, all gains are gains and losses are losses (no weird 4th 
    #   down gains that are actually turnover on downs)
    for(move in moveOpts){
      if(move < 0){
        #Defense obtains ball state
        if(move < -90) nextDist = 100 + move
        else nextDist = 10
        storedNextStates <- rbind(storedNextStates,
                        list(DOWN = 1,DIST = nextDist,LOS = 100 + move,
                             SCORE = 0,NextTeam = 'Defense',
                             RunProb = runProbs[runProbs$X == move,'Prob'],
                             PassProb = passProbs[passProbs$X == move,'Prob']))
      }
      else if(move > 0){
        #Offense retains ball state
        if(move <= curL2G){
          #First down reached
          if(move < 10) nextDist = move
          else nextDist = 10
          storedNextStates <- rbind(storedNextStates,
                        list(DOWN = 1,DIST = nextDist,LOS = move,
                             SCORE = 0,NextTeam = 'Offense',
                             RunProb = runProbs[runProbs$X == move,'Prob'],
                             PassProb = passProbs[passProbs$X == move,'Prob']))
        }
        else{
          #Not first down, but not a turnover
          nextDown = curDown + 1
          nextDist = move - curL2G
          storedNextStates <- rbind(storedNextStates,
                        list(DOWN = nextDown,DIST = nextDist,LOS = move,
                             SCORE = 0,NextTeam = 'Offense',
                             RunProb = runProbs[runProbs$X == move,'Prob'],
                             PassProb = passProbs[passProbs$X == move,'Prob']))
        }
      }
    }
  }
  else{
    #Add negative states
    for(move in -1:-99){
      #Defense obtains ball state
      if(move < -90) nextDist = 100 + move
      else nextDist = 10
      storedNextStates <- rbind(storedNextStates,
                        list(DOWN = 1,DIST = nextDist,LOS = 100 + move,
                             SCORE = 0,NextTeam = 'Defense',
                             RunProb = runProbs[runProbs$X == move,'Prob'],
                             PassProb = passProbs[passProbs$X == move,'Prob']))
    }
    
    #Add positive gain values
    if(curL2G > 0){
      for(move in curL2G:1){
        #First down reached
        if(move < 10) nextDist = move
        else nextDist = 10
        storedNextStates <- rbind(storedNextStates,
                        list(DOWN = 1,DIST = nextDist,LOS = move,
                             SCORE = 0,NextTeam = 'Offense',
                             RunProb = runProbs[runProbs$X == move,'Prob'],
                             PassProb = passProbs[passProbs$X == move,'Prob']))
      }
      for(move in 99:(curL2G+1)){
        #Turnover on downs
        if(move > 90) nextDist = 100 - move
        else nextDist = 10
        storedNextStates <- rbind(storedNextStates,
                        list(DOWN = 1,DIST = nextDist,LOS = 100 - move,
                             SCORE = 0,NextTeam = 'Defense',
                             RunProb = runProbs[runProbs$X == move,'Prob'],
                             PassProb = passProbs[passProbs$X == move,'Prob']))
      }
    }
  }
  return(storedNextStates)
}

# q = findFutureStates(4,10,80)
# off = ggplot(q %>% filter(NextTeam == 'Offense')) + 
#   geom_point(aes(x = LOS,y = RunProb),color = 'red') +
#   geom_point(aes(x = LOS,y = PassProb),color = 'blue')
# def = ggplot(q %>% filter(NextTeam == 'Defense')) + 
#   geom_point(aes(x = LOS,y = RunProb),color = 'red') +
#   geom_point(aes(x = LOS,y = PassProb),color = 'blue')
# grid.arrange(off,def,nrow = 2)


#The big boy. Give it a down, distance, and line of scrimmage, and it'll tell 
#   you how many points you should get if you call the right play every time. If
#   you want details on how we got to here, read the paper, because we can't 
#   explain it in the comments. Basically we're just going to take a weighted
#   average of the future states based off the probability of getting to each
#   of them from the given state, given the best action is taken.
findStateUtil <- function(curDown,curDist,curLOS,curTeam = 'Offense'){
  #Calculates the value of being in a state by finding the max of the potential 
  #   actions. 
  
  #First look if terminal state. This shouldn't really be called,
  # if(is.na(curDown)){
  #   if(curLOS == 0) return(7)
  #   else if(curLOS == -100) return(-7)
  #   else if(curLOS == 100) return(-2)
  #   else print('Unknown State')
  # }
  # else{
  #   #Next, look if already stored, assuming we dont want it reset
  #   stateUtil = subStateStore[curDown,curDist,curLOS]
  #   if(!is.na(stateUtil) & curTeam == 'Offense') {
  #     # if(curTeam == 'Offense') return(stateUtil)
  #     # else return(-stateUtil)
  #     return(stateUtil)
  #   }
  # }
  
  #If we've already stored the state value, pull it
  stateUtil = stateStore[curDown,curDist,curLOS]
  if(!is.na(stateUtil) & curTeam == 'Offense') {
    return(stateUtil)
  }
  
  #Else, if its a turnover state, lets just return the staticTurnoverValue, but
  #   we have to negate it to put it in the right perspective. 
  if(curTeam == 'Defense') return(-staticTurnOverStates[curLOS])
  
  #Else, compute it manually
  #Normal States
  nextStateDf <- findFutureStates(curDown,curDist,curLOS)
  bestUtil = -100
  for(playIx in playsToSample){
    tempUtil = findActionUtil(curDown,curDist,curLOS,playIx,nextStateDf)
    if(tempUtil > bestUtil){
      optAction = mainCalls[playIx]
      bestUtil = tempUtil
    }
  }
  
  if(curDown == 4){
    puntUtil = findActionUtil(curDown,curDist,curLOS,'PUNT',nextStateDf)
    if(puntUtil > bestUtil) {
      optAction = 'PUNT'
      bestUtil = puntUtil
    }
    fgUtil = findActionUtil(curDown,curDist,curLOS,'FG',nextStateDf)
    if(fgUtil > bestUtil){
      optAction = 'FG'
      bestUtil = fgUtil
    }
  }
  subStateStore[curDown,curDist,curLOS] <<- bestUtil
  optActStore[curDown,curDist,curLOS] <<- optAction
  if(curLOS > 0) return(bestUtil)
  else return(-bestUtil)
}













