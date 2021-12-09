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
stateStore <- array(NA,c(4,99,99))
actionStore <- array(NA,c(4,4,99,99))
staticTurnOverStates <- seq(6,-1,length.out = 99)

#This isn't acutally necessary for the math, but it helps to store the best 
#   action just for future lookup purposes
optActStore <- array(NA,c(4,99,99))

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

#The big boy. Give it a down, distance, and line of scrimmage, and it'll tell 
#   you how many points you should get if you call the right play every time. If
#   you want details on how we got to here, read the paper, because we can't 
#   explain it in the comments. Basically we're just going to take a weighted
#   average of the future states based off the probability of getting to each
#   of them from the given state, given the best action is taken.
findStateUtil <- function(curDown,curDist,curLOS,curTeam = 'Offense'){
  #Calculates the value of being in a state by finding the max of the potential 
  #   actions. 
  
  #If we've already stored the state value, pull it
  stateUtil = stateStore[curDown,curDist,curLOS]
  if(!is.na(stateUtil) & curTeam == 'Offense') {
    return(stateUtil)
  }
  
  #Else, if its a turnover state, lets just return the staticTurnoverValue, but
  #   we have to negate it to put it in the right perspective. 
  if(curTeam == 'Defense') return(-staticTurnOverStates[curLOS])
  
  #Else, compute it manually
  print(paste(curDown,curDist,curLOS))
  
  #Normal States
  nextStateDf <- findFutureStates(curDown,curDist,curLOS)
  
  #Look up the action value for all the different options, report the best
  bestUtil = -100
  playsToSample = c('RUN','PASS')
  for(play in playsToSample){
    tempUtil = findActionUtil(curDown,curDist,curLOS,play,nextStateDf)
    if(tempUtil > bestUtil){
      optAction = play
      bestUtil = tempUtil
    }
  }
  
  #If its 4th down, look up FG and PUNT too
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
  
  #Store the utility and optimal action for dynamic programming stuff
  stateStore[curDown,curDist,curLOS] <<- bestUtil
  optActStore[curDown,curDist,curLOS] <<- optAction
  return(bestUtil)
}

#The big boy's little brother, but he's still pretty big. Give it a down, 
#   distance, line of scrimmage, and a specific action, and it'll tell you how 
#   many points you should expect to get, given you take this action and all 
#   other recommended actions at the subsequent states reached. 
findActionUtil <- function(curDown,curDist,curLOS,curPlay,nextStates = NULL){
  playIx = switch(curPlay,'PASS' = 1,'RUN' = 2,'FG' = 3,'PUNT' = 4)

  #First check if we have it
  actUtil = actionStore[playIx,curDown,curDist,curLOS]
  if(!is.na(actUtil)) {
    return(actUtil)
  }
  
  #If we don't, or we just wanna reset, lets find it
  
  #First make sure we have a state frame
  if(is.null(nextStates)) nextStates = findFutureStates(curDown,curDist,curLOS)
  
  if(curPlay == 'PUNT'){
    #Punt
    
    #Its gotten to be pretty rare for teams to punt from within 40 yards, so
    #   we assume all yard lines less than 40 to have the same punt dist as a 
    #   punt from the 40. In hindsight, this was probably too aggressive, but
    #   this is fixed in future methods
    if(curLOS < 40) losForCalcs = 40
    else losForCalcs = curLOS
    
    #Put together the future states for a punt, similar to FindFutureStates func
    curPuntDf = data.frame(Util = staticTurnOverStates)
    curPuntDf$Prob = punt.df[(punt.df$ResultLOS > 0),paste('LOS=',losForCalcs,sep='')]
    
    #For muffed punts, will assume (incorrectly) that if it happens, it will 
    #   happen at the location of the punt's landing spot, which we also assume 
    #   to be the point with the highest probability density
    muffPuntLOS = punt.df[which(punt.df[,paste('LOS=',losForCalcs,sep='')] == 
                          max(punt.df[,paste('LOS=',losForCalcs,sep='')])),
                          'ResultLOS']
    
    #The utility for an offense recovering a muffed punt is just the utility of
    #   having a first and 10 at that location, from the offenses perspective
    offRecUtil = staticTurnOverStates[100 - muffPuntLOS]
    
    #The utility for returning a punt is the weighted sum of the probabilities
    #   of returning the ball to each los by the utility of that first and ten 
    #   or goal spot, plus the return touchdown prob, all from offense view
    retPuntUtil = -sum(curPuntDf$Util * curPuntDf$Prob) - 
      7 * punt.df[1,paste('LOS=',losForCalcs,sep='')]
    
    #Add it up, weighinig the muff punt percentage properly
    totalPuntUtil = muffPuntPerc * offRecUtil + (1 - muffPuntPerc) * retPuntUtil
    actionStore[playIx,curDown,curDist,curLOS] <<- totalPuntUtil
    return(totalPuntUtil)
  }
  else if(curPlay == 'FG'){
    #FG
    
    #Find probability of making fg
    curFGProb = fg.df[fg.df$LOS == curLOS,'Prob']
    
    #Find util of 1st and 10 if missed
    missLOS = 100 - curLOS
    utilIfMissed = staticTurnOverStates[missLOS]
    
    #Overall Utility is 3 * probMaking - (1-probMaking) * util if missed
    fgUtil = 3 * curFGProb + utilIfMissed * (1 - curFGProb)
    actionStore[playIx,curDown,curDist,curLOS] <<- fgUtil
    return(fgUtil)
  }
  else{
    #Find utilities
    nextStates$StateUtil = NA
    for(ix in 1:nrow(nextStates)){
      if(nextStates[ix,'NextTeam'] == 'Offense'){
        #For the states where the offense keeps the ball, we pull the utility 
        #   normally by looking ahead
        nextStates[ix,'StateUtil'] = findStateUtil(curDown = nextStates[ix,'DOWN'],
                                                   curDist = nextStates[ix,'DIST'],
                                                   curLOS = nextStates[ix,'LOS'])
      }
      else if(nextStates[ix,'NextTeam'] == 'Defense'){
        #For the states where the defense takes the ball, we pull the utility 
        #   by using the semiterminal utility values
        nextStates[ix,'StateUtil'] = staticTurnOverStates[nextStates[ix,'LOS']]
      }
      else{
        #If its a scoring state, the utility is the points!
        nextStates[ix,'StateUtil'] = nextStates[ix,'SCORE']
      }
    }
    
    #Pull the right column to use for probabilities
    
    #Find overall action utility
    if(curPlay == 'RUN') probCol = nextStates$RunProb
    else probCol = nextStates$PassProb
    
    #Take that weighted sum
    actionUtil = sum(probCol * nextStates$StateUtil) 
    actionStore[playIx,curDown,curDist,curLOS] <<- actionUtil
    return(actionUtil)
  } 
}


#Test it out!
findStateUtil(3,1,1)










