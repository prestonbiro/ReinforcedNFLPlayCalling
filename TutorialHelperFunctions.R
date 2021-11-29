#TutorialHelperFunctions
#Author: Preston Biro

#This is a tutorial for Biro and Walker's Reinforcement Learning for Football 
#     Play Calling Paper, or whatever it gets titled in the end

#This is the first script in a set of three (or maybe four or five, not sure 
#     yet) that will help you optimize play calling for a football team using
#     NFL data.

#This script will setup a lot of helper functions used to do some of the 
#     background work. I'll walk you through it, but this really isn't the meat
#     of the paper, so you may just want to trust that it all works and move on
#     to the fun stuff in the second and third scripts.

#Let's start by loading the libraries and previous scripts we need
library(MCMCpack)
library(ggplot2)
library(robustbase)
library(plyr)
library(dplyr)
library(MatchIt)

#Set your working directory properly
setwd('~/GithubTutorial')

#Load in the data we need to use
#This data is from NFLScrapR, provided by Yurko, Horowitz, and Ventura
#For full access to NFL PBP Data, see https://github.com/ryurko/nflscrapR-data
fulldata1 = read.csv('Data/reg_pbp_2017.csv')
fulldata2 = read.csv('Data/reg_pbp_2018.csv')

#I put 2017 and 2018 in one big data frame and then delete the individual files
fulldata = rbind(fulldata1,fulldata2)
rm(fulldata1,fulldata2)

#Trims down the data to what is needed for the probability 
#   distribution calculations
smalldata <- fulldata[,c('yardline_100','down','ydstogo','play_type',
                         'yards_gained','interception','incomplete_pass')]
colnames(smalldata) <- c('LOS','DOWN','DIST','PLAYTYPE','GAIN',
                         'INTERCEPTION','INCOMPLETE')
rm(fulldata)

#The first helper function
#This function allows us to put in a specific play (state) as a down, distance,
#   and line of scrimmage, and a corresponding action. The function will then 
#   spit out the plays that are most similar to those plays. Will spit out more
#   plays if there are a lot of very similar plays. This will help us fit a 
#   probability distribution for each state and action combination.
#DOWN can be from 1 to 4
#DIST can be from 1:LOS (with some restrictions at the team's own goal line)
#LOS can be from 1:99
#ACTION can be either 'run' or 'pass'. Typically it can also be punt or fg, but
#     you won't need either of those options for this function
singlePlayMatch <- function(curDown,curDist,curLOS,actionChoice){
  #Make a fake play with the data we want to match, make the other components NA
  playToMatch = list(DOWN = curDown,DIST = curDist,LOS = curLOS,
                     PLAYTYPE = actionChoice,GAIN = NA,
                     INTERCEPTION = NA,INCOMPLETE = NA)
  #Add the fake play to the playlist
  tempFrame = rbind(playToMatch,smalldata)
  #Stealing some stuff from the Causal Inference methodology, doing matching
  #   as if the play we want is a unit being untreated and finding the similar
  #   treated units.
  tempFrame$treat = 0
  tempFrame[1,'treat'] = 1
  #Get rid of all the plays that are issues, and only keep 
  #   the right type of action
  tempFrame = tempFrame[!is.na(tempFrame$DOWN),] %>% 
    filter(PLAYTYPE == actionChoice)
  
  #If its first and 10, there's a lot of options, so lets get more
  #Find all the closest matches by Mahalanobis distance
  if(curDown == 1 & curDist == 10){
    out <- matchit(treat ~ DOWN + DIST + LOS,data = tempFrame,
                   method = 'nearest',distance = 'mahalanobis',ratio = 200)
  }
  else{
    out <- matchit(treat ~ DOWN + DIST + LOS,data = tempFrame,
                   method = 'nearest',distance = 'mahalanobis',ratio = 100)
  }

  #Now that we found the closest matches, lets pull them out
  matchedData = tempFrame[out$match.matrix[1,],]
  #Sometimes there's still weird NA issues, so drop those just in case
  matchedData = matchedData[!is.na(matchedData$DOWN),]
  
  #When you match plays that aren't from the exact same scenarios, you might 
  #   need to adjust their results so they make sense. For example, if you match
  #   a 1st and 10 play from the 10 with a 1st and 10 play from the 11, and your
  #   play from the 11 gained 11 yards, well that's not possible from the 10, so
  #   we adjust that down. Also want to recognize earning a first down.
  maxScale = 1.5
  matchedData$adjYardsGained = matchedData$GAIN * 
    ifelse(curDist / matchedData$DIST > maxScale,
           maxScale,curDist / matchedData$DIST)
  matchedData[matchedData$adjYardsGained > curLOS,'adjYardsGained'] = curLOS
  return(matchedData)
}

#Test it out!
# singlePlayMatch(1,10,40,'run')
# singlePlayMatch(3,5,80,'pass')


#At this point, look at the TutorialFindTransitionProbs.R script.

