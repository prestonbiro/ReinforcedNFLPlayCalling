#TutorialFindTransitionProbs
#Author: Preston Biro

#This is a tutorial for Biro and Walker's Reinforcement Learning for Football 
#     Play Calling Paper, or whatever it gets titled in the end

#This is the second script in a set of three (or maybe four or five, not sure 
#     yet) that will help you optimize play calling for a football team using
#     NFL data.

#This script will do work that isn't explicitly described in the paper, but 
#     potentially the most critical task: finding the transition probabilities
#     needed to connect the states.

#The methods used in this paper are no longer in use by the author for the 
#     newer applications, as better models have been decided upon for individual
#     play modeling. However, we think the methods used for this paper, and in
#     this code are reasonable approximates. When the next stage of the research
#     is published, the newer methods will be provided for transition
#     probability modeling.

#The modeling in this paper can be divided between run and pass plays. 
#   For run plays, we fit a subset of the most relevant data points using a 
#     hierarchical mixture of four normal distributions, with the means coming 
#     from a N(0,100) prior, weights coming from Dirichlet(1,1,1,1) prior, and
#     variance coming from a InvGamma(1,1) prior. 
#   For pass plays, we fit a subset of the most relevant data points using a 
#     mixture of discrete and continuous distributions, where the incompletions,
#     interceptions, and touchdowns are considered point masses and positive 
#     plays are modeled using a conjugate gamma-gamma distribution with a 
#     known shape parameter of alpha = 1.3 and unknown rate parameter, and 
#     negative plays are modeled as discrete points from -1 to -24 using a 
#     dirichlet-multinomial conjugate distribution with a Dir(10*tau_i) prior, 
#     where the tau_i's are the relative frequency of observing a i yard loss on
#     any particular pass play (more or less empirical bayes prior).
#   Specifications for the posterior distributions are given in the arXiv 
#     version of the paper. 


#Let's start by loading the libraries and previous scripts we need

#Set your working directory properly, and run the previous script
setwd('~/GithubTutorial')
source('TutorialHelperFunctions.R')

#One variable needed, and its a big one. Will be discussed once we get to the 
#   getAndGiveProb function
probArray = array(NA,dim = c(2,4,99,99,201))

#The following link is where I got the numbers for lost fumble rate on pass and
#   run plays respectively. This is used for turnover probabilities. In general
#   there's not a significant increase in fumble probability related to down or
#   distance, although there is a slight relationship with LOS, it was not 
#   put into this modeling for now
#   http://archive.advancedfootballanalytics.com/2010/01/fumble-rates-by-play-type.html#:~:text=55%25%20of%20run%20play%20fumbles,on%202.04%25%20of%20pass%20plays.
fumbleRunRate = .0065
fumblePassRate = .0097

### Pass Probability Functions
#First, here's a few functions that help partition the data for pass plays

partitionPasses <- function(passPlayData,los,subPart = FALSE){
  #Takes the raw data (or already separated data in case of subPart = F) and 
  #   assigns a 'playResult' tag to it in which the play result can either be 
  #   Touchdown, Interception, Incomplete Pass, Positive Gain (not including 
  #   Touchdown), and Negative Loss (not including interceptions and 
  #   incompletions). Used later in computation of multinomial probabilities
  
  #First option used for subclassifying to find the smoother probabilities for 
  #   hyperparameters, second only for assigning playResult
  if(subPart) newFrame = passPlayData[passPlayData$LOS == los,]
  else newFrame = passPlayData
  
  #Assigns Int, TD, Inc, Neg, or Pos, no overlap
  newFrame$playResult = NA
  newFrame[newFrame$INTERCEPTION == 1,'playResult'] <- 'Int'
  newFrame[(newFrame$INTERCEPTION == 0) & (newFrame$GAIN >= los),
           'playResult'] <- 'TD'
  newFrame[(newFrame$INTERCEPTION == 0) & (newFrame$INCOMPLETE == 1),
           'playResult'] <- 'Inc'
  newFrame[(newFrame$INTERCEPTION == 0) & (newFrame$GAIN <= 0) & 
             (newFrame$INCOMPLETE == 0),'playResult'] <- 'Neg'
  newFrame[(newFrame$GAIN > 0) & (newFrame$GAIN < los),
           'playResult'] <- 'Pos'
  return(newFrame)
}

probSplits <- function(data,los,subSplit = F){
  #Creates a list of probabilities for a particular set of data, breaking down 
  #the probabilities of each potential play result
  
  #Splits data and assigns results
  passPart = partitionPasses(data,los,subSplit)
  
  #Only looks at play result once split
  subdata = passPart$playResult
  
  #Find relative frequency of each respective event
  N = length(subdata)
  pInc = sum(subdata == 'Inc')/N
  pInt = sum(subdata == 'Int')/N
  pTD = sum(subdata == 'TD')/N
  pPos = sum(subdata == 'Pos')/N
  pNeg = sum(subdata == 'Neg')/N
  return(list(X = los,Int = pInt,Inc = pInc,TD = pTD,Pos = pPos,Neg = pNeg))
}

passSetup <- function(allData){
  #Sets up global variables used for default parameters in functions. 
  #   Isn't <<- a cool trick??? Why didn't anyone tell me about it sooner
  passdata = allData[!is.na(allData$PLAYTYPE) & (allData$PLAYTYPE == 'pass'),
                     c('PLAYTYPE','LOS','GAIN','INTERCEPTION',
                       'INCOMPLETE')]
  
  #Identifies sack plays... sorta. Trying to get a distribution of what regular 
  #   'sack' plays look like. Don't actually use the sack variable, but look at 
  #   negative yardage pass plays. Care more about the distribution of negative 
  #   passplays than that of sacks for the case of this model. Therefore this 
  #   shouldnt actually be used for an actual model for sack plays, although 
  #   there's a good chance that it mimics the model somewhat
  allSacks = passdata[(passdata$LOS > 10) & 
                        (passdata$LOS < 90) & 
                        (passdata$GAIN <= 0) & 
                        (passdata$INCOMPLETE == 0) & 
                        (passdata$INTERCEPTION == 0),'GAIN']
  
  #Find the overall frequentist probability of each negative play yardage, will 
  #   use as a empirical bayes prior
  sackProbsTab = table(allSacks)/sum(table(allSacks))
  sackNames <- strtoi(names(sackProbsTab))
  sackProbs <<- data.frame(x = numeric(0),p = numeric(0))
  for(i in 0:24) {
    sackProbs <<- rbind(list(x = -i,
                             p = ifelse(-i %in% sackNames,
                                        sackProbsTab[[as.character(-i)]],
                                        sackProbsTab[[as.character(-i-1)]])),
                        sackProbs)
  }
  sackProbs$p <<- sackProbs$p/sum(sackProbs$p)
  
  #For pass plays, find the hyperparameters for each play type occurring, 
  #   smoothed over of course
  probPassPlays = data.frame(X = numeric(0),Inc = numeric(0),
                             Int = numeric(0),TD = numeric(0),
                             Pos = numeric(0),Neg = numeric(0))
  for(l in 1:99) probPassPlays <- rbind(probPassPlays,
                                        probSplits(passdata,l,subSplit = T))
  smInt <- loess(Int~X,data = probPassPlays,span = .5)
  smInc <- loess(Inc~X,data = probPassPlays,span = .5)
  smTD <- loess(TD~X,data = probPassPlays,span = .5)
  smPos <- loess(Pos~X,data = probPassPlays,span = .5)
  smNeg <- loess(Neg~X,data = probPassPlays,span = .5)
  
  #Store and release for the world to use
  smoothProbs <<- data.frame(Inc = predict(smInc),Int = predict(smInt),
                             TD = predict(smTD),Pos = predict(smPos),
                             Neg = predict(smNeg))
  smoothProbs[smoothProbs<0] <<- 0
}

#Runs previous function, setting up the parameters for pass distribution draws
passSetup(smalldata)
#The big output from this is the smoothProbs data frame, which gives the prior
#   probabilities that will be used for an incomplete pass, interception, 
#   touchdown, positive gain, and negative gain options for each line of 
#   scrimmage option. For example, for a play from the 10 yardline, we expect
#   an incompletion percentage (or no gain percentage) of .3931351, touchdown
#   percentage of .241624428, and so on based off smoothProbs[10,]

#Two more helper functions for passes, doing the fitting of the data for 
#   positive and negative pass plays separately. Positive is a fitting of a 
#   conjugate gamma-gamma dist with unknown rate parameter
posPassDist <- function(posPass,los){
  #Calculates the probability of each positive yardage gain for a pass play, 
  #   excluding touchdowns
  
  #Cant gain yards on the 1 yard line, except TDs
  if(los == 1) return(NULL)
  else{
    #Setup hyperparameters for a gamma conjugate distribution with known shape 
    #   alpha. Can be interpreted as observing 100 observations that sum to 
    #   1300, or an average of 13 yards per positive pass play that isnt a 
    #   touchdown. On average, this yields a very slight overestimation on the 
    #   order of about .2 yards. The extreme estimates range are only about 1.5 
    #   yards over or under estimating, and only occur in the middle of the 
    #   field, never the goal lines, although the the negative goal line always 
    #   tends to underestimate on the range of about .5-.8 yards. These 
    #   estimates are much better than the bias and variance in estimations we 
    #   see in others, including the Gibbs Sampler we created on a previous 
    #   version. The errors are on a less extreme order than we see with run 
    #   plays. More updated modeling techniques have been created for this data
    #   and will be discussed and shown in a future version of this paper and 
    #   code. Stay tuned!
    avgYds = 10
    alpha = 1.3
    alpha0 = alpha * 100
    beta0 = alpha0 * avgYds
    
    #Draw unknown beta parameter
    betaDraw <- rgamma(1000,alpha0 + length(posPass) * alpha,
                       beta0 + sum(posPass))
    
    #Find individual probabilities
    prob = matrix(NA,nrow=1000,ncol=los-1)
    for(i in 1:1000){
      b = betaDraw[i]
      probs <- dgamma(1:(los-1),alpha,b)
      prob[i,] <- probs/sum(probs)
    }
    #Average and return
    finalProbs = colMeans(prob)
    finalProbs = finalProbs/sum(finalProbs)
    return(data.frame(X = 1:(los-1),Prob = finalProbs))
  }
}

#Negative plays we treat different, and to be honest in a weird way that I don't
#   particularly love. We treat each potential negative gain more or less as a 
#   categorical variable and fit a dirichlet-multinomial conjugate model to 
#   find the posterior. This is obviously not a natural method to doing this, 
#   but there's just not a distribution class that I could find that could fit 
#   the data better, at least not until I came back to the modeling like a year
#   later. Again, this will be improved later, but lets work with this for now.
negPassDist <- function(negPass,los,negPrior = sackProbs){
  #Calculates the probability of each negative yardage loss for a pass play, 
  #   excluding interceptions and incompletions
  
  #Truncate this boy so we can use our prior
  minLoss = max(los - 100,min(negPrior$x))
  
  #Setup hyperparameters for dirchlet multinomial conjugate draw
  hyperParams = negPrior[negPrior$x >= minLoss,] 
  
  #Find the observed statistics for the draws
  obsStats = data.frame(x = numeric(0),freq = numeric(0))
  for(y in 0:minLoss) obsStats <- rbind(obsStats,
                                        list(x = y,freq = sum(negPass == y)))
  bigFrame = join(hyperParams,obsStats,by='x')
  
  #Draw baby draw
  negProbs = colMeans(rdirichlet(1000,10 * bigFrame$p + bigFrame$freq))
  return(data.frame(X = minLoss:0,Prob = negProbs))
}


#This function actual gets us a posterior probability estimation. We don't put
#   in the DOWN,DIST,LOS tuple because we are putting in directly the matched
#   plays for the state in question. We do give it the LOS because its needed
#   to figure out which row to pull out of smoothProbs
passProbGen <- function(dataPass,los,priorProbs = smoothProbs){
  #Puts it all together so you can get actual passing distributions
  
  #Get the data we want, doesn't get rid of the wiggle room since that part
  #   is taken care of by default
  subData = partitionPasses(dataPass,los)
  
  #Setup those pretty hyperparameters, give a weighting by 10 (prior worth 
  #   approximately 10 real plays)
  hyperParams = priorProbs[los,] * 10
  
  #Get observed values
  freqTab = table(subData$playResult)
  
  #Posterior parameters in dirichlet-multinomial 
  postParams = list(Inc = hyperParams$Inc + ifelse('Inc' %in% names(freqTab),
                                                   freqTab[['Inc']],0),
                    Int = hyperParams$Int + ifelse('Int' %in% names(freqTab),
                                                   freqTab[['Int']],0),
                    TD  = hyperParams$TD  + ifelse('TD' %in% names(freqTab),
                                                   freqTab[['TD']],0),
                    Pos = hyperParams$Pos + ifelse('Pos' %in% names(freqTab),
                                                   freqTab[['Pos']],0),
                    Neg = hyperParams$Neg + ifelse('Neg' %in% names(freqTab),
                                                   freqTab[['Neg']],0))
  
  #Gets posterior probability of each play result occuring via averaging 
  #   pulls from the posterior
  passProbs = colMeans(rdirichlet(1000,unlist(postParams)))
  
  #Ignores computation of positive probs if on the 1 yard line (can't have a 
  #   positive gain on the 1 without scoring a touchdown)
  if(los > 1) {
    posProbs <- posPassDist(subData[subData$playResult == 'Pos','GAIN'],los)
  }
  else posProbs = data.frame(X = numeric(0),Prob = numeric(0))
  
  #Weights the conditional probabilities by the probability of the condition 
  #   occurring to get overall value
  posProbs$Prob = passProbs[4] * posProbs$Prob
  
  #Same process for negative plays, but easier and maybe even more fun if you 
  #   think about it
  negProbs <- negPassDist(subData[subData$playResult == 'Neg','GAIN'],los)
  negProbs$Prob = passProbs[5] * negProbs$Prob
  
  #Put it together, getting those incompletions in there as more 0 yard plays
  #Btw, 0 yard plays that aren't incompletions are calculated within the 
  #   negative stuff, and then just summed with the incompletion rate to get the 
  #   full probability for a 0 yard play
  fullPass = rbind(negProbs,posProbs)
  fullPass[fullPass$X == 0,'Prob'] = fullPass[fullPass$X == 0,'Prob'] + 
    passProbs[1]
  
  #TD probability added in
  fullPass <- rbind(fullPass,list(X = los,Prob = passProbs[3]))
  
  #Add in missing yardlines as 0s
  for(yl in (-100 + los):los){
    if(!(yl %in% fullPass$X)) fullPass <- rbind(fullPass,list(X = yl,Prob = 0))
  }
  
  #Interceptions marked as -200 yard plays, which is how QBs should consider 
  #   throwing a pick. Looking at you Jameis
  fullPass <- rbind(list(X = -200,Prob = passProbs[2]),fullPass)
  return(fullPass[order(fullPass$X),])
}

### Run Probability Functions
#Runs are smoother, and therefore easier to deal with. So we just have one 
#   function that fits the given data with a mixture of four normals using 
#   a Gibbs sampler
runProbGen <- function(dataY,LOS){
  #Fits a mixture of 4 normal distributions with a common variance
  n = length(dataY)
  
  #Set up a sequence for plots
  ySeq = (LOS-100):LOS
  
  #Set initial values, fixed but could be random
  Mu1 = 0 
  Mu2 = 5 
  Mu3 = 10
  Mu4 = -5
  Lambda = rgamma(1,1,1)
  W = c(.25,.25,.25,.25)
  
  #Burn like 10% or so, store the rest in arrays
  Burn = 200
  Store = 2000
  
  #Setup Storage
  densityStore = matrix(NA,nrow=Store,ncol=101)
  
  for(i in 1:Burn){
    #Find the probability of each point falling in each distribution
    p1 = W[1] * dnorm(dataY,Mu1,sqrt(1/Lambda))
    p2 = W[2] * dnorm(dataY,Mu2,sqrt(1/Lambda))
    p3 = W[3] * dnorm(dataY,Mu3,sqrt(1/Lambda))
    p4 = W[4] * dnorm(dataY,Mu4,sqrt(1/Lambda))
    
    #Reassign points to distributions by sampling
    cp = matrix(c(p1,p2,p3,p4),ncol=4)
    cp = cp/rowSums(cp)
    cp[is.na(cp)] <- 1E-6
    d = apply(cp,1,rmultinom,n=1,size=1)
    
    #Lets count up the groups
    dIx = which(d==1,1)
    d1 = dIx[dIx[,1] == 1,2]
    d2 = dIx[dIx[,1] == 2,2]
    d3 = dIx[dIx[,1] == 3,2]
    d4 = dIx[dIx[,1] == 4,2]
    
    #Separate data points by their groups
    y1 = dataY[d1]
    y2 = dataY[d2]
    y3 = dataY[d3]
    y4 = dataY[d4]
    
    n1 = length(y1)
    n2 = length(y2)
    n3 = length(y3)
    n4 = length(y4)
    
    nybar1 = sum(y1)
    nybar2 = sum(y2)
    nybar3 = sum(y3)
    nybar4 = sum(y4)
    
    #Resample the means based off the points in their groups
    Mu1 = rnorm(1,nybar1 * Lambda/(.01 + n1*Lambda),sqrt(1/(.01 + n1 * Lambda)))
    Mu2 = rnorm(1,nybar2 * Lambda/(.01 + n2*Lambda),sqrt(1/(.01 + n2 * Lambda)))
    Mu3 = rnorm(1,nybar3 * Lambda/(.01 + n3*Lambda),sqrt(1/(.01 + n3 * Lambda)))
    Mu4 = rnorm(1,nybar4 * Lambda/(.01 + n4*Lambda),sqrt(1/(.01 + n4 * Lambda)))
    
    #Resample that precision baby
    Lambda = rgamma(1,1 + n/2, 1 + 1/2 * (sum((y1 - Mu1)^2) + 
                                            sum((y2 - Mu2)^2) + 
                                            sum((y3 - Mu3)^2) + 
                                            sum((y4 - Mu4)^2)))
    
    #Dont forget those sweet sweet scaling probability terms
    W = rdirichlet(1,c(n1+1,n2+1,n3+1,n4+1))
  }
  
  #Repeat but lets keep them around now!
  for(i in 1:Store){
    p1 = W[1] * dnorm(dataY,Mu1,sqrt(1/Lambda))
    p2 = W[2] * dnorm(dataY,Mu2,sqrt(1/Lambda))
    p3 = W[3] * dnorm(dataY,Mu3,sqrt(1/Lambda))
    p4 = W[4] * dnorm(dataY,Mu4,sqrt(1/Lambda))
    
    cp = matrix(c(p1,p2,p3,p4),ncol=4)
    cp = cp/rowSums(cp)
    d = apply(cp,1,rmultinom,n=1,size=1)
    
    dIx = which(d==1,1)
    d1 = dIx[dIx[,1] == 1,2]
    d2 = dIx[dIx[,1] == 2,2]
    d3 = dIx[dIx[,1] == 3,2]
    d4 = dIx[dIx[,1] == 4,2]
    
    y1 = dataY[d1]
    y2 = dataY[d2]
    y3 = dataY[d3]
    y4 = dataY[d4]
    
    n1 = length(y1)
    n2 = length(y2)
    n3 = length(y3)
    n4 = length(y4)
    
    nybar1 = sum(y1)
    nybar2 = sum(y2)
    nybar3 = sum(y3)
    nybar4 = sum(y4)
    
    Mu1 = rnorm(1,nybar1 * Lambda/(.01 + n1*Lambda),sqrt(1/(.01 + n1 * Lambda)))
    Mu2 = rnorm(1,nybar2 * Lambda/(.01 + n2*Lambda),sqrt(1/(.01 + n2 * Lambda)))
    Mu3 = rnorm(1,nybar3 * Lambda/(.01 + n3*Lambda),sqrt(1/(.01 + n3 * Lambda)))
    Mu4 = rnorm(1,nybar4 * Lambda/(.01 + n4*Lambda),sqrt(1/(.01 + n4 * Lambda)))
    
    Lambda = rgamma(1,1 + n/2, 1 + 1/2 * (sum((y1 - Mu1)^2) + 
                                            sum((y2 - Mu2)^2) + 
                                            sum((y3 - Mu3)^2) + 
                                            sum((y4 - Mu4)^2)))
    
    W = rdirichlet(1,c(n1+1,n2+1,n3+1,n4+1))
    
    #Store the predicitve density at each iteration
    densityStore[i,] = sapply(1:101,function(t) W[1]*dnorm(ySeq[t],Mu1,
                                                           sqrt(1/Lambda)) + 
                                W[2]*dnorm(ySeq[t],Mu2,sqrt(1/Lambda)) + 
                                W[3]*dnorm(ySeq[t],Mu3,sqrt(1/Lambda)) + 
                                W[4]*dnorm(ySeq[t],Mu4,sqrt(1/Lambda)))
  }
  
  #Lets average the densities to get some probabilities
  mixDens = colMeans(densityStore)
  mixDens = mixDens/sum(mixDens)
  
  li <- list(df = data.frame(X = ySeq,Prob = mixDens),
             hi = dataY)
  return(li)
}

#The function that combines run and pass draw methods to bring it all together. 
#   Spits out the probability of a play gaining or losing a specific number of 
#   yards for a given DOWN, DIST, and LOS, and the corresponding action.
findProbs <- function(DOWN,DIST,LOS,PLAYTYPE){
  #Finds the probability of every event occurring in a pass or run play.
  #Spits out two things: 
  #   First, and most importantly a dataframe with the 
  #     number of yards gained (or lost) listed as X and the probability of that
  #     particular outcome occurring as the Prob column
  #   Second, a list of adjusted yards gained used for fitting the posterior.
  #     This is helpful because you can plot and check that the fit makes sense,
  #     using these values to create a histogram.
  
  #Finds at least 100 plays to use in the posterior in the matching process.
  dataList = singlePlayMatch(DOWN,DIST,LOS,PLAYTYPE)
  
  #Pulls based off the method for each playtype
  if(PLAYTYPE == 'pass'){
    return(list(df = passProbGen(dataList,LOS),hi = dataList$adjYardsGained))
  }
  else{
    return(runProbGen(dataList$adjYardsGained,LOS))
  }
}

#Try it out!
findProbs(1,10,80,'run')
findProbs(2,1,25,'pass')

# #Here's some help plotting to check
# down = 1
# dist = 10
# los = 80
# act = 'run' #'pass'
# 
# exFit = findProbs(down,dist,los,act)
# hist(exFit$hi,freq = F,breaks = 20,xlim = c(los-100,los),xlab = 'Yards Gained',
#      main = paste0('Distribution of Yards Gained for \nDOWN = ',down,
#                    ', DIST = ',dist,', LOS = ',los,
#                    ', \nAction = ',ifelse(act == 'run','Run','Pass')))
# points(exFit$df,col = 'red')
# 
# down = 2
# dist = 1
# los = 20
# act = 'pass'
# 
# exFit = findProbs(down,dist,los,act)
# hist(exFit$hi,freq = F,breaks = 20,xlim = c(los-100,los),xlab = 'Yards Gained',
#      main = paste0('Distribution of Yards Gained for \nDOWN = ',down,
#                    ', DIST = ',dist,', LOS = ',los,
#                    ', \nAction = ',ifelse(act == 'run','Run','Pass')))
# points(exFit$df,col = 'red')


#This function helps make adjustments for turnovers, giving us a distribution
#   of the yardline where the takeaway team will receive the ball based off the
#   line of scrimmage of the play.
findNegPlayProbs <- function(curLOS,actionChoice){
  #Gets the estimated probability of giving the defense the ball at each yard 
  #   line, given the current LOS. Values are from the offensive team's 
  #   perspective to keep in line with offensive probabilities
  
  #Here, we make the assumption that fumbles are recovered in roughly the same 
  #   distribution on pass and run plays, which is obviously false, as pass 
  #   fumbles are often strip sacks and more likely to be behind he LOS, and run
  #   fumbles are probably normally distributed centered a bit beyond the LOS. 
  #   However, theres not a ton of fumble data available, so we assume they have
  #   the same dist and work with that
  
  #That said, we do not assume rate of fumble is the same on run or pass, but 
  #   rather assume the rates that are given by constants at top of this file
  
  #We assume that the recovery location of a fumble is normal with a standard 
  #   deviation of 10 yards centered around the line of scrimmage. We assume
  #   an interception is also normal centered around the line of scrimmage, but
  #   has a standard deviation of 25 yards. These were determined by examination
  #   of the data and while we know these aren't going to be the most accurate
  #   estimates, we do believe they are sufficient to not be too far from the 
  #   true distribution. For the edge points (touchbacks and return touchdowns),
  #   we assume the probability is just the sum of the probabilities more 
  #   extreme than their actual yard line results, which again seem to match the
  #   true data fairly well.
  x = 0:100
  if(actionChoice == 'pass'){
    fumbleProbs = dnorm(x,mean = curLOS,sd = 10)
    intProbs = dnorm(x,mean = curLOS,sd = 25)
    fumbleProbs[1] = pnorm(0,mean = curLOS,sd = 10)
    fumbleProbs[101] = pnorm(100,mean = curLOS,sd = 10,lower.tail = F)
    intProbs[1] = pnorm(0,mean = curLOS,sd = 25)
    intProbs[101] = pnorm(100,mean = curLOS,sd = 25,lower.tail = F)
    totProbs = fumbleProbs + intProbs
  }
  else if(actionChoice == 'run'){
    fumbleProbs = dnorm(x,mean = curLOS,sd = 10)
    fumbleProbs[1] = pnorm(0,mean = curLOS,sd = 10)
    fumbleProbs[101] = pnorm(100,mean = curLOS,sd = 10,lower.tail = F)
    totProbs = fumbleProbs
  }
  df = data.frame(LOS = rev(x), Prob = totProbs)
  #Adjust touchbacks LOS
  df[df$LOS == 80,'Prob'] = df[df$LOS == 80,'Prob'] + df[df$LOS == 100,'Prob']
  df = df[df$LOS != 100,]
  df$Prob = df$Prob/sum(df$Prob)
  return(df)
}


#Here's where the dynamic programming aspects come in. This function will make 
#   life easier over time, by calculating the probabilities needed for a given
#   state if and only if it has not been calculated previously, and will store
#   the results of those calculations. If they have calculated the probabilities
#   before, the function will simply pull it from storage. 
getAndGiveProb <- function(DOWN,DIST,LOS,reset = F){
  #Will find probabilities if they haven't been calculated previously for a 
  #   specific down, distance, and los. Does include all resulting line of 
  #   scrimmage information for turnovers and stuff
  
  #Prints if there's a big error in input
  if((DOWN > 4) | (DOWN < 1) | (DIST < 1) | (DIST > 99) |(LOS < 1) | 
     (LOS > 99)) print(paste('Error: DOWN=',DOWN,'DIST=',DIST,'LOS=',LOS))
  
  #If we've already calculated the probability, lets grab that
  tempProb = probArray[,DOWN,DIST,LOS,]
  if(any(is.na(tempProb)) | reset){
    tempProbsPass = findProbs(DOWN,DIST,LOS,'pass')$df
    tempProbsRun = findProbs(DOWN,DIST,LOS,'run')$df
    
    intProb = tempProbsPass[tempProbsPass$X == -200,'Prob']
    turnProbPass = intProb + fumblePassRate
    
    negProbsPass = turnProbPass * findNegPlayProbs(LOS,'pass')$Prob
    negProbsRun = fumbleRunRate * findNegPlayProbs(LOS,'run')$Prob
    
    #Positive Numbers are the number of yards away from the end zone, Negative 
    #   Numbers are the number of yards the opponent would be from their own end
    #   zone if they got a turnover returned to that positive line. Thus, the 
    #   positive and negative yard lines are the same line, but owned by 
    #   opposite teams
    
    #0 is Offensive TD
    #100 is safety for offense
    #-100 is defensive TD
    run = data.frame(X = 100:-100,Prob = c(tempProbsRun$Prob,negProbsRun)) 
    pass = data.frame(X = 100:-100,Prob = 
                        c(tempProbsPass[tempProbsPass$X > -200,'Prob'],
                          negProbsPass)) 
    
    #Normalize the probabilities (I don't think this needs to be done, but just
    #   in case)
    run$Prob = run$Prob/sum(run$Prob)
    pass$Prob = pass$Prob/sum(pass$Prob)
    
    probArray[1,DOWN,DIST,LOS,] <<- pass$Prob
    probArray[2,DOWN,DIST,LOS,] <<- run$Prob
    tempProb = probArray[,DOWN,DIST,LOS,]
  }
  return(list(RunProb = data.frame(X = 100:-100,Prob = tempProb[2,]),
              PassProb = data.frame(X = 100:-100,Prob = tempProb[1,])))
}

getAndGiveProb(1,10,80)

#Once you're comfortable with how this function works, you can use the following
#   line to load a bunch of precalculated probabilities so you don't have to 
#   take the time to do it on your own.

# load('ProbArrayStorage.Rdata')

