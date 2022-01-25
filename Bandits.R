###########
# LIBRERIE
###########

rm(list = ls())
dir.work <- "./"
setwd(dir.work)
library(tidyverse)
source('./pullArm.R')

####################################
# NUMERO DI ARMS E NUMERO DI TRIALS
####################################

sdtart <- -10L
nArmsEachGame <- 4L
numTrials <- 10000L 

#######################
# NOMI DEGLI ALGORITMI
#######################

ArmEpsGreedy <- "ArmEpsGr"
ArmUCB1 <- "UCB1"
ArmThompson <- "ArmThm"

if(exists(ArmUCB1)){
  rm(list = c(ArmUCB1))
}

if(exists(ArmEpsGreedy)){
  rm(list = c(ArmEpsGreedy))
}

if(exists(ArmThompson)){
  rm(list = c(ArmThompson))
}

###############################
# INIZIALIZZAZIONE DEI BANDITI
###############################

for(j in seq_len(nArmsEachGame)){
  tmp <- pullArm(namePull = ArmEpsGreedy,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart) 
  
  tmp <- pullArm(namePull = ArmUCB1,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  tmp <- pullArm(namePull = ArmThompson,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart)
}

######################################################################
# NUMERO DI VOLTE CHE E' STATA TIRATA OGNI ARM E POSTERIOR AGGIORNATA
######################################################################

Times_Pulled <- c(1,1,1,1)
alpha_t <- (get(ArmThompson)$sequence)$rewards + 1
beta_t <- 1 - (get(ArmThompson)$sequence)$rewards + 1

###########################
# ALGORITMO VERO E PROPRIO
###########################

for(j in seq_len(numTrials)){
  
  if(j %% 1000 == 0){
    cat("Num trials ",j,"out of",numTrials,"\n")
  }
  
  exploration <- runif(1) < 16/j
  
  ############
  # EPSGREEDY
  ############
  
  if(exploration){
    tmp <- pullArm(namePull = ArmEpsGreedy,
                   nArm = sample.int(nArmsEachGame,size = 1),
                   nPlayGame = 1,
                   seed_start = sdtart) 
  } else {
    bestSoFar <- get(ArmEpsGreedy)$stats %>% 
      filter(meanArmSoFar == max(meanArmSoFar)) 
    
    tmp <- pullArm(namePull = ArmEpsGreedy,
                   nArm = bestSoFar$numArm,
                   nPlayGame = 1,
                   seed_start = sdtart)
  }
  
  #######
  # UCB1
  #######
  
  alpha <- 0.5
  mu <- (get(ArmUCB1)$stats)$meanArmSoFar
  
  ucb <- mu + alpha * sqrt(2*log(j)/Times_Pulled)
  
  argucb <- which(ucb == max(ucb))
  
  tmp <- pullArm(namePull = ArmUCB1,
                 nArm = argucb,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  Times_Pulled[argucb] <- Times_Pulled[argucb] + 1
  
  ###########
  # THOMPSON
  ###########
  
  theta <- c(0,0,0,0)
  
  for (i in 1:nArmsEachGame){
    theta[i] <- rbeta(n = 1, shape1 = alpha_t[i], shape2 = beta_t[i])
  }
  
  arg_th <- which(theta == max(theta))
  
  tmp <- pullArm(namePull = ArmThompson,
                 nArm = arg_th,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  npl <- (get(ArmThompson)$stats)$numPlayed[arg_th]
  masf <- (get(ArmThompson)$stats)$meanArmSoFar[arg_th]
  alpha_t[arg_th] <- masf * npl + alpha_t[arg_th]
  beta_t[arg_th] <- npl * (1 - masf) + beta_t[arg_th]
  
}

############
# RESOCONTI
############

get(ArmEpsGreedy)
get(ArmUCB1)
get(ArmThompson)

######################
# PREPARAZIONE E PLOT
######################

seqEpdGreedy <- get(ArmEpsGreedy)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(rewards)/numGame) %>%
  add_column(strategy = ArmEpsGreedy)

seqUCB1 <- get(ArmUCB1)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(rewards)/numGame) %>%
  add_column(strategy = ArmUCB1)

seqThm <- get(ArmThompson)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(rewards)/numGame) %>%
  add_column(strategy = ArmThompson)


dataTimeSeries <- bind_rows(seqEpdGreedy, seqUCB1, seqThm) %>%
  mutate(numGame = numGame -nArmsEachGame) %>%
  filter(numGame > 0) 


ggplot(data = dataTimeSeries, mapping = aes(x = numGame,
                                           y = meanRewards)) + 
  geom_line(aes(color = strategy, linetype = strategy)) +
  ylab("mean of Rewards")+
  xlab("number of games")

################################################################################
# INTOSSICAZIONI
################################################################################

rm(list = ls())
dir.work <- "./"
setwd(dir.work)
library(tidyverse)

################################
# PULLARM CON REWARD MODIFICATA
################################

pullArmInt <- function(namePull = "Pull1",nArm = 1L,seed_start = 0L, nPlayGame = 1){
  if(!exists(namePull,envir = .GlobalEnv)){
    assign(namePull, list(sequence = tibble(numArm = integer(),
                                            rewards = numeric()),
                          stats = tibble(
                            numArm = integer(),
                            meanArmSoFar = numeric(),
                            numPlayed = integer()
                          )) , envir = .GlobalEnv)
  }
  
  if(nPlayGame>0){
    Pull <- get(namePull,envir = .GlobalEnv)
    HistTN <- filter(Pull$stats,numArm == nArm)
    
    if(HistTN %>% nrow()==0 ){
      Pull$stats <- add_row(Pull$stats,numArm=nArm,meanArmSoFar=0.0,numPlayed=0L)
      HistTN <- filter(Pull$stats,numArm == nArm)
    }
    
    switch (nArm,
            {a=8
            b=12},
            {a=1
            b=1},
            {a=0.05
            b=0.05},
            {a=.6
            b=.4}
    )
    
    set.seed(seed = 1e7*nArm + HistTN$numPlayed + seed_start)
    rewards <- rbeta(n = nPlayGame,shape1 = a,shape2 = b)
    
    # CAMBIO DI REWARD
    rewards <- ifelse(rewards < 0.2, 0, 1)
    
    Pull$sequence <- bind_rows(Pull$sequence,
                               tibble(numArm = nArm,
                                      rewards = rewards))
    
    sumRewSofar <- sum(rewards,HistTN$meanArmSoFar*HistTN$numPlayed)
    Pull$stats <- Pull$stats %>% mutate(
      numPlayed = case_when(
        numArm == nArm ~ numPlayed + nPlayGame,
        TRUE ~ numPlayed + 0),
      meanArmSoFar = case_when(
        numArm == nArm ~ sumRewSofar/numPlayed,
        TRUE ~ meanArmSoFar))
    
    assign(namePull, Pull , envir = .GlobalEnv)
    return(rewards)
  }
  
  return()
}

####################################
# NUMERO DI ARMS E NUMERO DI TRIALS
####################################

sdtart <- -10L 
nArmsEachGame <- 4L
numTrials <- 10000L 

#######################
# NOMI DEGLI ALGORITMI
#######################

ArmEpsGreedy <- "ArmEpsGr"
ArmUCB1 <- "UCB1"
ArmThompson <- "ArmThm"

if(exists(ArmUCB1)){
  rm(list = c(ArmUCB1))
}

if(exists(ArmEpsGreedy)){
  rm(list = c(ArmEpsGreedy))
}

if(exists(ArmThompson)){
  rm(list = c(ArmThompson))
}

###############################
# INIZIALIZZAZIONE DEI BANDITI
###############################

for(j in seq_len(nArmsEachGame)){
  tmp <- pullArmInt(namePull = ArmEpsGreedy,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart) 
  
  tmp <- pullArmInt(namePull = ArmUCB1,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  tmp <- pullArmInt(namePull = ArmThompson,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart)
}

######################################################################
# NUMERO DI VOLTE CHE E' STATA TIRATA OGNI ARM E POSTERIOR AGGIORNATA
######################################################################

Times_Pulled <- c(1,1,1,1)
alpha_t <- (get(ArmThompson)$sequence)$rewards + 1
beta_t <- 1 - (get(ArmThompson)$sequence)$rewards + 1

###########################
# ALGORITMO VERO E PROPRIO
###########################

for(j in seq_len(numTrials)){
  
  if(j %% 1000 == 0){
    cat("Num trials ",j,"out of",numTrials,"\n")
  }
  
  exploration <- runif(1) < 16/j
  
  ############
  # EPSGREEDY
  ############
  
  if(exploration){
    tmp <- pullArmInt(namePull = ArmEpsGreedy,
                   nArm = sample.int(nArmsEachGame,size = 1),
                   nPlayGame = 1,
                   seed_start = sdtart) 
  } else {
    bestSoFar <- get(ArmEpsGreedy)$stats %>% 
      filter(meanArmSoFar == max(meanArmSoFar)) 
    
    tmp <- pullArmInt(namePull = ArmEpsGreedy,
                   nArm = bestSoFar$numArm,
                   nPlayGame = 1,
                   seed_start = sdtart)
  }
  
  #######
  # UCB1
  #######
  
  alpha <- 0.5
  mu <- (get(ArmUCB1)$stats)$meanArmSoFar
  
  ucb <- mu + alpha * sqrt(2*log(j)/Times_Pulled)
  
  argucb <- which(ucb == max(ucb))[1]
  
  tmp <- pullArmInt(namePull = ArmUCB1,
                 nArm = argucb,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  Times_Pulled[argucb] <- Times_Pulled[argucb] + 1
  
  
  ###########
  # THOMPSON
  ###########
  
  theta <- c(0,0,0,0)
  
  for (i in 1:nArmsEachGame){
    theta[i] <- rbeta(n = 1, shape1 = alpha_t[i], shape2 = beta_t[i])
  }
  
  arg_th <- which(theta == max(theta))
  
  tmp <- pullArmInt(namePull = ArmThompson,
                 nArm = arg_th,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  npl <- (get(ArmThompson)$stats)$numPlayed[arg_th]
  masf <- (get(ArmThompson)$stats)$meanArmSoFar[arg_th]
  alpha_t[arg_th] <- masf * npl + alpha_t[arg_th]
  beta_t[arg_th] <- npl * (1 - masf) + beta_t[arg_th]
  
}

############
# RESOCONTI
############

get(ArmEpsGreedy)
get(ArmUCB1)
get(ArmThompson)


######################
# PREPARAZIONE E PLOT
######################

seqEpdGreedy <- get(ArmEpsGreedy)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(1-rewards)/numGame) %>%
  add_column(strategy = ArmEpsGreedy)

seqUCB1 <- get(ArmUCB1)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(1-rewards)/numGame) %>%
  add_column(strategy = ArmUCB1)

seqThm <- get(ArmThompson)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(1-rewards)/numGame) %>%
  add_column(strategy = ArmThompson)


dataTimeSeries <- bind_rows(seqEpdGreedy, seqUCB1, seqThm) %>%
  mutate(numGame = numGame -nArmsEachGame) %>%
  filter(numGame > 0) 


ggplot(data = dataTimeSeries, mapping = aes(x = numGame,
                                            y = meanRewards)) + 
  geom_line(aes(color = strategy, linetype = strategy)) +
  ylab("mean of Intoxicated")+
  xlab("number of trials")









