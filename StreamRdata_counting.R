###########
# LIBRERIE
###########

rm(list = ls())
library(tidyverse) 
library(car)
Dir.Work <- "C:/Users/Lorenzo/Desktop" 
setwd(Dir.Work)

#############
# DEBUG MODE
#############

debugMode <- FALSE # for debugging / change to false for final

if(debugMode){
  path_Data <- "dataInTiny" 
  real_counts <- "counts_tiny.txt.gz"
} else {
  path_Data <- "dataIn" # for debugging / production
  real_counts <- "counts.txt.gz"
}

#########################################
# PARAMETRI DI LETTURA DEI FILE DI INPUT
#########################################

nFileMax <- 5000
allFiles <- dir(path = path_Data, pattern = "./*.gz",full.names = "T")
nFiles <- min(nFileMax,length(allFiles))

###########################
# PARAMETRI DELL'ALGORITMO
###########################

K <- 5
J <- 1e4

################################
# PARAMETRI DELLE FUNZIONI HASH
################################

hash_params <- matrix(data = c(
  3,1561,
  17,277,
  38,394,
  61,13,
  78,246),byrow = T,nrow = 5,ncol = 2)

################
# MATRICE H E t
################

H <- matrix(data = 0L, nrow = K, ncol = J) 
nTotWords <- 0L

for(j1 in seq_len(nFiles)){
  cat("number ",j1," out of ",nFiles,": ")
  cat("processing file",allFiles[[j1]],"\n")
  streamData <- read_delim(file = allFiles[[j1]],
                           col_names = F,
                           col_types = "i",delim = " ")

  objects_stream <- streamData$X1
  nTotWords <- nTotWords + length(objects_stream)

  for (k in seq_len(K)){
    a <- hash_params[k,1]
    b <- hash_params[k,2]
    
    hashStream <- (((a*objects_stream)+b) %% 123457) %% J
    
    hashSummary <- tibble(ai = hashStream) %>% group_by(ai) %>% count()
    
    H[k,hashSummary$ai+1L] <- H[k,hashSummary$ai+1L] + hashSummary$n 

  }
  
}

###########################################
# CARICO LE FREQUENZE ESATTE DEGLI OGGETTI
###########################################

counts_words <- read_delim(real_counts,
                           "\t", escape_double = FALSE, col_names = FALSE,
                           trim_ws = TRUE,col_types = "ii") 
different_objects <- counts_words$X1 # i = 1, ... , n
Fi <- counts_words$X2 

###############################
# CALCOLO LE FREQUENZE STIMATE
###############################
 
hatFi <- rep(x = Inf,length.out = length(different_objects)) # \hat{F}[i]

for (k in seq_len(K)){
  a <- hash_params[k,1]
  b <- hash_params[k,2]
  
  key <- (((a*different_objects)+b) %% 123457) %% J
  
  hatFi <- pmin(hatFi, H[k,key+1L]) 
  
}

##############
# PLOT FINALE
##############

ggplot(data = tibble(x = Fi, y = (hatFi-Fi)/Fi)) +
  geom_point(mapping = aes(x=x,y=y), colour = "blue", size = .125) +
  scale_y_log10() +
  scale_x_log10() +
  xlab("Object frequency") +
  ylab("Relative error in estimates") +
  annotation_logticks() +
  theme_bw()


################################################################################
# LINEAR MODEL
################################################################################

x <- Fi[which(Fi > 1e4)]
y <- hatFi[which(Fi > 1e4)]

clean <- y - x
x <- x[which(clean > 0)]
y <- y[which(clean > 0)]
y <- (y-x)/x

x <- log(x)
y <- log(y)

linear_model <- lm(y~x)
summary(linear_model)

###############
# VERIFICA HP
###############

plot(linear_model$fitted.values, linear_model$residuals, 
     main='Residui vs valori stimati', lwd=2,
     xlab='Y stimati', ylab='Residui')
abline(h=0, lwd=2)

qqPlot(linear_model$residuals,distribution = "norm",main='QQP dei residui')
