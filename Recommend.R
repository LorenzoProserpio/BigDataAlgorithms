##############
# LIBRERIE
##############

rm(list = ls())
library(sparklyr)
library(tidyverse)
Dir.Work <- "D:/Uni/Magistrale/Statistica Matematica Avanzata/HomeWork_3/5"
setwd(Dir.Work)

##############
# IN SPARK
##############

sc <- spark_connect(master = "local")

###################
# DATASET NETWORK  
###################

fileData <- "/dataHW1"

NETWORK <-  spark_read_parquet(sc = sc, 
                               memory = T, # in memory
                               overwrite = T,
                               name = "NETWORK",
                               path = paste0(Dir.Work,fileData) 
)

##########
# STEP 1. 
##########

You_May_Know <- inner_join(x = NETWORK,
                                 y = NETWORK,
                                 by = c("V2"),
                                 suffix = c("", "_B")) %>%
                select(-frAll_B,-V2) %>% 
                sdf_register(name = "You_May_Know")



##########
# STEP 2. 
##########

You_May_Know <- You_May_Know %>%
                filter(!array_contains(frAll,V1_B)) %>% 
                filter(V1 != V1_B)

##########
# STEP 3. 
##########

You_May_Know <- You_May_Know  %>% 
                group_by(V1,V1_B) %>% 
                summarise(n = n()) %>%
                transmute(V1 = V1, V3 = V1_B, nFriends = n)

##########
# STEP 4. 
##########

UtRec <- c(2, 924, 8941, 31506)

Best_Recommended <-  You_May_Know %>% group_by(V1) %>% 
                arrange(-nFriends,V3) %>% 
                mutate(pos = row_number()) %>% 
                filter(pos <= 10) %>%
                select(-pos) %>% 
                ungroup()

You_May_Know_Query <- Best_Recommended %>% 
                      filter(V1 %in% UtRec)  %>%
                      collect() %>%
                      arrange(V1,-nFriends,V3)

############
# SOLUTION
############

You_May_Know_SOL <- You_May_Know_Query %>% group_by(V1) %>% 
                      summarise(Suggested = paste(V3, collapse = ","))

view(You_May_Know_SOL)

################
# DISCONNECTION
################

spark_disconnect(sc)

