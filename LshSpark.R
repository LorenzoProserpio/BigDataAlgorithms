##############
# LIBRERIE
##############

rm(list = ls())
library(sparklyr)
library(tidyverse)
Dir.Work <- "D:/Uni/Magistrale/Statistica Matematica Avanzata/HomeWork_3/4"
setwd(Dir.Work)

##############
# IN SPARK
##############

sc <- spark_connect(master = "local")

###################
# DATASET IMMAGINI
###################

typeCols <- paste0("V",1:400,"= 'double'",collapse = ",")
typeCols <- paste("list(",typeCols,")")

Immagini <-  spark_read_csv(sc, name = "Immagini",
                                 path = "lsh.csv.gz",
                                 columns = eval(parse(text = typeCols)),
                                 header = FALSE, memory = F)

Immagini_Assembled <- Immagini %>% 
  ft_vector_assembler(input_cols = paste0("V",1:400),output_col = "vect") %>%
  select(vect) %>% 
  sdf_with_unique_id %>% 
  mutate(id = int(id)) %>%
  sdf_register(name = "Immagini_Assembled")

tbl_cache(sc, "Immagini_Assembled")



########################
# DATASET IMMAGINI TEST
########################

## COMMENTO: questa parte serve nella seconda metà del programma, nella prima
##           non viene usata perchè ho notato che le immagini test sono le prime
##           20 immagini dell'altro database

Immagini_Test <- read_csv(file = "lsh2.csv.gz",
                        col_names = FALSE)

########################
# LSH AMPLIFICAZIONE OR
########################

## COMMENTO: non uso 240 hash tables perchè non vi è una sostanziale differenza 
##           con 24, considerato che uso solo l'amplificazione OR

k <- 24L
r <- 100L

model_LSH <- ft_bucketed_random_projection_lsh(sc,
                                                input_col = "vect",
                                                output_col = "buckets",
                                                bucket_length = r,
                                                num_hash_tables = k,
                                                seed = 17)

model_LSHfitted <- ml_fit(model_LSH, Immagini_Assembled)

#####################################
# TROVO I SEI CANDIDATI PIU' VICINI
#####################################

d <- matrix(0, nrow = 20, ncol = 1)

for (i in (1:20)){
  
  zi <- Immagini_Test %>%
          slice(i) %>%
          as.matrix()
  
  NNeigh <- ml_approx_nearest_neighbors(model = model_LSHfitted, 
                                        dataset = Immagini_Assembled, 
                                        key = zi %>% as.vector(), 
                                        num_nearest_neighbors = 7,
                                        dist_col = "distCol")
  
  Closest <- NNeigh %>% collect()
  
  ## COMMENTO: Ovviamente tra le 6 più vicine c'è pure se' stessa, la escludo.
  Closest <- Closest[-1,] 
  
  xij <- read_csv(file = "lsh.csv.gz",
                  col_names = FALSE) %>%
    slice(Closest$id+1) %>%
    as.matrix()
  
  distanceL2 <- sqrt(rowSums((xij - matrix(zi, ncol = 400, nrow = 6, byrow = T))^2))
  xiast <- xij[which(distanceL2 == min(distanceL2)),]
  
  par(mfrow=c(1,2))
  
  image(matrix(zi,ncol = 20,byrow = T), axes=FALSE)
  title(paste0("Immagine test ",i))
  
  image(matrix(xiast,ncol = 20,byrow = T), axes=FALSE)
  title(paste0("Immagine trovata"))
  
  d[i] <- sum(distanceL2) / 6
  
}

view(d)

################
# DISCONNECTION
################

spark_disconnect(sc)


################################################################################


###################
# RITORNO SU SPARK
###################

rm(list = ls())
library(sparklyr)
library(tidyverse)
Dir.Work <- "D:/Uni/Magistrale/Statistica Matematica Avanzata/HomeWork_3/4"
setwd(Dir.Work)
sc <- spark_connect(master = "local")

###################
# DATASET IMMAGINI
###################

typeCols <- paste0("V",1:400,"= 'double'",collapse = ",")
typeCols <- paste("list(",typeCols,")")

Immagini <-  spark_read_csv(sc, name = "Immagini",
                            path = "lsh.csv.gz",
                            columns = eval(parse(text = typeCols)),
                            header = FALSE, memory = F)

Immagini_Assembled <- Immagini %>% 
  ft_vector_assembler(input_cols = paste0("V",1:400),output_col = "vect") %>%
  select(vect) %>% 
  sdf_with_unique_id %>% 
  mutate(id = int(id)) %>%
  sdf_register(name = "Immagini_Assembled")

tbl_cache(sc, "Immagini_Assembled")

########################
# DATASET IMMAGINI TEST
########################

Immagini_Test <- read_csv(file = "lsh2.csv.gz",
                          col_names = FALSE)

names(Immagini_Test) <- paste0("V",1:400)

Immagini_Test_S <-  copy_to(dest = sc,
                      df = Immagini_Test,
                      name = "Immagini_Test_S",
                      memory = F)

Immagini_Test_S_Assembled <- Immagini_Test_S  %>% 
  ft_vector_assembler(input_cols = paste0("V",1:400),output_col = "vect") %>%
  select(vect) %>% 
  sdf_with_unique_id %>% 
  mutate(id = int(id)) %>%
  sdf_register(name = "Immagini_Test_S_Assembled")

tbl_cache(sc, "Immagini_Test_S_Assembled")

#########################
# LSH AMPLIFICAZIONE AND
#########################

L <- 10
k <- 24
r <- 100

model_LSH <- ft_bucketed_random_projection_lsh(sc,
                                                input_col = "vect",
                                                output_col = "buckets",
                                                bucket_length = r,
                                                num_hash_tables = (L*k),
                                                seed = 42)

model_LSHfitted <- ml_fit(model_LSH,Immagini_Assembled)

model_LSHfitted2 <- ml_fit(model_LSH,Immagini_Test_S_Assembled)

hash1 <- ml_transform(model_LSHfitted,Immagini_Assembled) %>% 
  select(-vect) %>%
  ft_sql_transformer("SELECT id,POSEXPLODE(buckets) AS (pos, valueInt) FROM __THIS__") %>%
  sdf_separate_column(column = "valueInt", into = "value") %>%
  mutate(nBand = as.integer(pos / k)) %>%
  select(-valueInt,-pos) %>%
  group_by(id,nBand) %>% summarise(bucket = hash(collect_list(value)))

hash2 <- ml_transform(model_LSHfitted2,Immagini_Test_S_Assembled) %>% 
  select(-vect) %>%
  ft_sql_transformer("SELECT id,POSEXPLODE(buckets) AS (pos, valueInt) FROM __THIS__") %>%
  sdf_separate_column(column = "valueInt", into = "value") %>%
  mutate(nBand = as.integer(pos / k)) %>%
  select(-valueInt,-pos) %>%
  group_by(id,nBand) %>% summarise(bucket = hash(collect_list(value)))

############
# CANDIDATI
############

Candidates <- inner_join(x=hash1,
                         y=hash2,
                         by = c("nBand","bucket"),
                         suffix = c("_A", "_B")
)

Candidates <- Candidates %>%
  sdf_register(name = "Candidates") %>% 
  select(-nBand,-bucket)

Candidates_R <- Candidates %>% collect()
Candidates_R <- unique(Candidates_R)
Candidates_R <- Candidates_R %>% filter(id_A != id_B)

#################################################
# TROVO I SEI CANDIDATI PIU' VICINI (SE CI SONO)
#################################################

## COMMENTO: ci sono solo per le immagini con id 1,10,13,14,15,17

d <- matrix(0, ncol = 1, nrow = 20)
temp <- c(1,10,13,14,15,17)

for (i in temp){
  zi <- read_csv(file = "lsh2.csv.gz",
                 col_names = FALSE) %>%
    slice(i+1) %>%
    as.matrix()

  filtrato <- Candidates_R %>% filter(id_B == i)
  vettoreID <- (as.vector(filtrato$id_A)) + 1

  xij <- read_csv(file = "lsh.csv.gz",
                  col_names = FALSE) %>%
    slice(vettoreID) %>%
    as.matrix()

  distanceL2 <- sqrt(rowSums((xij - matrix(zi, ncol = 400, nrow = length(vettoreID), byrow = T))^2))
  d[i+1] <- mean((sort(distanceL2)[1:6]))
  
}

view(d)

################
# DISCONNECTION
################

spark_disconnect(sc)

################################################################################


################
# USING MLPACK
################

rm(list = ls())
library(mlpack)
library(tidyverse)
Dir.Work <- "D:/Uni/Magistrale/Statistica Matematica Avanzata/HomeWork_3/4"
setwd(Dir.Work)

###################
# DATASET IMMAGINI
###################

Immagini <- read.csv("lsh.csv", header = FALSE)

########################
# DATASET IMMAGINI TEST
########################

Immagini_Test <- read.csv("lsh2.csv", header = FALSE)

#######
# LSH
#######

output <- lsh(bucket_size = 100, k = 7, projections = 24,
              query = Immagini_Test, reference = Immagini, tables = 10)
## COMMENTO: shifto gli id e rimuovo l'immagine stessa dai candidati come prima.

Candidates<- output$neighbors + 1
Candidates <- Candidates[,-1]

d <- output$distances
d <- d[,-1]

mean_d <- rowSums(d) / 6
view(mean_d)

