###########
# LIBRERIE
###########

rm(list = ls())
library(igraph)
library(Matrix)
library(tidyverse) 

Dir.Work <- "C:/Users/Lorenzo/Desktop/HW4" # Your path
setwd(Dir.Work)

################
# CARICO I DATI
################

load("UserShows.RDATA")

#################################
# CREO LE MATRICI R, P*, Q*
#################################

R = Matrix(UserShows, sparse = TRUE) 

S_ROW <- rowSums(R)
S_COL <- colSums(R)

Past <- Diagonal(x = S_ROW ^ (-0.5))
Qast <- Diagonal(x = S_COL ^ (-0.5))

####################################################
# RIMUOVO CIO' CHE NON MI SERVE PER RISPARMIARE RAM
####################################################

rm(S_ROW)
rm(S_COL)

################################################################################
# COLLABORATIVE FILTERING
################################################################################

###################
# CALCOLO GAMMA_O
###################

L <- crossprod(R,R)
S_O <- Qast %*% L %*% Qast
G_O <- R %*% S_O

####################################################
# RIMUOVO CIO' CHE NON MI SERVE PER RISPARMIARE RAM
####################################################

rm(L)
rm(S_O)
gc()

###################
# CALCOLO GAMMA_U
###################

T <- tcrossprod(R,R)
S_U <- Past %*% T %*% Past
G_U <- S_U %*% R

####################################################
# RIMUOVO CIO' CHE NON MI SERVE PER RISPARMIARE RAM
####################################################

rm(T)
rm(S_U)
gc()

#############################################################
# ESTRAGGO LE RACCOMANDAZIONI SUI PRIMI 100 SHOWS PER CLAUDIA
#############################################################

SClaudia_O <- G_O[500,1:100]
SClaudia_U <- G_U[500,1:100]

##############################################
# ORDINO E STAMPO I PRIMI 5 SHOWS RACCOMANDATI
##############################################

SClaudia_O <- sort(SClaudia_O, 
                     decreasing = TRUE, 
                     index.return = T)$ix 
SClaudia_O_5 <- SClaudia_O[1:5]

SClaudia_U <- sort(SClaudia_U, 
                     decreasing = TRUE, 
                     index.return = T)$ix 
SClaudia_U_5 <- SClaudia_U[1:5]

cat(paste(as.vector(Shows$name[SClaudia_O_5]),collapse = "\n"))
cat(paste(as.vector(Shows$name[SClaudia_U_5]),collapse = "\n"))

################################################################################
# PAGE RANK
################################################################################

#########################################
# CREO LA MATRICE DI ADIACENZA DEL GRAFO
#########################################

UtUt <- matrix(0, ncol = 9985, nrow = 9985)
ShSh <- matrix(0, ncol = 563, nrow = 563)
Adj_mat <- cbind(UtUt,R)
aux <- cbind(t(R),ShSh)
Adj_mat <- rbind(Adj_mat,aux)

####################################################
# RIMUOVO CIO' CHE NON MI SERVE PER RISPARMIARE RAM
####################################################

rm(aux)
rm(UtUt)
rm(ShSh)
gc()

################
# CREO IL GRAFO
################

graph <- graph_from_adjacency_matrix(Adj_mat, mode = "undirected")

######################################################
# IMPOSTO LA RIPARTENZA DA CLAUDIA ED ESEGUO PAGERANK
######################################################

Ripartenza <- rep_len(0,10548)
Ripartenza[500] <- 1
PageRank <- page_rank(graph = graph, personalized = Ripartenza)

#############################################################
# ESTRAGGO LE RACCOMANDAZIONI SUI PRIMI 100 SHOWS PER CLAUDIA
#############################################################

# COMMENTO: Siccome abbiamo creato la matrice di adiacenza tali indici vanno shiftati
vettoreRank100 <- PageRank$vector[9986:10085]

##############################################
# ORDINO E STAMPO I PRIMI 5 SHOWS RACCOMANDATI
##############################################

SClaudia_PRank <- sort(vettoreRank100, 
                              decreasing = TRUE, 
                              index.return = T)$ix 

cat(paste(as.vector(Shows$name[SClaudia_PRank[1:5]]),collapse = "\n"))

################################################################################
# CALCOLO DELLE PERFORMANCE
################################################################################
 
Performance <- bind_rows(
  tibble(y = cumsum(Claudia[SClaudia_U])/ seq_len(100)) %>% 
    mutate(x = row_number(), z = "user-user") ,
  tibble(y = cumsum(Claudia[SClaudia_O])/ seq_len(100)) %>% 
    mutate(x = row_number(), z = "item-item") ,
  tibble(y = cumsum(Claudia[SClaudia_PRank])/ seq_len(100)) %>%
    mutate(x = row_number(), z = "page-rank") )

ggplot(data = Performance %>% filter(x<51)) +
  geom_line(aes(y = y, x=x, color = z), size = 1.5) + labs(title = "Confronto di performances",
                                               x = "numero suggerimenti",
                                               y = "frazione di suggerimenti corretti") + 
  theme_classic()
