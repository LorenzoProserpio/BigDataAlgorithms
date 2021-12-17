###########
# LIBRERIE
###########

rm(list = ls())
library(igraph)
library(Matrix)
library(tidyverse)
library(sparklyr)

Dir.Work <- "./"
setwd(Dir.Work)

###########################
# DENSE SUBGRAPH FUNCTIONS
###########################

densGr <- function(Gr){
  nRows <- gorder(Gr)
  if(nRows > 0){
    return(gsize(Gr) / nRows)
  } else {
    return(0)
  }
}

findDenseSub <- function(vertices, edges,epsilon = 0.1){
  countPass <- 0L
  GraphS <- GraphST <- igraph::simplify(graph_from_data_frame(d = edges,
                                                              directed=FALSE,
                                                              vertices = vertices),
                                        remove.multiple = TRUE, remove.loops = TRUE)

  STnrow = gorder(GraphST)
 
  while(STnrow > 0){
    cat(paste0("Round n. ",  countPass <- countPass + 1L, ", num. vertices = ",STnrow,"\n"))

    densS2 <- densGr(GraphST)
    verticesSelection <- degree(GraphST) > 2*(1+epsilon)*densS2 
    GraphST <- induced_subgraph(GraphST, which(verticesSelection))
    
    STnrow = gorder(GraphST)
  
    if(densGr(GraphST)>densGr(GraphS)){
      GraphS <- GraphST
    }
  }
  cat(paste0("Ended!\n"))
  return(GraphS)
}

###############################
# SPARK CONNECTION AND DATASET
###############################

sc <- spark_connect(master = "local")

list_files_all <- dir(path = paste0(Dir.Work,"/dataHW1"),  
                      pattern = ".parquet",
                      full.names = TRUE)

DataSet <-  spark_read_parquet(sc = sc, 
                               memory = T,
                               overwrite = T,
                               name = "friends",
                               path = list_files_all 
)


##################################
# VERTICES AND EDGES OF THE GRAPH
##################################

vertices_tbl <- DataSet %>%
  transmute(id = V1) %>% 
  sdf_drop_duplicates()

edges_tbl <- DataSet %>% 
  transmute(src = V1, dst = V2) %>% 
  sdf_drop_duplicates()

edgR <- edges_tbl %>% collect() 
vertR <- vertices_tbl %>% collect() 

######################
# SPARK DISCONNECTION
######################

spark_disconnect(sc = sc)

################################################################################
# DENSE SUBGRAPH ALGORITHM
################################################################################

Subgraph <- findDenseSub(vertices = vertR, edges = edgR)

#########################
# DENSE SUBGRAPH DENSITY
#########################

rho_Subgraph <- densGr(Subgraph)
rho_Subgraph

################################################################################
# SWEEP ALGORITHM
################################################################################

########
# GRAPH
########

grafo <- igraph::simplify(graph_from_data_frame(d = edgR,
                                            directed=FALSE,
                                            vertices = vertR),
                      remove.multiple = TRUE, remove.loops = TRUE)

#####################################
# CONNECTED COMPONENT OF NODE ID = 1
#####################################

c_connesse <- components(grafo)

n_c_id1 <- c_connesse$membership[which(names(c_connesse$membership) == "1")] %>% 
  as.numeric() 

vert_c_id1 <- as.numeric(names(which(c_connesse[["membership"]] == n_c_id1)))
edg_c_id1 <- edgR[which(edgR$dst %in% vert_c_id1),]

######################################################################
# TELEPORT VECTOR AND GRAPH OF THE CONNECTED COMPONENT OF NODE ID = 1
######################################################################

S = rep(x = 0,length.out = length(vert_c_id1))
S[which(vert_c_id1 == 1)] <- 1

componente_id1 <- igraph::simplify(graph_from_data_frame(d = edg_c_id1,
                                            directed=FALSE,
                                            vertices = vert_c_id1),
                      remove.multiple = TRUE, remove.loops = TRUE)

###########
# PAGERANK 
###########

PageRank <- page_rank(graph = componente_id1,
                      damping = 0.85,
                      personalized = S)

##############################
# SORT IN DECREASING PR SCORE
##############################

orderPageRank <- sort(x = PageRank$vector,
                      decreasing = TRUE, 
                      index.return = TRUE)$ix
  
graphPermuted <- permute(componente_id1, invPerm(orderPageRank))

######################################
# COMPUTE CONDUCTANCE FROM ADJ MATRIX
######################################

A1 <- as_adjacency_matrix(graphPermuted, 
                          type = c("both"))
D <- rowSums(A1)
VolAi <- cumsum(D)
CutAi <- cumsum(D) - 2*cumsum(rowSums(tril(A1)))

#####################
# CONDUCTANCE GRAPH
#####################

ggplot(data = tibble(x = VolAi,y = CutAi) %>%
         mutate(n = row_number()),aes(x=n,y = y/x)) +
  geom_line() +
  labs(x = "Node rank i in decreasing PPR score",
       y = "Conductance") +
  theme_classic()


