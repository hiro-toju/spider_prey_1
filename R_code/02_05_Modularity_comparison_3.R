##########################################
# 2021.10.04
# Suzuki edit
# 2021.10.19
# Suzuki edit
# 2023.03.07 Hirokazu Toju
# R 4.0.4 GUI 1.74 Catalina build (7936)
# month network index  
# randamization each network index    
##########################################

library(bipartite)
library(parallel)
library(tictoc)
library(foreach)
library(doParallel)
library(igraph)
library(lpbrim)
library(metacom)
library(BipartiteModularityMaximization)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(cowplot)
library(RColorBrewer)
library(ggrepel)
library(reshape2)


# check core
detectCores(logical = FALSE)
detectCores()
registerDoParallel(cores=4)

n.core <- 6

# each month 
month.uni <- c(4:11)

###########################

mat.ori.sp <- readRDS("../Data/mat.ori.sp.rds")
mat.ori.sp.bi <- readRDS("../Data/mat.ori.sp.bi.rds")
g <- readRDS("../Data/graph.ori.rds")

######################################################
# louvain

Mod.louvain <- unlist(foreach(u=1:8) %dopar% modularity(cluster_louvain(g[[u]])))
mem.louvain <- foreach(u=1:8) %dopar% membership(cluster_louvain(g[[u]]))

s.louvain <- foreach(u=1:8) %dopar%  {
	memvec <- mem.louvain[[u]]
	module <- unique(memvec)
	calc <- foreach(j=1:length(module), .combine=cbind) %dopar% {
	temp <- rep(0, times=length(memvec))
	temp[which(memvec ==module[j])] <- 1
	temp
	}
	calc
}

babMod.louvain <- unlist(foreach(u=1:8) %dopar% Qbip(mat.ori.sp.bi[[u]], s.louvain[[u]]))

######################################################
# fast_greedy

Mod.fast_greedy <- unlist(foreach(u=1:8) %dopar% modularity(cluster_fast_greedy(g[[u]])))
mem.fast_greedy <- foreach(u=1:8) %dopar% membership(cluster_fast_greedy(g[[u]]))

s.fast_greedy <- foreach(u=1:8) %dopar%  {
	memvec <- mem.fast_greedy[[u]]
	module <- unique(memvec)
	calc <- foreach(j=1:length(module), .combine=cbind) %dopar% {
	temp <- rep(0, times=length(memvec))
	temp[which(memvec ==module[j])] <- 1
	temp
	}
	calc
}

babMod.fast_greedy <- unlist(foreach(u=1:8) %dopar% Qbip(mat.ori.sp.bi[[u]], s.fast_greedy[[u]]))

######################################################
# infomap

Mod.infomap <- unlist(foreach(u=1:8) %dopar% modularity(cluster_infomap(g[[u]])))
mem.infomap <- foreach(u=1:8) %dopar% membership(cluster_infomap(g[[u]]))

s.infomap <- foreach(u=1:8) %dopar%  {
	memvec <- mem.infomap[[u]]
	module <- unique(memvec)
	calc <- foreach(j=1:length(module), .combine=cbind) %dopar% {
	temp <- rep(0, times=length(memvec))
	temp[which(memvec ==module[j])] <- 1
	temp
	}
	calc
}

babMod.infomap <- unlist(foreach(u=1:8) %dopar% Qbip(mat.ori.sp.bi[[u]], s.infomap[[u]]))

######################################################

df1 <- data.frame(month=4:11, Mod.louvain, babMod.louvain, Mod.fast_greedy, babMod.fast_greedy, Mod.infomap, babMod.infomap)

######################################################


pdf('../Output/Modularity_comparison_square.pdf', w= 6, h= 3)

g <- ggplot(df1, aes(month)) +
  geom_line(aes(y = Mod.louvain, colour = "Mod.louvain")) +
  geom_line(aes(y = Mod.fast_greedy, colour = "Mod.fast_greedy")) +
  geom_line(aes(y = Mod.infomap, colour = "Mod.infomap"))
  
plot(g)
dev.off()


pdf('../Output/Modularity_comparison_bipartite.pdf', w= 6, h= 3)

g <- ggplot(df1, aes(month)) +
  geom_line(aes(y = babMod.louvain, colour = "babMod.louvain")) +
  geom_line(aes(y = babMod.fast_greedy, colour = "babMod.fast_greedy")) +
  geom_line(aes(y = babMod.infomap, colour = "babMod.infomap"))
  
plot(g)
dev.off()

