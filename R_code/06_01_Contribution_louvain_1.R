##########################################
# 2021.10.04
# Suzuki edit
# 2021.10.19
# Suzuki edit
# 2023.03.07 Hirokazu Toju
##########################################

library(bipartite)
library(parallel)
library(tictoc)
library(foreach)
library(doParallel)
library(igraph)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(cowplot)
library(RColorBrewer)
library(ggrepel)

set.seed(123)

# check core
detectCores(logical = FALSE)
detectCores()
registerDoParallel(cores=4)

nrep <- 1000

###########################

ori <- readRDS("../Data/meta.ori.rds")
ori.sq <- readRDS("../Data/meta.ori.sq.rds")
df2 <- readRDS(sprintf("../Output/Olesen_boot_louvain_%s.rds", nrep))

###########################
# Deletion of each node

#sim.sq <- foreach(i=1:nrow(ori.sq)) %dopar% ori.sq[-i, -i]

sim.sq <- list()

for (i in 1:nrow(ori.sq)) {
	sim.sq[[i]] <- ori.sq[-i, -i]
}

###########################

g.ori <- graph_from_adjacency_matrix(ori.sq , weighted = T, mode = "undirected" )
mdis.unconT.ori <- mean_distance(g.ori, directed=FALSE, unconnected=TRUE, details=TRUE)
#mdis.unconF.ori <- mean_distance(g.ori, directed=FALSE, unconnected=FALSE, details=TRUE)

g.sim <- foreach(i=1:nrow(ori.sq)) %dopar% graph_from_adjacency_matrix(sim.sq[[i]] , weighted = T, mode = "undirected" )

mdis.unconT.sim <- foreach(i=1:nrow(ori.sq), .combine=rbind) %dopar% mean_distance(g.sim[[i]], directed=FALSE, unconnected=TRUE, details=TRUE)
#mdis.unconF.boot <- foreach(i=1:nrow(ori.sq), .combine=rbind) %dopar% mean_distance(g.sim[[i]], directed=FALSE, unconnected=FALSE, details=TRUE)

colnames(mdis.unconT.sim) <- c("MeanDistance_removal", "Unconnected.Pairs")

df3 <- data.frame(df2, mdis.unconT.sim, log10_Unconnected.Pairs=log10(as.numeric(mdis.unconT.sim[,2])))

df3[df3$log10_Unconnected.Pairs==-Inf, c("log10_Unconnected.Pairs")] <- NA

#attach(df3)
#plot(AmongModConnectivity.z, log10(Unconnected.Pairs))

df4 <- as.matrix(df3)

write.table(df4, file=sprintf("../Output/Contribution_louvain_%s.txt", nrep), quote=F, sep='\t', row.names=F)
saveRDS(df2, file=sprintf("../Output/Contribution_louvain_%s.rds", nrep))


###########################
# oroginal vs. bootstrap values z


pdf(sprintf('../Output/Contribution_louvain_%s.pdf', nrep), w= 10, h= 10)
g <- ggplot(df3) +
 aes(x = AmongModConnectivity.z, y = log10_Unconnected.Pairs, shape = Category, fill= Category ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(size=(Count)^0.5), alpha=2) +
 labs(x = "Among-module connectivity (bootstrap standardization)", y = "log10(Unconnected.Pairs)", 
 title = sprintf("")) +  geom_vline(xintercept=0, lty=2) +  geom_vline(xintercept=2.5, lty=3, col="red") +
    geom_label_repel(aes(label =label))
plot(g)
dev.off()

pdf(sprintf('../Output/Contribution_louvain_small_%s.pdf', nrep), w= 6, h= 5)
g <- ggplot(df3) +
 aes(x = AmongModConnectivity.z, y = log10_Unconnected.Pairs, shape = Category, fill= Category ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(size=(Count)^0.5), alpha=2) +
 labs(x = "Among-module connectivity (bootstrap standardization)", y = "log10(Unconnected.Pairs)", 
 title = sprintf("")) +  geom_vline(xintercept=0, lty=2) +  geom_vline(xintercept=2.5, lty=3, col="red") +
    geom_label_repel(aes(label =label))
plot(g)
dev.off()



