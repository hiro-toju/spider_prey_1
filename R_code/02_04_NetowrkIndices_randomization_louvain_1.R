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

# check core
detectCores(logical = FALSE)
detectCores()
registerDoParallel(cores=4)

# ramdamization set up
nrep <- 1000
n.core <- 6

# each month 
month.uni <- c(4:11)


###########################

mat.ori.sp <- readRDS("../Data/mat.ori.sp.rds")
mat.ori.sp.bi <- readRDS("../Data/mat.ori.sp.bi.rds")
g <- readRDS("../Data/graph.ori.rds")

mat.rand.sp <- readRDS(sprintf("../Data/mat.rand.sp_%s.rds", nrep))
mat.rand.sp.bi <- readRDS(sprintf("../Data/mat.rand.sp.bi_%s.rds", nrep))
g.rand <- readRDS(sprintf("../Data/graph.rand_%s.rds", nrep))

######################################################
# Calculation of network indices

H2 <- foreach(u=1:8) %dopar% networklevel(mat.ori.sp[[u]], index='H2')
wNODF <- foreach(u=1:8) %dopar% networklevel(mat.ori.sp[[u]], index='weighted NODF')
Mod <- foreach(u=1:8) %dopar% modularity(cluster_louvain(g[[u]]))
mem <- foreach(u=1:8) %dopar% membership(cluster_louvain(g[[u]]))


s <- foreach(u=1:8) %dopar%  {
	memvec <- mem[[u]]
	module <- unique(memvec)
	calc <- foreach(j=1:length(module), .combine=cbind) %dopar% {
	temp <- rep(0, times=length(memvec))
	temp[which(memvec ==module[j])] <- 1
	temp
	}
	calc
}

babMod <- foreach(u=1:8) %dopar% Qbip(mat.ori.sp.bi[[u]], s[[u]])


###########################

rand.H2 <- foreach(u=1:8) %dopar% {
	calc <- foreach(i=1:nrep, .combine=rbind) %dopar% {
		networklevel(mat.rand.sp[[u]][[i]], index='H2')
		}
		calc
		}
		
rand.wNODF <- foreach(u=1:8) %dopar% {
	calc <- foreach(i=1:nrep, .combine=rbind) %dopar% {
		networklevel(mat.rand.sp[[u]][[i]], index='weighted NODF')
		}
		calc
		}
		
rand.Mod <- foreach(u=1:8) %dopar% {
	calc <- foreach(i=1:nrep, .combine=rbind) %dopar% {
		modularity(cluster_louvain(g.rand[[u]][[i]]))
		}
		calc
		}
		
rand.mem <- foreach(u=1:8) %dopar% {
	calc <- foreach(i=1:nrep, .combine=rbind) %dopar% {
		membership(cluster_louvain(g.rand[[u]][[i]]))
		}
		calc
		}	
		
rand.s <- foreach(u=1:8) %dopar% {
	calc <- foreach(i=1:nrep) %dopar% {
		memvec <- rand.mem[[u]][i,]
		module <- unique(memvec)
		calc2 <- foreach(j=1:length(module), .combine=cbind) %dopar% {
		temp <- rep(0, times=length(memvec))
		temp[which(memvec ==module[j])] <- 1
		temp
		}
		as.matrix(calc2)
		}
		calc
		}	

rand.babMod <- foreach(u=1:8) %dopar% {
	calc <- foreach(i=1:nrep, .combine=rbind) %dopar% {
		Qbip(mat.rand.sp.bi[[u]][[i]], rand.s[[u]][[i]])
		}
		calc
		}		
	
		
saveRDS(rand.H2, file=sprintf("../Data/rand.H2_%s.rds", nrep))
saveRDS(rand.wNODF, file=sprintf("../Data/rand.wNODF_%s.rds", nrep))
saveRDS(rand.Mod, file=sprintf("../Data/rand.Modularitylouvain_%s.rds", nrep))
saveRDS(rand.babMod, file=sprintf("../Data/rand.ModularityBarberlouvain_%s.rds", nrep))


# zscore ##################

z.H2 <- foreach(u=1:8) %dopar% {(H2[[u]] - mean(unlist(rand.H2[[u]]))) /  sd(unlist(rand.H2[[u]]))}
z.wNODF <- foreach(u=1:8) %dopar% {(wNODF[[u]] - mean(unlist(rand.wNODF[[u]]))) /  sd(unlist(rand.wNODF[[u]]))}
z.Mod <- foreach(u=1:8) %dopar% {(Mod[[u]] - mean(unlist(rand.Mod[[u]]))) /  sd(unlist(rand.Mod[[u]]))}

z.babMod <- foreach(u=1:8) %dopar% {(babMod[[u]] - mean(unlist(rand.babMod[[u]]))) /  sd(unlist(rand.babMod[[u]]))}

###########################
# P value 

P.H2 <- unlist(foreach(u=1:8) %dopar% { sum( unlist(rand.H2[[u]]) > H2[[u]] ) /  length(unlist(rand.H2[[u]]))})
for (u in 1:8) {
	if (P.H2[u] > 0.5) {P.H2[u] <- 1 - P.H2[u]}
}

P.wNODF <- unlist(foreach(u=1:8) %dopar% { sum( unlist(rand.wNODF[[u]]) > wNODF[[u]] ) /  length(unlist(rand.wNODF[[u]]))})
for (u in 1:8) {
	if (P.wNODF[u] > 0.5) {P.wNODF[u] <- 1 - P.wNODF[u]}
}

P.Mod <- unlist(foreach(u=1:8) %dopar% { sum( unlist(rand.Mod[[u]]) > Mod[[u]] ) /  length(unlist(rand.Mod[[u]]))})
for (u in 1:8) {
	if (P.Mod[u] > 0.5) {P.Mod[u] <- 1 - P.Mod[u]}
}

P.babMod <- unlist(foreach(u=1:8) %dopar% { sum( unlist(rand.babMod[[u]]) > babMod[[u]] ) /  length(unlist(rand.babMod[[u]]))})
for (u in 1:8) {
	if (P.babMod[u] > 0.5) {P.babMod[u] <- 1 - P.babMod[u]}
}

# FDR ##################

FDR.H2 <- p.adjust(P.H2, method="fdr")
FDR.wNODF <- p.adjust(P.wNODF, method="fdr")
FDR.Mod <- p.adjust(P.Mod, method="fdr")
FDR.babMod <- p.adjust(P.babMod, method="fdr")

################	

r1 <- cbind(Month=month.uni, H2=unlist(H2), z.H2=unlist(z.H2), P.H2, FDR.H2, wNODF=unlist(wNODF), z.wNODF=unlist(z.wNODF), P.wNODF, FDR.wNODF, Modlouvain=unlist(Mod), z.Modlouvain =unlist(z.Mod), P.Mod, FDR.Mod, ModBablouvain=unlist(babMod), z.ModBablouvain=unlist(z.babMod), P.babMod, FDR.babMod)

write.table(r1, file=sprintf('../Output/NetworkIndices_louvain_%s.txt', nrep), sep='\t', quote=F, row.name=F)

