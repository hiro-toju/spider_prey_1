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
n.core <- 3

# each month 
month.uni <- c(4:11)

###########################

data <- list()
xx <- list()

for (i in 1:8) {
	u <- i +3
	data[[i]] <- read.table(sprintf('../Data/prey.id.matrix.sum.%s.spider.name.txt',u), header=T, sep ='\t')
	mat <- data[[i]][,-1]
	mat2 <- mat[, colSums(mat) > 0]
	xx[[i]] <- cbind(spider=data[[i]][,1], mat2)
}

###########################
# original matrices: bipartite

mat.ori.sp <- list()
mat.ori.sp.bi <- list()

for (u in 1:8 ) {
	spname <- unique(xx[[u]][, 1])
	matsp <- foreach(j=1:length(spname), .combine=rbind) %dopar% {
			colSums(subset(xx[[u]][,-1], xx[[u]][, 1]== spname[j]))
	}
	rownames(matsp) <- spname
	mat.ori.sp[[u]] <- matsp
	bi <- matsp
	bi[which(bi > 0)] <- 1
	mat.ori.sp.bi[[u]] <- bi
}

###########################

###########################
# randomized matrices: bipartite

tic()

row.rand <- list()
mat.rand <- list()
mat.rand.sp <- list()
mat.rand.sp.bi <- list()

for (u in 1:8 ) {
	row.rand[[u]] <- foreach(i=1:nrep) %dopar% sample(xx[[u]][,1], replace=FALSE)
	mat.rand[[u]] <- foreach(i=1:nrep) %dopar% cbind(species=row.rand[[u]][[i]], xx[[u]][,-1])
	spname <- unique(xx[[u]][, 1])
	mat.rand.sp[[u]] <- foreach(i=1:nrep) %dopar% {
		matsp <- foreach(j=1:length(spname), .combine=rbind) %dopar% {
			colSums(subset(mat.rand[[u]][[i]][,-1], mat.rand[[u]][[i]][, 1]== spname[j]))
	}
	rownames(matsp) <- spname
	matsp
	}
}

for (u in 1:8 ) {
	mat.rand.sp.bi[[u]] <- foreach(i=1:nrep) %dopar% {
		bi <- mat.rand.sp[[u]][[i]]
		bi[which(bi > 0)] <- 1
		bi
		}
}

toc()
###########################

#Square Matrix: original

g <- list()

for (u in 1:8 ) {
m <- nrow(mat.ori.sp[[u]])
n <- ncol(mat.ori.sp[[u]])
original.sq <- rbind( cbind(matrix(0,m,m), mat.ori.sp[[u]]) , cbind(t(mat.ori.sp[[u]]), matrix(0,n,n) )　)
colnames(original.sq) <- c(rownames(mat.ori.sp[[u]]), colnames(mat.ori.sp[[u]]))
g[[u]] <-  graph_from_adjacency_matrix(original.sq , weighted = T, mode = "undirected" )
}

################################		
#Square Matrix: ranodomized

g.rand <- list()

for (u in 1:8 ) {
	g.rand[[u]] <-  foreach(i=1:nrep) %dopar% {
	m <- nrow(mat.rand.sp[[u]][[i]])
	n <- ncol(mat.rand.sp[[u]][[i]])
	rand.sq <- rbind( cbind(matrix(0,m,m), mat.rand.sp[[u]][[i]]) , cbind(t(mat.rand.sp[[u]][[i]]), matrix(0,n,n) )　)
	colnames(rand.sq) <- c(rownames(mat.rand.sp[[u]][[i]]), colnames(mat.rand.sp[[u]][[i]]))
	graph_from_adjacency_matrix(rand.sq , weighted = T, mode = "undirected" )
}}

###########################

saveRDS(mat.ori.sp, file="../Data/mat.ori.sp.rds")
saveRDS(mat.ori.sp.bi, file="../Data/mat.ori.sp.bi.rds")
saveRDS(g, file="../Data/graph.ori.rds")


saveRDS(mat.rand.sp, file=sprintf("../Data/mat.rand.sp_%s.rds", nrep))
saveRDS(mat.rand.sp.bi, file=sprintf("../Data/mat.rand.sp.bi_%s.rds", nrep))
saveRDS(g.rand, file=sprintf("../Data/graph.rand_%s.rds", nrep))


