##########################################
# 2022.08.16
# Suzuki writing
# R 4.1.3 GUI 1.77 High Sierra build (8051)
# each month mojularity
# randamization matrix    
##########################################

# 日本語メモ
# ランダム行列を作成する時はsample-level matrixをランダマイズして、colSums()を行う
# as.matrix()で読み込まない
# 
# 
# 
# 


library(bipartite)
library(parallel)


# ramdamization set up
nrep <- 1000
#nrep <- 5
n.core <- 8

# each month 
month.uni <- c(4:11)

for (u in month.uni ) { 

xx <- read.table(sprintf("../Data/prey.id.matrix.sum.%s.spider.name.txt",u), header=T, sep="\t")
dim(xx)

row.sample <- list(NULL)
for (i in 1:nrep) { row.sample[[i]] <- sample(xx[, 1]) }

null.matrices <- list(NULL)
for (i in 1:nrep) { null.matrices[[i]] <- cbind(row.sample[[i]], xx[, -1]) } 

species.level <- list(NULL)

plant <- unique(xx[, 1])
fungus <- colnames(xx[,-1])

#NULLリストを作成する
for (j in 1:nrep) { 
species.level[[j]] <- matrix(NA, nrow=length(plant), ncol=ncol(xx[, -1]))
for (i in 1:length(plant)) {
species.level[[j]][i, ] <- colSums(subset(null.matrices[[j]], null.matrices[[j]][, 1]==plant[i])[, -1])}


dir.create(sprintf('../Output/Randomized_%d',u),showWarnings = F)

species.level[[j]][which(species.level[[j]] >= 1)] <- 1


otu.0col <- t(subset(t(species.level[[j]]), rowSums(t(species.level[[j]])) >0 ))
otu.0row <-   subset(otu.0col, rowSums(otu.0col)  >0 )
dim(otu.0row)

#otu.0col <- subset(species.level[[j]], colSums(species.level[[j]])) >0 
#otu.0row <-   subset(otu.0col, rowSums(otu.0col)  >0 )
#dim(otu.0row)


m <- nrow(otu.0row)
n <- ncol(otu.0row)
original.sq <- rbind(cbind(matrix(0,m,m), otu.0row),cbind(t(otu.0row), matrix(0,n,n)))
dim(original.sq)

write.table(original.sq, file=sprintf("../Output/Randomized_%d/ramdomization.mat.%s.txt", u,j),col.names=F, row.names=F, sep="\t", quote=F)

}

}

