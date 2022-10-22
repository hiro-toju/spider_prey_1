##########################################
# 2021.10.04
# Suzuki edit
# 2021.10.19
# Suzuki edit
# R 4.0.4 GUI 1.74 Catalina build (7936)
# month network index  
# randamization each network index    
##########################################

library(bipartite)
library(parallel)

# check core
detectCores(logical = FALSE)
detectCores()

# ramdamization set up
nrep <- 1000
#nrep <- 2
n.core <- 8

# each month 
month.uni <- c(4:11)

# 4,5,6,.... ->  04,05,06...
#month.uni <- formatC(month.uni,width=2,flag="0")

###########################
#for cycle with each month ###########################

#install.packages("tictoc")
library(tictoc)

tic()

H2.mean.all <- matrix(NA, nrow=1, ncol= length(month.uni))
wNODF.mean.all <- matrix(NA, nrow=1, ncol= length(month.uni))
cscore.HL.mean.all <- matrix(NA, nrow=1, ncol= length(month.uni))
cscore.LL.mean.all <- matrix(NA, nrow=1, ncol= length(month.uni))
colnames(H2.mean.all) <- c(4:11)
colnames(wNODF.mean.all) <- c(4:11)
colnames(cscore.HL.mean.all) <- c(4:11)
colnames(cscore.LL.mean.all) <- c(4:11)


for (u in month.uni ) {

# ネットワークレベル解析は逆なので、他のコードも行列指定を逆にする？途中でt()を挟めば良い？どこで？
xx <- read.table(sprintf('../Data/prey.id.matrix.sum.%s.spider.name.txt',u),header=T, sep ='\t')


row.sample <- list(NULL)

# sample()
# takes a sample of the specified size from the elements of x using either with or without replacement.
# データの中から要素（数字)を（ランダムに)サンプリングする
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
species.level[[j]][i, ] <- colSums(subset(null.matrices[[j]], null.matrices[[j]][, 1]==plant[i])[, -1])
}}
 

m.null <- c(NULL)
n.null <- c(NULL)

for (i in 1:nrep) {
m.null[i] <- ncol(species.level[[i]])
n.null[i] <- nrow(species.level[[i]])
}

# original matrix

original <- matrix(NA, nrow=length(plant), ncol=ncol(xx[, -1]))

for (i in 1:length(plant)) {
original[i, ] <- colSums(subset(xx, xx[, 1]==plant[i])[, -1])
}

plant.label <- as.vector(plant)
rownames(original) <- plant.label
colnames(original) <- colnames(xx[, -1])
dim(original)
#original network index


dir.create('../Output/index_originaldata', showWarnings =F)
write.table(original, file=sprintf("../Output/index_originaldata/original.data.%s.txt",u), quote=F, sep="\t",row.names=T)

# hensuu hennkou xx.otu ⇢　original 
#xx.otu <- xx[, -1]
 
H2 <- networklevel(original,index='H2')
wNODF <- networklevel(original,index='weighted NODF')
cscore <- networklevel(original,index='C score')




# nulls calculation 
# 3つのインデックスやる？設定できる？
# 個別に設定したい
#mc.network <- mclapply(species.level, networklevel, mc.cores=n.core)


# H2 ##################
#species.level.t <- t(species.level)
# t
#mc.H2 <- mclapply(species.level.t,
#function(y) {
#	net <- networklevel(y,index='H2')
#	return(net)}, mc.cores=n.core)
# not t	
mc.H2 <- mclapply(species.level,
function(y) {
	net <- networklevel(y,index='H2')
	return(net)}, mc.cores=n.core)
		
mc.H2.vec <- c(unlist(mc.H2[[1]][1]))
for (i in 2:nrep) { mc.H2.vec <- rbind(mc.H2.vec, c(unlist(mc.H2[[i]][1])))}
H2.mean.all[,sprintf('%s',u)] <- mean(mc.H2.vec)

# H2 ##################

# wNODF ##################
mc.wNODF <- mclapply(species.level,
function(y) {
	net <- networklevel(y,index='weighted NODF')
	return(net)}, mc.cores=n.core)

mc.wNODF.vec <- c(unlist(mc.wNODF[[1]][1]))
for (i in 2:nrep) { mc.wNODF.vec <- rbind(mc.wNODF.vec, c(unlist(mc.wNODF[[i]][1])))}
wNODF.mean.all[,sprintf('%s',u)] <- mean(mc.wNODF.vec)

# wNODF ##################

# cscore ##################
mc.cscore <- mclapply(species.level,
function(y) {
	net <- networklevel(y,index='C score')
	return(net)}, mc.cores=n.core)

mc.cscore.HL.vec <- c(unlist(mc.cscore[[1]][1]))
for (i in 2:nrep) { mc.cscore.HL.vec <- rbind(mc.cscore.HL.vec, c(unlist(mc.cscore[[i]][1])))}
cscore.HL.mean.all[,sprintf('%s',u)] <- mean(mc.cscore.HL.vec)

mc.cscore.LL.vec <- c(unlist(mc.cscore[[1]][2]))
for (i in 2:nrep) { mc.cscore.LL.vec <- rbind(mc.cscore.LL.vec, c(unlist(mc.cscore[[i]][2])))}
cscore.LL.mean.all[,sprintf('%s',u)] <- mean(mc.cscore.LL.vec)

# cscore ##################


dir.create('../Output/index_randomization', showWarnings =F)
# round() 小数点以下4桁で四捨五入する
write.table(round(mc.H2.vec, 4), file=sprintf("../Output/index_randomization/H2.rand1000.%s.txt",u), quote=F, row.name=F, sep="\t")
write.table(round(mc.wNODF.vec, 4), file=sprintf("../Output/index_randomization/wNODF.rand1000.%s.txt",u), quote=F, row.name=F, sep="\t")

write.table(round(mc.cscore.HL.vec, 4), file=sprintf("../Output/index_randomization/cscore.HL.rand1000.%s.txt",u), quote=F, row.name=F, sep="\t")
write.table(round(mc.cscore.LL.vec, 4), file=sprintf("../Output/index_randomization/cscore.LL.rand1000.%s.txt",u), quote=F, row.name=F, sep="\t")

write.table(round(H2.mean.all, 4), file="../Output/index_randomization/H2.randa1000.mean.txt", quote=F, row.name=F, sep="\t")
write.table(round(wNODF.mean.all, 4), file="../Output/index_randomization/wNODF.rand1000.mean.txt", quote=F, row.name=F, sep="\t")

write.table(round(cscore.HL.mean.all, 4), file="../Output/index_randomization/cscore.HL.rand1000.mean.txt", quote=F, row.name=F, sep="\t")
write.table(round(cscore.LL.mean.all, 4), file="../Output/index_randomization/cscore.LL.rand1000.mean.txt", quote=F, row.name=F, sep="\t")

# zscore ##################

H2.z <- c(NULL)
wNODF.z <- c(NULL)
cscore.LL.z <- c(NULL)
cscore.HL.z <- c(NULL)

H2.p.value <- c(NULL)
wNODF.p.value <- c(NULL)
cscore.LL.p.value <- c(NULL)
cscore.HL.p.value <- c(NULL)

################	
	
	H2.z <-
	( H2 -　mean(mc.H2.vec) ) / sd(mc.H2.vec) 
  H2.p.value <- sum(mc.H2.vec >  H2)/length(mc.H2.vec)

################
  ################	
  
  wNODF.z <-
    ( wNODF -　mean(mc.wNODF.vec) ) / sd(mc.wNODF.vec) 
  wNODF.p.value <- sum(mc.wNODF.vec >  wNODF)/length(mc.wNODF.vec)
  
  ################
    ################	
  
  cscore.HL.z <-
    ( cscore[[1]] -　mean(mc.cscore.HL.vec) ) / sd(mc.cscore.HL.vec) 
  cscore.HL.p.value <- sum(mc.cscore.HL.vec >  cscore[[1]])/length(mc.cscore.HL.vec)
  
  ################
  ################	
  
  cscore.LL.z <-
    ( cscore[[2]] -　mean(mc.cscore.LL.vec) ) / sd(mc.cscore.LL.vec) 
  cscore.LL.p.value <- sum(mc.cscore.LL.vec >  cscore[[2]])/length(mc.cscore.LL.vec)
  
  ################	

dir.create('../Output/index_z.score', showWarnings =F)

write.table(round(H2.z, 4), file=sprintf("../Output/index_z.score/H2.zscore.%s.txt",u), quote=F, row.name=F, sep="\t")
write.table(round(wNODF.z, 4), file=sprintf("../Output/index_z.score/wNODF.zscore.%s.txt",u), quote=F, row.name=F, sep="\t")
write.table(round(cscore.LL.z, 4), file=sprintf("../Output/index_z.score/cscore.LL.zscore.%s.txt",u), quote=F, row.name=F, sep="\t")
write.table(round(cscore.HL.z, 4), file=sprintf("../Output/index_z.score/cscore.HL.zscore.%s.txt",u), quote=F, row.name=F, sep="\t")


dir.create('../Output/index_p.value', showWarnings =F)
write.table(round( H2.p.value, 4), file=sprintf("../Output/index_p.value/H2.p.value.%s.txt",u), quote=F, row.name=F, sep="\t")
write.table(round(wNODF.p.value, 4), file=sprintf("../Output/index_p.value/wNODF.p.value.%s.txt",u), quote=F, row.name=F, sep="\t")
write.table(round(cscore.LL.p.value, 4), file=sprintf("../Output/index_p.value/cscore.LL.p.value.%s.txt",u), quote=F, row.name=F, sep="\t")
write.table(round(cscore.HL.p.value, 4), file=sprintf("../Output/index_p.value/cscore.HL.p.value.%s.txt",u), quote=F, row.name=F, sep="\t")

}

toc()
















