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

# ramdamization set up
nrep <- 1000
n.core <- 6

# each month 
month.uni <- c(4:11)

###########################
# one-tailed

Pcalc1 <- function(ori, rand) {
	P <- sum(rand[is.na(rand)==F] > ori ) /  length(rand[is.na(rand)==F])
	return(P)
}

# two-tailed

Pcalc2 <- function(ori, rand) {
	P <- sum(rand[is.na(rand)==F] > ori ) /  length(rand[is.na(rand)==F])
	if (P > 0.5) {P <- 1 - P}
	return(P)
}

###########################

meta.ori <- as.matrix( read.table('../Data/hexa_id.prey.matrix.agg.all.sort.txt', header=T,sep ='\t',row.names=1))

spider <- data.frame(label=rownames(meta.ori))
prey <- data.frame(label=colnames(meta.ori))

month.ori <- readRDS("../Data/mat.ori.sp.rds")
month.boot <- readRDS(sprintf("../Data/mat.boot.sp_%s.rds", nrep))

###########################
# metaweb original

m <- foreach(u=1:8) %dopar% {
	d <- data.frame(label=rownames(month.ori[[u]]), month.ori[[u]])
	tmp <- left_join(spider, d, by="label")
	rownames(tmp) <- tmp$label
	df1 <- t(tmp[,-1])
	d2 <- data.frame(label=rownames(df1), df1)
	tmp2 <- left_join(prey, d2, by="label")
	tmp2[is.na(tmp2)] <- 0
	rownames(tmp2) <- tmp2$label
	t(tmp2[,-1])
}

meta.ori <- matrix(0, nrow=nrow(spider), ncol=nrow(prey))

for (u in 1:8) {
	meta.ori <- meta.ori + m[[u]]
}

saveRDS(meta.ori, file="../Data/meta.ori.rds")

###########################
# metaweb bootstrap

meta.boot <- foreach(i=1:nrep) %dopar% {
m <- foreach(u=1:8) %dopar% {
	d <- data.frame(label=rownames(month.boot[[u]][[i]]), month.boot[[u]][[i]])
	tmp <- left_join(spider, d, by="label")
	rownames(tmp) <- tmp$label
	df1 <- t(tmp[,-1])
	d2 <- data.frame(label=rownames(df1), df1)
	tmp2 <- left_join(prey, d2, by="label")
	tmp2[is.na(tmp2)] <- 0
	rownames(tmp2) <- tmp2$label
	t(tmp2[,-1])
}

meta <- matrix(0, nrow=nrow(spider), ncol=nrow(prey))

for (u in 1:8) {
	meta <- meta + m[[u]]
}
return(meta)
}

saveRDS(meta.boot, file=sprintf("../Data/meta.boot_%s.rds", nrep))


###########################
# louvain original

m <- nrow(meta.ori)
n <- ncol(meta.ori)
original.sq <- rbind( cbind(matrix(0,m,m), meta.ori) , cbind(t(meta.ori), matrix(0,n,n) )　)
colnames(original.sq) <- c(rownames(meta.ori), colnames(meta.ori))
g <-  graph_from_adjacency_matrix(original.sq , weighted = T, mode = "undirected" )

mem.ori <- membership(cluster_louvain(g))
mod.ori <- data.frame(cbind(V(g), module=mem.ori))

saveRDS(original.sq, file="../Data/meta.ori.sq.rds")
saveRDS(mem.ori, file="../Data/meta. mem.ori.rds")
saveRDS(mod.ori, file="../Data/meta.mod.ori.rds")

###########################
# louvain bootstrap

boot.sq <-  foreach(i=1:nrep) %dopar% {
	m <- nrow(meta.boot[[i]])
	n <- ncol(meta.boot[[i]])
	boot.sq <- rbind( cbind(matrix(0,m,m), meta.boot[[i]]) , cbind(t(meta.boot[[i]]), matrix(0,n,n) )　)
	colnames(boot.sq) <- c(rownames(meta.boot[[i]]), colnames(meta.boot[[i]]))
	boot.sq
}

g.boot <-  foreach(i=1:nrep) %dopar% {
	graph_from_adjacency_matrix(boot.sq[[i]] , weighted = T, mode = "undirected" )
}

mod.boot <- foreach(i=1:nrep) %dopar% {
	module <- membership(cluster_louvain(g.boot[[i]]))
	data.frame(cbind(V(g.boot[[i]]), module))
	}
	
saveRDS(boot.sq, file=sprintf("../Data/meta.boot.sq_%s.rds", nrep))
saveRDS(mod.boot, file=sprintf("../Data/meta.boot.mod.boot_%s.rds", nrep))


###########################
# within-module degree: original

wmd <- function(sqmat, modvec) {
	res <- foreach(j=1:length(unique(modvec)), .combine=rbind) %dopar% {
	bi <- sqmat
	bi[which(bi > 0)] <- 1
	m <- bi[modvec == j, modvec == j]
	if (table(modvec)[j] == 1) {deg <- sum(m)} else {deg <- rowSums(m)}
	if (table(modvec)[j] == 1) {WithinModDegree <- NA} else {WithinModDegree <- scale(deg)}	
	data.frame(label=rownames(sqmat)[modvec == j], WithinModDegree)
	}
	return(res[rownames(sqmat), ])
}

wd.ori <- wmd(original.sq, mod.ori$module)

###########################
# within-module degree: bootstrap

wd.boot <- foreach(i=1:nrep, .combine=cbind) %dopar% {
	wmd(boot.sq[[i]], mod.boot[[i]]$module)
	}

wd.boot <- wd.boot[, -grep("label", colnames(wd.boot))]

###########################
# within-module degree: comparison with rondomization

wd.mean <- apply(wd.boot, 1, mean)
wd.sd <- apply(wd.boot, 1, sd)

wd <- foreach(i=1:nrow(wd.ori), .combine=rbind) %dopar% {
	res <- data.frame((wd.ori[i,2] - wd.mean[i]) / wd.sd[i])
	colnames(res) <- c("WithinModDegree.z")
	res
}

ori.sq.bi <- original.sq
ori.sq.bi[which(ori.sq.bi > 0)] <- 1

wd <- foreach(i=1:nrow(wd.ori), .combine=rbind) %dopar% {
	res <- data.frame((wd.ori[i,2] - wd.mean[i]) / wd.sd[i])
	colnames(res) <- c("WithinModDegree.z")
	res
}


P.wd <- foreach(i=1:nrow(wd.ori), .combine="c") %dopar% Pcalc1(wd.ori[i,2], wd.boot[i,])
FDR.wd <- p.adjust(P.wd, method="fdr")

df1 <- data.frame(wd.ori, mod.ori, wd, Category="spider", Degree=rowSums(ori.sq.bi), P.wd, FDR.wd)

df1[grep("H_", df1$label), "Category"] <- "prey"

###########################
# among-module connectivity

amc <- function(sqmat, modvec) {
	bi <- sqmat
	bi[which(bi > 0)] <- 1
	ki <- rowSums(bi)
	kit <- foreach(t=1:length(unique(modvec)), .combine=cbind) %dopar% {
	if (table(modvec)[t] == 1) {sum(bi[, modvec ==t])} else {rowSums(bi[, modvec ==t])}
	}

	kikit2 <- foreach(i=1:nrow(kit), .combine="rbind") %dopar%  {
	res <- (kit[i,]/ki[i])^2
	}
	return(1 - rowSums(kikit2))
}

AmongModConnectivity <- amc(original.sq, mod.ori$module)

AMC.boot <- foreach(i=1:nrep, .combine=cbind) %dopar% {
	amc(boot.sq[[i]], mod.boot[[i]]$module)
	}

amc.mean <- apply(AMC.boot, 1, mean)
amc.sd <- apply(AMC.boot, 1, sd)

amc.z <- foreach(i=1:length(AmongModConnectivity), .combine=rbind) %dopar% {
	res <- data.frame((AmongModConnectivity[i] - amc.mean[i]) / amc.sd[i])
	colnames(res) <- c("AmongModConnectivity.z")
	res
}

P.amc <- foreach(i=1:length(AmongModConnectivity), .combine="c") %dopar% Pcalc1(AmongModConnectivity[i], AMC.boot[i,])
FDR.amc <- p.adjust(P.amc, method="fdr")


df2 <- data.frame(df1, Count=rowSums(original.sq), AmongModConnectivity, amc.z, P.amc, FDR.amc)

write.table(df2, file=sprintf("../Output/Olesen_boot_louvain_%s.txt", nrep), quote=F, sep='\t', row.names=F)
saveRDS(df2, file=sprintf("../Output/Olesen_boot_louvain_%s.rds", nrep))

###########################
# Olesen figure: original values

pdf(sprintf('../Output/Olesen_louvain_%s.pdf', nrep), w= 15, h= 15)
g <- ggplot(df2) +
 aes(x = AmongModConnectivity, y = WithinModDegree, shape = Category, fill= Category ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(size=(Count)^0.5), alpha=2) +
 labs(x = "Among-module connectivity", y = "Within-module degree (z-standardized)", 
 title = sprintf("")) + geom_hline(yintercept=2.5, lty=3) + geom_vline(xintercept=0.62, lty=3) +
    geom_label_repel(aes(label =label))
plot(g)
dev.off()

pdf(sprintf('../Output/Olesen_louvain_%s_small.pdf', nrep), w= 6, h= 5)
g <- ggplot(df2) +
 aes(x = AmongModConnectivity, y = WithinModDegree, shape = Category, fill= Category ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(size=(Count)^0.5), alpha=2) +
 labs(x = "Among-module connectivity", y = "Within-module degree (z-standardized)", 
 title = sprintf("")) + geom_hline(yintercept=2.5, lty=3) + geom_vline(xintercept=0.62, lty=3) +
    geom_label_repel(aes(label =label))
plot(g)
dev.off()

###########################
# Olesen figure: comparison with bootstrap values z

pdf(sprintf('../Output/Olesen_boot_louvain_%s.pdf', nrep), w= 15, h= 15)
g <- ggplot(df2) +
 aes(x = AmongModConnectivity.z, y = WithinModDegree.z, shape = Category, fill= Category ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(size=(Count)^0.5), alpha=2) +
 labs(x = "Among-module connectivity (bootstrap standardization)", y = "Within-module degree (bootstrap standardization)", 
 title = sprintf("")) + geom_hline(yintercept=0, lty=3) + geom_vline(xintercept=0, lty=3) +    geom_label_repel(aes(label =label))
plot(g)
dev.off()

pdf(sprintf('../Output/Olesen_boot_louvain_%s_small.pdf', nrep), w= 6, h= 5)
g <- ggplot(df2) +
 aes(x = AmongModConnectivity.z, y = WithinModDegree.z, shape = Category, fill= Category ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(size=(Count)^0.5), alpha=2) +
 labs(x = "Among-module connectivity (bootstrap standardization)", y = "Within-module degree (bootstrap standardization)", 
 title = sprintf("")) + geom_hline(yintercept=0, lty=3) + geom_vline(xintercept=0, lty=3) +   geom_label_repel(aes(label =label))
plot(g)
dev.off()

###########################
# oroginal vs. bootstrap values z

pdf(sprintf('../Output/WMD_original_vs_boot_louvain_%s.pdf', nrep), w= 15, h= 15)
g <- ggplot(df2) +
 aes(x = WithinModDegree, y = WithinModDegree.z, shape = Category, fill= Category ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(size=(Count)^0.5), alpha=2) +
 labs(x = "Within-module degree (original)", y = "Within-module degree (bootstrap standardization)", 
 title = sprintf("")) + geom_hline(yintercept=0, lty=3) + geom_vline(xintercept=2.5, lty=3) +
    geom_label_repel(aes(label =label))
plot(g)
dev.off()

pdf(sprintf('../Output/AMC_original_vs_boot_louvain_%s.pdf', nrep), w= 15, h= 15)
g <- ggplot(df2) +
 aes(x = AmongModConnectivity, y = AmongModConnectivity.z, shape = Category, fill= Category ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(size=(Count)^0.5), alpha=2) +
 labs(x = "Among-module connectivity (original)", y = "Among-module connectivity (bootstrap standardization)", 
 title = sprintf("")) + geom_hline(yintercept=0, lty=3) + geom_vline(xintercept=0.62, lty=3) +
    geom_label_repel(aes(label =label))
plot(g)
dev.off()

###########################
# amcz vs. FDR.amc

pdf(sprintf('../Output/amc_z_vs_FDR_louvain_louvain_%s.pdf', nrep), w= 5, h= 4)
g <- ggplot(df2) +
 aes(x = AmongModConnectivity.z, y = FDR.amc, shape = Category, fill= Category ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(size=(Count)^0.5), alpha=2) +
 labs(x = "Among-module connectivity (bootstrap standardization)", y = "FDR", 
 title = sprintf("")) + geom_hline(yintercept=0.05, lty=3, col="red") + geom_vline(xintercept=2.5, lty=2, col="black") 
plot(g)
dev.off()

