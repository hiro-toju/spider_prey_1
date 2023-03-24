##########################################
# 2021.10.04
# Suzuki edit
# 2021.10.19
# Suzuki edit
# 2023.03.07 Hirokazu Toju
# R 4.0.4 GUI 1.74 Catalina build (7936)
# month network index  
# bootamization each network index    
##########################################

library(bipartite)
library(parallel)
library(tictoc)
library(foreach)
library(doParallel)
library(igraph)
library(ggplot2)
library(palmerpenguins)
library(cowplot)
library(RColorBrewer)
library(ggrepel)


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

mat.ori.sp  <- readRDS("../Data/mat.ori.sp.rds")
mat.ori.sp.bi  <- readRDS("../Data/mat.ori.sp.bi.rds")

mat.boot.sp  <- readRDS(sprintf("../Data/mat.boot.sp_%s.rds", nrep))
mat.boot.sp.bi  <- readRDS(sprintf("../Data/mat.boot.sp.bi_%s.rds", nrep))
g.boot  <- readRDS(sprintf("../Data/graph.boot_%s.rds", nrep))

###########################
#Square Matrix: original

g <- list()
bet <- list()

for (u in 1:8 ) {
m <- nrow(mat.ori.sp[[u]])
n <- ncol(mat.ori.sp[[u]])
original.sq <- rbind( cbind(matrix(0,m,m), mat.ori.sp[[u]]) , cbind(t(mat.ori.sp[[u]]), matrix(0,n,n) )　)
colnames(original.sq) <- c(rownames(mat.ori.sp[[u]]), colnames(mat.ori.sp[[u]]))
g[[u]] <-  graph_from_adjacency_matrix(original.sq , weighted = T, mode = "undirected" )
bet[[u]] <- as.matrix(round(betweenness(g[[u]], normalized=T), digits = 4 ))
}

################################		
#Square Matrix: ranodomized

boot.bet <- list()

for (u in 1:8 ) {
	boot.bet[[u]] <- foreach(i=1:nrep, .combine=cbind) %dopar% {
	as.matrix(round(betweenness(g.boot[[u]][[i]], normalized=T), digits = 4 ))
}}

######################################################
# Calculation of network indices

Count <- foreach(u=1:8) %dopar% {
	c(rowSums(mat.ori.sp[[u]]), colSums(mat.ori.sp[[u]]))
	}

z.bet <- foreach(u=1:8) %dopar% {
	calc <- foreach(i=1:nrow(bet[[u]]), .combine=rbind) %dopar% {
		(bet[[u]][i] - mean(boot.bet[[u]][i,])) /  sd(boot.bet[[u]][i,])
	}
	calc
	}

P.bet <- foreach(u=1:8) %dopar% {
	calc <- foreach(i=1:nrow(bet[[u]]), .combine=rbind) %dopar% {
		sum(boot.bet[[u]][i,] > bet[[u]][i]) /  length(boot.bet[[u]][i,])
	}
	calc
	}

FDR.bet <- foreach(u=1:8) %dopar% {
	p.adjust(unlist(P.bet[[u]]), method="fdr")
	}

Res.bet <- foreach(u=1:8) %dopar% {
	data.frame(label=rownames(bet[[u]]), month=u+3, Category="spider", Count=Count[[u]], Betweenness=bet[[u]], z.bet=z.bet[[u]], P.bet=P.bet[[u]], FDR.bet=FDR.bet[[u]])
	}

saveRDS(Res.bet, file="../Data/Result.bet.rds")

for (u in 1:8) {
	write.table(Res.bet[[u]], file=sprintf("../Output/Result.bet_%s.txt", u+3), quote=F, sep='\t', row.names=F)
}

df1 <- rbind(Res.bet[[1]], Res.bet[[2]], Res.bet[[3]], Res.bet[[4]], Res.bet[[5]], Res.bet[[6]], Res.bet[[7]], Res.bet[[8]])

df1[grep("H_", df1$label), "Category"] <- "prey"

######################################################

df1$month <- factor(df1$month ,level= c('4','5','6','7','8','9','10','11'))

saveRDS(df1, file=sprintf("../Data/Centrality_boot_%s.rds", nrep))
write.table(df1, file=sprintf("../Output/Centrality_boot_%s.txt", nrep), sep="\t", quote=F, row.names=F)


pdf(sprintf('../Output/Counts_Bet.z_boot_%s.pdf', nrep), w= 16, h= 8)
g <- ggplot(df1) +
 aes(x = log10(Count+1), y = z.bet , shape = Category, fill= Category ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(size=4) +
 labs(x = "log10(Prey detection count + 1)", y = "Betweenness (z-standardized)", 
 title = sprintf("")) +  geom_hline(yintercept=0, lty=2, col="black") +
  facet_wrap(~month, ncol=4)+
   geom_label_repel(aes(label =df1$label))
  
plot(g)
dev.off()


