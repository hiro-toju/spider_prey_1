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

# ramdamization set up
nrep <- 1000
n.core <- 6

# each month 
month.uni <- c(4:11)

###########################

mat.ori.sp <- readRDS("../Data/mat.ori.sp.rds")
mat.ori.sp.bi <- readRDS("../Data/mat.ori.sp.bi.rds")

df1 <- read.table(sprintf('../Output/NetworkIndices_louvain_%s.txt', nrep), header=T)

###########################

Connectance <- c()

for (i in 1:8) {
	n <- nrow(mat.ori.sp.bi[[i]])
	m <- ncol(mat.ori.sp.bi[[i]])
	Connectance[i] <- sum(mat.ori.sp.bi[[i]]) / (n*m)
}

df2 <- data.frame(df1, Connectance)
class(df2$Month) <- "numeric"

###########################

sp <- rep(15, times=8)

sp.H2 <- sp
sp.H2[which(df2$FDR.H2 >= 0.05)] <- 0

sp.wNODF <- sp
sp.wNODF[which(df2$FDR.wNODF >= 0.05)] <- 0

sp.babMod <- sp
sp.babMod[which(df2$FDR.babMod >= 0.05)] <- 0
###########################

p1 <- ggplot(df2, aes(x=as.numeric(Month), y=Connectance)) + geom_line(color="grey70") + geom_point(color="deepskyblue ", size=4, pch=15) + scale_y_continuous(limits=c(0,0.3)) + geom_hline(yintercept=0, lty=3, color="red")

p2 <- ggplot(df2, aes(x=as.numeric(Month), y=z.H2)) + geom_line(color="grey70") + geom_point(color="lightpink1 ", size=4, pch=15) + scale_y_continuous(limits=c(0,15)) + geom_hline(yintercept=0, lty=3, color="red")


p3 <- ggplot(df2, aes(x=as.numeric(Month), y=z.wNODF)) + geom_line(color="grey70") + geom_point(color="lightpink1 ", size=4, pch=15) + scale_y_continuous(limits=c(-5,5)) + geom_hline(yintercept=0, lty=3, color="red")


p4 <- ggplot(df2, aes(x=as.numeric(Month), y=z.ModBablouvain)) + geom_line(color="grey70") + geom_point(color="lightpink1 ", size=4, pch=15) + scale_y_continuous(limits=c(0,10)) + geom_hline(yintercept=0, lty=3, color="red")

pdf('../Output/Netowrk_indices_louvain.pdf', w= 6, h= 6)
gridExtra::grid.arrange(p1, p2, p3, p4)
dev.off()
