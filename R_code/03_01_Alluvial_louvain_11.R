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
library(ggalluvial)
library(ggforce)
library(alluvial)
#devtools::install_github("erblast/easyalluvial")
library(easyalluvial)

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

mat.ori.sp <- readRDS("../Data/mat.ori.sp.rds")
mat.ori.sp.bi <- readRDS("../Data/mat.ori.sp.bi.rds")
meta <- readRDS("../Data/meta.ori.rds")
spider <- data.frame(Species=rownames(meta), Count=rowSums(meta))
spl <- data.frame(spider[order(spider$Count, decreasing=F), ], Order=rev(1:nrow(spider)))

g <- readRDS("../Data/graph.ori.rds")

###########################
#Square Matrix: original

original.sq <- list()

for (u in 1:8 ) {
m <- nrow(mat.ori.sp[[u]])
n <- ncol(mat.ori.sp[[u]])
original.sq[[u]] <- rbind( cbind(matrix(0,m,m), mat.ori.sp[[u]]) , cbind(t(mat.ori.sp[[u]]), matrix(0,n,n) )　)
colnames(original.sq[[u]]) <- c(rownames(mat.ori.sp[[u]]), colnames(mat.ori.sp[[u]]))
}

###########################

mem <- foreach(u=1:8) %dopar% membership(cluster_louvain(g[[u]]))

###########################

df1 <- foreach(u=1:8, .combine=rbind) %dopar% {
Month=rep(u+3, times=nrow(original.sq[[u]]))
mat <- original.sq[[u]]
#bi <- original.sq[[u]]
#bi[which(bi > 0)] <- 1
Count=rowSums(mat)
Count.relative=rowSums(mat) / (sum(mat))
Species=rownames(original.sq[[u]])
Module=paste("X", formatC(as.numeric(mem[[u]]), width=2, flag="0"), sep="")
d1 <- data.frame(Month, Count, Count.relative, Species, Module)
#d1[d1$Module==Module[Species=="Larinia_argiopiformis"], "Module"] <- "M9"
d1[d1$Module==Module[Species=="Neoscona_scylloides"], "Module"] <- "M8"
d1[d1$Module==Module[Species=="Cyclosa_sedeculata"], "Module"] <- "M7"
d1[d1$Module==Module[Species=="Argiope_bruennichi"], "Module"] <- "M6"
d1[d1$Module==Module[Species=="Tetragnatha_caudicula"], "Module"] <- "M5"
d1[d1$Module==Module[Species=="Leucauge_blanda"], "Module"] <- "M4"
d1[d1$Module==Module[Species=="Xysticus_sp."], "Module"] <- "M3"
d1[d1$Module==Module[Species=="Ebrechtella_tricuspidata"], "Module"] <- "M2"
d1[d1$Module==Module[Species=="Oxyopes_sertatus"], "Module"] <- "M1"
d1
}

saveRDS(df1, file="../Data/Alluvial.rds")
write.table(df1, file="../Output/Alluvial.txt", sep='\t', quote=F, row.names=F)

#colvec <- c(brewer.pal(8, "Set1"), "mediumslateblue", rep("grey70", times=30))

colvec2 <- c("#E41A1C","#377EB8","#4DAF4A", "gold", "#F781BF", "mediumslateblue", "darkorange", "burlywood4", rep("grey70", times=30))

###########################

pdf('../Output/Alluvial_louvain.pdf', w= 8, h= 3)

ggplot(df1,
       aes(x = Month, stratum = as.factor(Module), alluvium = Species,
           y = Count.relative,
           fill = as.factor(Module), label = as.factor(Module))) +
  scale_x_discrete(expand = c(.01, .01)) +
  geom_flow(stat = "alluvium") + 
  geom_stratum(alpha = 0.8) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  scale_fill_manual(values = colvec2)

dev.off()
