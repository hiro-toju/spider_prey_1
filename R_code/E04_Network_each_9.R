#########################
# 2022.04.07
# Suzuki
# 2023.03.13 Hirokazu Toju
# 2023.03.14 Hirokazu Toju
# ggraph 
#########################

library("easypackages")

libraries(c( 'ggplot2', 'Rcpp', 'dplyr', 'ggforce', 'igraph', 'scales', 'digest', 'gtable', 'ggrepel', 'viridis', 'rlang', 'tidygraph', 'graphlayouts', 'withr' ,'ggraph'))
library(bipartite)
library(RColorBrewer)
library('dplyr')
library(parallel)
library(tictoc)
library(foreach)
library(doParallel)
library(gridExtra)

set.seed(123)

# check core
detectCores(logical = FALSE)
detectCores()
registerDoParallel(cores=4)

n.core <- 6
month.uni <- 4:11

#########################
# Netowrk 

mat.ori.sp <- readRDS("../Data/mat.ori.sp.rds")
mat.ori.sp.bi <- readRDS("../Data/mat.ori.sp.bi.rds")
g <- readRDS("../Data/graph.ori.rds")

label <- foreach(i=1:8) %dopar% {
	data.frame(Species=c(rownames(mat.ori.sp[[i]]), colnames(mat.ori.sp[[i]])))
}

#########################
# info

hexa.info <- read.table('../Data/03_taxonomy_table_mer.Hexa_ID_notUn.2.txt',header=T,sep ='\t',row.names=1)	

info.prey <- data.frame(Species=rownames(hexa.info), Order=hexa.info[, c("order")])

allu <- readRDS("../Data/Alluvial.rds")

info2 <- left_join(allu, info.prey, by="Species")
info2$Order[is.na(info2$Order)] <- "spider"

#########################
# Color taxa

tmp <- data.frame(info2, Order.Col= info2$Order, Module.Col=info2$Module, Shape=rep("spider", times=nrow(info2)))

lv.Order <- c('spider', 'Diptera', 'Hemiptera', 'Hymenoptera', 'Orthoptera', 'Collembola', 'Lepidoptera', 'Coleoptera', 'Ephemeroptera', 'Blattodea', 'Thysanoptera', 'Odonata', 'Psocoptera', 'Dermaptera', 'Strepsiptera', 'Isopoda', 'Lithobiomorpha', "other")

col.Order <- c('darkmagenta', '#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#FBB4AE', '#B3CDE3', '#DECBE4', '#FED9A6', '#FFFFCC', '#E5D8BD', '#FDDAEC', '#F2F2F2', 'grey70')

"%not.in%" <- Negate("%in%")
tmp$Order.Col[tmp$Order.Col %not.in% lv.Order] <- "other"

########################
# Color module

lv.Module <- c('M1', 'M2', 'M3', 'M4', 'M5', 'M6', 'M7', "M8", 'other')

col.Module <- c('#E41A1C', '#377EB8', '#4DAF4A', 'gold', '#F781BF', 'mediumslateblue', 'darkorange', "burlywood4", 'grey70')

tmp$Module.Col[tmp$Module.Col %not.in% lv.Module] <- "other"

########################
# Shape

tmp[grep("H_", tmp$Species),"Shape"] <- "prey"

########################
# info

stat <- foreach(i=1:8) %dopar% {
	left_join(label[[i]], tmp[tmp$Month==i + 3, ], by="Species")
}

########################
# layout = "stress"
# label FALSE
########################
# Order

o <- foreach(i=1:8) %dopar% {
	ggraph(g[[i]], layout = "stress") +  geom_edge_link0(width=0.2, colour= 'grey60' ) + geom_node_point( aes(shape = factor(stat[[i]]$Shape), fill=factor(stat[[i]]$Order.Col, levels= lv.Order), alpha=1), show.legend=TRUE) +
scale_shape_manual(values = c(21,23)) +
scale_fill_manual(values= col.Order[table(factor(stat[[i]]$Order.Col, levels= lv.Order))!=0]) +　
theme(legend.position = "none")  +
labs(title = sprintf('%s', i + 3))
}

pdf("../Output/Network_each_taxa.pdf", w=10, h=10)   	 	
grid.arrange(o[[1]], o[[2]], o[[3]], o[[4]], o[[5]], o[[6]], o[[7]], o[[8]], ncol=3)
dev.off()

pdf("../Output/Network_each_taxa_wide1.pdf", w=13, h=10)   	 	
grid.arrange(o[[1]], o[[2]], o[[3]], o[[4]], o[[5]], o[[6]], o[[7]], o[[8]], ncol=3)
dev.off()

pdf("../Output/Network_each_taxa_wide2.pdf", w=15, h=10)   	 	
grid.arrange(o[[1]], o[[2]], o[[3]], o[[4]], o[[5]], o[[6]], o[[7]], o[[8]], ncol=3)
dev.off()


########################
# Module

m <- foreach(i=1:8) %dopar% {
	ggraph(g[[i]], layout = "stress") +  geom_edge_link0(width=0.2, colour= 'grey60' ) + geom_node_point( aes(shape = factor(stat[[i]]$Shape), fill=factor(stat[[i]]$Module.Col, levels= lv.Module), alpha=1), show.legend=TRUE) +
scale_shape_manual(values = c(21,23)) +
scale_fill_manual(values= col.Module[table(factor(stat[[i]]$Module.Col, levels= lv.Module))!=0]) +　
theme(legend.position = "none")  +
labs(title = sprintf('%s', i + 3))
}

pdf("../Output/Network_each_module.pdf", w=10, h=10)   	 	
grid.arrange(m[[1]], m[[2]], m[[3]], m[[4]], m[[5]], m[[6]], m[[7]], m[[8]], ncol=3)
dev.off()

pdf("../Output/Network_each_module_wide1.pdf", w=13, h=10)   	 	
grid.arrange(m[[1]], m[[2]], m[[3]], m[[4]], m[[5]], m[[6]], m[[7]], m[[8]], ncol=3)
dev.off()

pdf("../Output/Network_each_module_wide2.pdf", w=15, h=10)   	 	
grid.arrange(m[[1]], m[[2]], m[[3]], m[[4]], m[[5]], m[[6]], m[[7]], m[[8]], ncol=3)
dev.off()

