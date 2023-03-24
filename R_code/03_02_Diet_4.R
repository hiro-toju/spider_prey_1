#########################
# 2021.12.21 Sayaka Suzuki
# 2023.03.14 Hirokazu Toju
#########################

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
library(gridExtra)

# check core
detectCores(logical = FALSE)
detectCores()
registerDoParallel(cores=4)

n.core <- 6

#########################

diet.info <-  read.table('../Data/hexa.id.diet.merge_id_sort.txt', header=T)
colnames(diet.info) <- c("Family", "Species", "Diet")

allu <- readRDS("../Data/Alluvial.rds")
allu.prey <- allu[grep("H_", allu$Species),]

month.uni <- 4:11

#########################

df1 <- foreach(u=4:11) %dopar% {
	left_join(allu.prey[allu.prey$Month==u, ], diet.info, by="Species")
}

df2 <- foreach(i=1:8) %dopar% {
	Diet.Col <- df1[[i]]$Diet
	Diet.Col[which(Diet.Col=="herbivorous")] <- 'darkseagreen3'
	Diet.Col[which(Diet.Col=="predatory/parasitic")] <- 'lightgoldenrod4'
	Diet.Col[which(Diet.Col=="detritivorous")] <- 'chocolate2'
	Diet.Col[which(Diet.Col=="variable")] <- 'grey28'
	Diet.Col[which(Diet.Col=="unidentified")] <- 'grey50'
	data.frame(df1[[i]], Diet.Col)	
}

col.vector <- c('darkseagreen3','lightgoldenrod4','chocolate2','grey28','grey50')

#lf$diet <- factor(lf$diet, level=rev(c("herbivorous", "predatory/parasitic" ,"detritivorous","variable",'unidentified'))  ) 

#########################

pdf("../Output/Diet_Module.pdf", w=10, h=3)    	  		 	

g <- foreach(i=1:8) %dopar% {
	ggplot(df2[[i]], aes(x = factor(Module, levels=sort(unique(Module), decreasing=T)), y = Count, fill = factor(Diet, levels=c("herbivorous", "predatory/parasitic" ,"detritivorous","variable",'unidentified'))))　+
	 geom_bar(stat = "identity", position='fill', width=0.9)+
	  scale_fill_manual(values=col.vector) + 
	  theme(legend.position="none") +
	  coord_flip() + scale_y_reverse() +
	  labs(title = sprintf('%s', i + 3)) +
	  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
}

grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], g[[6]], g[[7]], g[[8]], nrow=1)

dev.off()


pdf("../Output/Diet_Module_label.pdf", w=20, h=3)    	  		 	

g <- foreach(i=1:8) %dopar% {
	ggplot(df2[[i]], aes(x = factor(Module, levels=sort(unique(Module), decreasing=T)), y = Count, fill = factor(Diet, levels=c("herbivorous", "predatory/parasitic" ,"detritivorous","variable",'unidentified'))))　+
	 geom_bar(stat = "identity", position='fill', width=0.9)+
	  scale_fill_manual(values=col.vector) + 
	  theme(legend.position="right") +
	  coord_flip() + scale_y_reverse() +
	  labs(title = sprintf('%s', i + 3)) +
	  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
}

grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], g[[6]], g[[7]], g[[8]], nrow=1)

dev.off()


