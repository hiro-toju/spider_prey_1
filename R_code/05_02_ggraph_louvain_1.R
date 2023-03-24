#########################
# 2022.04.07
# Suzuki writing
# 2023.03.13 Hirokazu Toju
# ggraph 
#########################

library("easypackages")

libraries(c( 'ggplot2', 'Rcpp', 'dplyr', 'ggforce', 'igraph', 'scales', 'digest', 'gtable', 'ggrepel', 'viridis', 'rlang', 'tidygraph', 'graphlayouts', 'withr' ,'ggraph'))
library(RColorBrewer)

set.seed(123)
nrep <- 1000

########################

original.sq  <- readRDS(file="../Data/meta.ori.sq.rds")
mem.ori <- readRDS(file="../Data/meta. mem.ori.rds")
mod.ori <- readRDS(file="../Data/meta.mod.ori.rds")

g.meta <-  graph_from_adjacency_matrix(original.sq , weighted = T, mode = "undirected" )

stat <- readRDS(sprintf("../Output/Olesen_boot_louvain_%s.rds", nrep))

#colvec <- brewer.pal(8, "Accent")[sample(1:8)]
colvec <- c("#BEAED4", "#FFFF99", "#666666", "#F0027F", "#386CB0", "#FDC086", "#BF5B17", "#7FC97F")

########################
# layout = "stress"
# label FALSE
########################
pdf('../Output/metanetwork_louvain_stress.pdf',h= 10, w=10 )

g <- ggraph(g.meta, layout = "stress") +  geom_edge_link0(width=0.2, colour= 'grey60' ) + geom_node_point( aes(size=(stat$Count)^0.5, shape = as.factor(stat$Category), fill=as.factor(stat$module)), show.legend=TRUE) +
scale_shape_manual(values = c(21,23)) +
scale_fill_manual(values=colvec)

g <- g +　guides()+theme_graph()+theme(legend.position = "right")+theme(text =element_text("Helvetica")) 

plot(g)
dev.off()

########################
# layout = "stress"
# label TRUE
########################
pdf('../Output/metanetwork_louvain_stress_label.pdf',h= 10, w=10 )

g <- ggraph(g.meta, layout = "stress") +  geom_edge_link0(width=0.2, colour= 'grey60' ) + geom_node_point( aes(size=(stat$Count)^0.5, shape = as.factor(stat$Category), fill=as.factor(stat$module)), show.legend=TRUE) +
scale_shape_manual(values = c(21,23)) +
scale_fill_manual(values=colvec)

g <- g +　geom_node_label(aes(label = as.vector(rownames(original.sq) ) ),repel = TRUE,label.size = 0.5 ,nudge_y =  -0.03 ) 

g <- g +　guides()+theme_graph()+theme(legend.position = "right")+theme(text =element_text("Helvetica")) 

plot(g)
dev.off()
########################
########################
# layout = "stress"
# label TRUE
########################
pdf('../Output/metanetwork_louvain_stress_label.pdf',h= 70, w=70 )

g <- ggraph(g.meta, layout = "stress") +  geom_edge_link0(width=0.2, colour= 'grey60' ) + geom_node_point( aes(size=(stat$Count)^0.5, shape = as.factor(stat$Category), fill=as.factor(stat$module)), show.legend=TRUE) +
scale_shape_manual(values = c(21,23)) +
scale_fill_manual(values=colvec)

g <- g +　geom_node_label(aes(label = as.vector(rownames(original.sq) ) ),repel = TRUE,label.size = 0.5 ,nudge_y =  -0.03 ) 

g <- g +　guides()+theme_graph()+theme(legend.position = "right")+theme(text =element_text("Helvetica")) 

plot(g)
dev.off()
########################






