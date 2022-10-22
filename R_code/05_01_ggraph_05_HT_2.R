#########################
# 2022.04.07
# Suzuki writing
# ggraph 
# all data network 
#########################


#install.packages(c( 'ggplot2', 'Rcpp', 'dplyr', 'ggforce', 'igraph', 'scales', 'digest', 'gtable', 'ggrepel', 'viridis', 'rlang', 'tidygraph', 'graphlayouts', 'withr' ,'ggraph'))

# 複数のパッケージを読み込みたいとき
#　use "easypackages"
# libraries( c(xx,xx,...) )

#install.packages("devtools")
#install.packages("easypackages")
library("easypackages")

libraries(c( 'ggplot2', 'Rcpp', 'dplyr', 'ggforce', 'igraph', 'scales', 'digest', 'gtable', 'ggrepel', 'viridis', 'rlang', 'tidygraph', 'graphlayouts', 'withr' ,'ggraph'))


########################
# input infomation data
#
########################


# as.marix　に変換が必要
original <- as.matrix( read.table('../Data/hexa_id.prey.matrix.agg.all.sort.txt',header=T,sep ='\t',row.names=1) )
dim(original)

# クモの生態についての種情報
spider.info <- read.table('../Data/species.gakumei.tmp_20220324.txt',header=T,sep ='\t',row.names=1)

# 各月のクモ種だけを取り出す
spi.eco <- subset(spider.info, spider.info[,4] %in% rownames(original) )

#data.info[,'tax'] <- c(spi.eco[,'ecology'],  ## ???

hexa.info <- read.table('../Data/hexa.tax.info.ALL.txt',header=T,sep ='\t',row.names=1)

dim(hexa.info)

rownames(hexa.info) == colnames(original)

m <- nrow(original)
n <- ncol(original)

original.sq <- rbind( cbind(matrix(0,m,m), original) , cbind(t(original), matrix(0,n,n) )　)
colnames(original.sq) <- c(rownames(original), colnames(original))
#head(original.sq)
dim(original.sq)



data.info <- as.data.frame(read.table('../Data/ggraph.info.txt',header=T,sep ='\t',row.names=1) )
head(data.info)

# Module sums in each month


mat4 <- as.data.frame(read.table('../Data/prey.id.prey.matrix.agg.4.txt',header=T,sep ='\t',row.names=1) )
mat5 <- as.data.frame(read.table('../Data/prey.id.prey.matrix.agg.5.txt',header=T,sep ='\t',row.names=1) )
mat6 <- as.data.frame(read.table('../Data/prey.id.prey.matrix.agg.6.txt',header=T,sep ='\t',row.names=1) )
mat7 <- as.data.frame(read.table('../Data/prey.id.prey.matrix.agg.7.txt',header=T,sep ='\t',row.names=1) )
mat8 <- as.data.frame(read.table('../Data/prey.id.prey.matrix.agg.8.txt',header=T,sep ='\t',row.names=1) )
mat9 <- as.data.frame(read.table('../Data/prey.id.prey.matrix.agg.9.txt',header=T,sep ='\t',row.names=1) )
mat10 <- as.data.frame(read.table('../Data/prey.id.prey.matrix.agg.10.txt',header=T,sep ='\t',row.names=1) )
mat11 <- as.data.frame(read.table('../Data/prey.id.prey.matrix.agg.11.txt',header=T,sep ='\t',row.names=1) )

sum4 <- data.frame(Name=c(rownames(mat4), colnames(mat4)), Count_4=c(rowSums(mat4), colSums(mat4)))
sum5 <- data.frame(Name=c(rownames(mat5), colnames(mat5)), Count_5=c(rowSums(mat5), colSums(mat5)))
sum6 <- data.frame(Name=c(rownames(mat6), colnames(mat6)), Count_6=c(rowSums(mat6), colSums(mat6)))
sum7 <- data.frame(Name=c(rownames(mat7), colnames(mat7)), Count_7=c(rowSums(mat7), colSums(mat7)))
sum8 <- data.frame(Name=c(rownames(mat8), colnames(mat8)), Count_8=c(rowSums(mat8), colSums(mat8)))
sum9 <- data.frame(Name=c(rownames(mat9), colnames(mat9)), Count_9=c(rowSums(mat9), colSums(mat9)))
sum10 <- data.frame(Name=c(rownames(mat10), colnames(mat10)), Count_10=c(rowSums(mat10), colSums(mat10)))
sum11 <- data.frame(Name=c(rownames(mat11), colnames(mat11)), Count_11=c(rowSums(mat11), colSums(mat11)))

all.info <- data.frame(Name=rownames(data.info), data.info)


#https://gist.github.com/yuu-ito/7094938

merge2 <- function(dfs, ...)
{
　 # res <- merge2(list(Df1, Df2, Df3), by="ID")
　 base <- dfs[1]
　 lapply(dfs[-1], function(i) base <<- merge(base, i, ...)) # [1]
  return(base)
}

all.sum <- merge2(list(all.info, sum4, sum5, sum6, sum7, sum8, sum9, sum10, sum11), by="Name", all.x=TRUE)

counts.months <- as.matrix(all.sum[, 10:17])
counts.months[is.na(counts.months)] <- 0

module.sum <- matrix(NA, nrow=length(unique(all.sum$module.no)), ncol=8)

for (i in 1:length(unique(all.sum$module.no))) {
	module.sum[i,] <- colSums(subset(counts.months, all.sum$module.no==i))
}

rownames(module.sum) <- 1:length(unique(all.sum$module.no))
colnames(module.sum) <- c("April", "May", "June", "July", "August", "September", "October", "November")

module.prop <- module.sum/rowSums(module.sum)
module.selected <- module.prop[rev(c(3,10,8,18,4,14)),]

library(lattice)
levelplot(t(module.selected), col.regions=colorRampPalette(c("white", "deeppink3 ")))



col.pal <- data.frame(Module.No=unique(data.info$module.no), Color=unique(data.info$col.moju))

pdf('../Output/module.color.pdf')
pie(rep(1, nrow(col.pal)), col=col.pal$Color)
dev.off()





########################
# make figure
########################

# plot degree x betweenness


pdf('../Output/degree.bet.plot.pdf')
plot(data.info[,'size.cent'] ,data.info[,'degree'])

dev.off()

class(original.sq) 

########################
# layout = "stress"
# label ari
########################
pdf('../Output/all.data.network_stress.label.pdf',h= 70, w=70 )

# geom_edge_link0() のカラーは1の長さでないといけない？
g <- ggraph(original.sq ,layout = "stress") +  geom_edge_link0(width=0.2,colour= 'grey60' )
g <-  g + geom_node_point( aes(size =10  ), shape = data.info$shape,show.legend=TRUE, color = data.info$col.moju)

g <- g +　guides()+theme_graph()+theme(legend.position = "right")
g <- g +  geom_node_label(aes(label = as.vector(rownames(original.sq) ) ),repel = TRUE,label.size = 0.5 ,nudge_y =  -0.03 )+theme(text =element_text("Helvetica")) 

plot(g)

dev.off()
########################


########################
# layout = "centrality"
# label ari
########################

bet <- sna::betweenness(original.sq, rescale=FALSE)

pdf('../Output/all.data.network_centrality.label.pdf', h= 12, w=12 )

g <- ggraph(original.sq ,layout = "centrality", cent = log10(bet+100)) +  geom_edge_link0(width=0.2,colour= 'grey60' )
g <-  g + geom_node_point( aes(size =10  ), shape = data.info$shape,show.legend=TRUE, color = data.info$col.moju)

g <- g +　guides()+theme_graph()+theme(legend.position = "right")
g <- g +  geom_node_label(aes(label = as.vector(rownames(original.sq) ) ),repel = TRUE, label.size = 0.5 ,nudge_y =  -0.03 )+theme(text =element_text("Helvetica")) 

plot(g)

dev.off()
########################

########################
# layout = "centrality"
# label nasi
########################

bet <- sna::betweenness(original.sq, rescale=FALSE)

pdf('../Output/all.data.network_centrality.pdf', h= 12, w=12 )

g <- ggraph(original.sq ,layout = "centrality", cent = log10(bet+100)) +  geom_edge_link0(width=0.2,colour= 'grey60' )
g <-  g + geom_node_point( aes(size =10  ), shape = data.info$shape,show.legend=TRUE, color = data.info$col.moju)

#g <- g +　guides()+theme_graph()+theme(legend.position = "right")
#g <- g +  geom_node_label(aes(label = as.vector(rownames(original.sq) ) ),repel = TRUE, label.size = 0.5 ,nudge_y =  -0.03 )+theme(text =element_text("Helvetica")) 

plot(g)

dev.off()
########################


########################
# layout = "stress"
# label nasi
########################
pdf('../Output/all.data.network_stress.pdf')

# geom_edge_link0() のカラーは1の長さでないといけない？
g <- ggraph(original.sq ,layout = "stress") +  geom_edge_link0(width=0.2,colour= 'grey60' )
g <-  g + geom_node_point( aes(size = data.info$size.cent), shape = data.info$shape,show.legend=TRUE
, color = data.info$col.moju)
g <- g +　guides()+theme_graph()+theme(legend.position = "right")+theme(text =element_text("Helvetica")) 


plot(g)

dev.off()
########################
#layout = "nicely"

pdf('../Output/all.data.network_nicely.pdf')

# geom_edge_link0() のカラーは1の長さでないといけない？
g <- ggraph(original.sq ,layout = "nicely") +  geom_edge_link0(width=0.2,colour= 'grey60' )
g <-  g + geom_node_point( aes(size = data.info$size.cent), shape = data.info$shape,show.legend=TRUE
, color = data.info$col.moju)
g <- g +　guides()+theme_graph()+theme(legend.position = "right")+theme(text =element_text("Helvetica")) 


plot(g)

dev.off()
########################
#layout = "kk"
########################
pdf('../Output/all.data.network_kk.pdf')

# geom_edge_link0() のカラーは1の長さでないといけない？
g <- ggraph(original.sq ,layout = "kk") +  geom_edge_link0(width=0.2,colour= 'grey60' )
g <-  g + geom_node_point( aes(size = data.info$size.cent), shape = data.info$shape,show.legend=TRUE
, color = data.info$col.moju)
g <- g +　guides()+theme_graph()+theme(legend.position = "right")+theme(text =element_text("Helvetica")) 



plot(g)

dev.off()







