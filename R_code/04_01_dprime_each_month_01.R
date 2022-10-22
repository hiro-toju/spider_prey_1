######################
# 2021.01.29.
# Suzuki writing
# 2021.09.16
# Suzuki edit
# 2022.06.07
# Suzuki edit
#
# d prime calculation 
# betweennes value
# degree value
# spider sp. level, hexa_id level
######################

library(bipartite)
library(igraph)

month.uni <- c(4:11)
for (u in month.uni ) { 
	

mat <- as.matrix(read.table(sprintf('../Data/prey.id.prey.matrix.agg.%s.txt',u),header=T, sep ='\t',row.names=1)	)
#head(mat)

mat1 <- as.matrix( mat[order(rownames(mat),decreasing =F), ] )
head(mat1)
mat2 <- t(subset(t(mat1), rowSums(t(mat1)) > 0  ))
mat3 <-   subset(mat2, rowSums(mat2)  > 0 )
dim(mat3)

original <- mat3
head(original)
prey.indi <- as.matrix(colSums(mat3) )

original.t <- t(original)
head(original.t)

dprime.spider <- as.matrix(dfun(original)[[1]])
dprime.spider.mean <- mean(dprime.spider)
dprime.prey <-  as.matrix(dfun(original.t)[[1]])
dprime.prey.mean <- mean(dprime.prey)

# 検出された個体数をインプット
spider.indi <- as.matrix(read.table(sprintf('../Data/spider.agg.%s.txt',u),header=T, sep ='\t',row.names=1)	)

rownames(spider.indi) == rownames(dprime.spider)
rownames(prey.indi) == rownames(dprime.prey)



# dprime.mean output
write.table(c(dprime.spider.mean, dprime.prey.mean),sprintf('../Output/dprime.mean.%s.txt',u),sep='\t' )


dprime.data <- rbind(dprime.spider,dprime.prey)
dprime.data.corre <- 1-dprime.data


# square original matrix
m <- nrow(original)
n <- ncol(original)

original.sq <- rbind( cbind(matrix(0,m,m), original) , cbind(t(original), matrix(0,n,n) )　)
dim(original.sq)
colnames(original.sq) <- c(rownames(original), colnames(original))

g <-  graph_from_adjacency_matrix(original.sq , weighted = T,mode =  "undirected" )

# mojule 
# between <-  cluster_edge_betweenness(g, weights = E(g)$weight)
# between.mojule <-  as.matrix(membership(between) )



# betweenness centrality 
# 有効数字を4桁に指定する
#between <- as.matrix(betweenness(g,normalized=T))
between <- as.matrix(round(betweenness(g,normalized=T) ,digits = 4 ))

# dgree centtality
# 有効数字を4桁に指定する
degree <- as.matrix( degree(g) )
degree.st <- as.matrix(round( degree(g)/(nrow(original.sq)-1) ,digits = 4 ))

# empty matrix data	
ncol = 7
output.data <-  as.data.frame(matrix(NA,ncol=ncol,nrow= length(rownames(original.sq) )  ))
colnames(output.data) <- c('dprime','dprime.hosei','degree','degree.st','betweenness','class','individuals')
rownames(output.data)	<- colnames(original.sq)

# それぞれの値の代入
output.data$degree <- degree
output.data$degree.st <- degree.st
output.data$dprime <- dprime.data
output.data$betweenness <- between
output.data$dprime.hosei <- dprime.data.corre
output.data$class <- c(rep('spider', length(dprime.spider)), rep('prey',length(dprime.prey)) )
output.data$individuals <- c(spider.indi , prey.indi)


write.table(cbind(rownames(output.data), output.data),sprintf('../Output/eachmonth_dprime_table.%s.txt',u),sep='\t',row.names=F)	

	}

	
	
	
