################
# infomap code 
# made fujita
# 2021.09.03 
# Suzuki edit
# 2021.12.21
# Suzuki edit
# R 4.0.4 GUI 1.74 Catalina build (7936)
############


file <- list.files("../Data/Infomap")
file <- file[grep('txt', file)]

a <- lapply(file, function(x){ read.table(sprintf('../Data/Infomap/%s', x), header=TRUE, row.names=1)})

spider <- unique( do.call(c, lapply(a, rownames)) )
spider.2col <- cbind(ID=1:length(spider), spider)

otu <- unique( do.call(c, lapply(a, colnames)) )
otu.2col <- cbind(ID=1:length(otu), otu)

dir.create('../Output/Infomap', showWarnings=F)
dir.create('../Output/Infomap/network_Pajek', showWarnings=F)
#dir.create('network_Pajek_mat', showWarnings=F)
dir.create('../Output/Infomap/network_clu', showWarnings=F)

for(a in file){	# a=file[1]

	mat <- matrix(0, nrow=nrow(spider.2col), ncol=nrow(otu.2col))
	rownames(mat) <-spider ; colnames(mat) <- otu

	b <- read.table(sprintf('../Data/Infomap/%s', a), header=TRUE, row.names=1)
	
	# 検出数が0のOTUは削除する
	b  <- t(subset(t(b), rowSums(t(b)) > 0 ))
	 	
	
	for(i in colnames(b)){ #i=colnames(b)[1]
		for(j in rownames(b)){ #j=rownames(b)[1]
			
			mat[j,i] <- b[j,i]		
		}
		
	}
	
	data <- mat
	
	n <- nrow(data)
	m <- ncol(data)
	
	bimode <- rbind(cbind(matrix(0, n, n), data), cbind(t(data), matrix(0, m, m)))
	node.names <- rownames(bimode)
	rownames(bimode) <- c(1:(n+m))
	colnames(bimode) <- c(1:(n+m))
	
	#binary <- bimode
	#binary[which(binary >= 1)] <- 1
	
	library(igraph)
	arrow <- graph.adjacency(bimode, mode="lower", weighted=T)
	weight <- get.edge.attribute(arrow, "weight")
	edge <- get.edgelist(arrow)
	data <- cbind(edge, weight)
	
	data2 <- rbind(rbind(c(sprintf('*vertices %s', n+m),'',''),
					cbind(1:(n+m), sprintf('%s', node.names), 1) ), 
					rbind(c(sprintf('*Arcs %s', nrow(data)),'',''), data))
		
	write.table(data2, sprintf('../Output/Infomap/network_Pajek/%s_adj.txt', a), quote=FALSE, sep='\t', 
				row.names=FALSE, col.names=F)
	
}



#####################################
