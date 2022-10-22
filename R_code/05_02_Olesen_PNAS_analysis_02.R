
#########################
# 2022.05.14
# Suzuki writing

# Olsen module analysis
#########################

library(igraph)
library(sna)


# as.marix　に変換が必要
original <- as.matrix( read.table('../Data/hexa_id.prey.matrix.agg.all.sort.txt',header=T,sep ='\t',row.names=1) )
dim(original)
m <- nrow(original)
n <- ncol(original)

# Convert to square matrix
original.sq <- rbind( cbind(matrix(0,m,m), original) , cbind(t(original), matrix(0,n,n) )　)
colnames(original.sq) <- c(rownames(original), colnames(original))
dim(original.sq)


original.sq.binary <- original.sq
mode(original.sq.binary) <- "numeric"
original.sq.binary[which(original.sq.binary >= 1)] <- 1

#as.matrix(degree( original.sq) )

module.data.info　<-  read.table('../Data/ggraph.info.txt' ,header=T,sep ='\t',row.names=1)
head(module.data.info)
modu.no <- unique(module.data.info$module.no)




#for fanction
dir.create('../Output/Olesen' ,showWarnings = F )
dir.create('../Output/Olesen/each.module.data' ,showWarnings = F )



#plot.data <- cbind(c.score,matrix(NA))

degree.z.score.all <- c()

	for (i in modu.no) {
each.modu.info <- subset(module.data.info, module.data.info$module.no == i )
each.modu.data  <- subset(original.sq , rownames(original.sq) %in% rownames(each.modu.info) ) 

write.table(cbind(rownames(each.modu.data),each.modu.data), file=sprintf("../Output/Olesen/each.module.data/module.data.%s.txt",i),sep="\t",quote=F,row.name=F,col.name=T)

mode(each.modu.data) <- "numeric"
each.modu.data[which(each.modu.data >= 1)] <- 1
#check
#max(each.modu.data)
write.table(cbind(rownames(each.modu.data),each.modu.data), file=sprintf("../Output/Olesen/each.module.data/module.data.binary.%s.txt",i),sep="\t",quote=F,row.name=F,col.name=T)

sum <- as.matrix(rowSums(each.modu.data))
z.score <- matrix(NA, nrow= length(sum[,1]), ncol=1)
dim(z.score)
rownames(z.score) <- rownames(each.modu.data)
colnames(z.score) <- 'degree_z'

for( t in 1:length(sum)  ){
	z.score[t,1] <- (sum[t,1]-mean(sum)) / sd(sum)	
}
degree.z.score.all <- rbind(degree.z.score.all,z.score)

write.table(cbind(rownames(z.score), z.score), file=sprintf("../Output/Olesen/each.module.data/degree.z.score.%s.txt",i),sep="\t",quote=F,row.name=F,col.name=T)

}

#check
dim(degree.z.score.all)




# conectivity k_it
c.mat <-  matrix(NA,nrow = length(colnames(original.sq)) ,ncol= length(modu.no) )
rownames(c.mat) <- colnames(original.sq)
colnames(c.mat) <- modu.no
 
	for (i in modu.no) {
			
each.modu.info <- subset(module.data.info, module.data.info$module.no == i )
each.modu.data  <- subset(original.sq , rownames(original.sq) %in% rownames(each.modu.info) )
 
for (u in 1:length(colnames(original.sq)) ) {
c.mat[u,i] <- length(subset( rownames(original.sq.binary)[u] , each.modu.data[,u] > 0 ))
 }
 }
 
 head(c.mat)


write.table(cbind(rownames(c.mat), c.mat), file='../Output/Olesen/module.k_it.txt',sep="\t",quote=F,row.name=F,col.name=T)


ncol(each.modu.data)
nrow(each.modu.data)




# merge data

degree.z.score.all <- as.data.frame(degree.z.score.all)
#rownames(degree.z.score.all)
degree.z.score.all <- cbind(rownames(degree.z.score.all), degree.z.score.all)
head(degree.z.score.all)

c.score <- as.data.frame(read.table('../Data/conectivity.c.score.txt',header=T,sep ='\t',row.names=1) )
c.score <- cbind(rownames(c.score), c.score) # rownames(c.score)
head(c.score)

colnames(degree.z.score.all)[1] <- "name"
colnames(c.score)[1] <- "name"

mer.data <- merge(degree.z.score.all, c.score , by ='name', incomparables =NA) 
dim(mer.data)

write.table(mer.data, file='../Output/Olesen/forplot.data.txt',sep="\t",quote=F,row.name=F,col.name=T)

pdf('../Output/Olesen/module_degree_plot.pdf',h = 8 ,w = 8)
plot(mer.data$among.module.connectivity, mer.data$degree_z)

dev.off()


