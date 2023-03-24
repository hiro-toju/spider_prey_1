
#########################
# 2022.09.07
# Suzuki writing
# family level prey composition 
# permanova test
#########################



library(vegan)


#Combine data for all months

month.uni <- as.character(c(4:11))

mat.all <- c()

#for fanction
	for (i in month.uni) {

month.d <- read.table(sprintf('../Data/family.matrix.sum.%s.spider.name.txt',i),header=T,sep ='\t')

month.d_2 <- cbind(month=i,month.d)
head(month.d_2)

mat.all <- rbind(mat.all, month.d_2)
}

dim(mat.all)
head(mat.all)

write.table(mat.all,'../Output/family.matrix.sum.all.spider.name.txt',sep='\t',row.names=F)		

mat.all.otu <- mat.all[,c(-1,-2)]
head(mat.all.otu)
max(mat.all.otu)
class(mat.all.otu)

#beta.dist <- vegdist(mat.all.otu, method="jaccard") 

result <- adonis(mat.all.otu ~ mat.all[,1]* mat.all[,2], permutations=10000)

output <- as.matrix(result$aov.tab)

write.table(cbind(rownames(output),output) ,file= '../Output/prey.family.adonis.result.txt',sep='\t',row.names=F)	




