######################
# 2021.03.08.
# Suzuki writing
# 2021.03.25
# Suzuki edit
# 2021.08.26
# Suzuki edit
# Sampling spider composition using update data
######################

dir.create("../Output")

library(tidyverse)


df <- read.table("../Data/spinfo.merge.spider.txt",header=T)

head(df)

# separate month ##########
# aggregate spider species ######


#source('~/Desktop/Function.R')
month.uni <- as.character(c(4:11))

# agg.sum2 <- c()
 
# col name はsort関数で並べ替えておく。merge関数のときに自動で昇順に並び替わるため
  agg.sum <- matrix(NA, nrow= length(unique(df[,"species.y"])),
 					   ncol=length(unique(df[,"month"])),
 					   dimnames=list(sort(unique(df[,"species.y"])), month.uni))					   
rowname.sum <- unique(df[,"species.y"])

					   
 for (i in month.uni)  {
 	 
agg <- subset(df, df$month == i)


spname <-  select(agg, 12)

agg.spider <- as.matrix(table(spname))


agg.spider_2 <- merge( as.data.frame(rowname.sum), as.data.frame(table(spname)),by.x="rowname.sum", by.y="spname",all=T)

agg.spider_3 <-agg.spider_2[,2]


#colnames(agg.spider_3) <- c(i)


write.table(cbind(row.names(agg.spider),agg.spider[,1]), file=sprintf("../Data/spider.agg.%s.txt",i),sep="\t",quote=F,row.name=F,col.name=T)

#agg.sum2 <- cbind(agg.sum, agg.spider)
#agg.sum[,i] <-  agg.spider[rownames(agg.sum), ]
 agg.sum[,i] <- agg.spider_3	
 
 }
 
agg.sum[is.na(agg.sum)] <- 0


# separate month end ##########

 
write.table(cbind(rownames(agg.sum),agg.sum), file="../Data/spider.sp.compo.txt",sep="\t",quote=F,row.name=F,col.name=T)
#saveRDS(agg.sum,file="spider.sp.compo")
