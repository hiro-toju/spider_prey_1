#########################
# 2022.06.16
# Suzuki writing
# each module compositon
#########################


library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)

month.uni <- as.character(c(4:11))

all.month.module <- c()

for (i in month.uni){

month.d <- read.table(sprintf('../Data/Infomap/module.matrix.%s.txt',i),header=T,sep ='\t',row.names=1)	
modu.uni <- sort(unique(month.d$module))
diet.uni <- unique(month.d$diet.set)

month.agg <- as.data.frame(t(read.table(sprintf('../Data/prey.id.prey.matrix.agg.%s.txt',i),header=T,sep ='\t',row.names=1)	))

month.agg_2 <- cbind(rownames(month.agg), month.agg)
	
# make module matrix
diet.matrix <- matrix(NA,nrow =5, ncol = length(modu.uni))
rownames(diet.matrix) <- c("herbivorous", "predatory/parasitic" ,"detritivorous","variable",'unidentified')
colnames(diet.matrix) <- c(modu.uni)

modu.group <- split(month.d, month.d$module)

for ( m in modu.uni) {

detect.mer <- as.data.frame(inner_join(modu.group[[m]], month.agg_2 ,by = c("sp_name" = "rownames(month.agg)") ))
ncol <- length(colnames(detect.mer))
detect.mer.sum <- rowSums(detect.mer[,5:ncol]) 

bind.data <-  as.data.frame( cbind(detect.mer$diet.set , detect.mer.sum )) 



for(k in unique(rownames(diet.matrix))  ){
sub <- 	subset(bind.data, bind.data$V1 ==  k)
	diet.matrix[k, m] <- sum( as.numeric(sub[,2]) )
	 }
diet.matrix[is.na(diet.matrix)] <- 0
}


write.table(cbind(rownames(diet.matrix), diet.matrix),sprintf('../Output/Infomap/module.diet.matrix.weight.%s.txt',i),sep='\t',row.names=F)		
	
data.set_01 <- as.data.frame(cbind(month=sprintf('%s',i), t(diet.matrix)))
data.set_02 <- cbind(module= c(colnames(diet.matrix)),data.set_01)

#　全ての月のモジュールの食性構成をまとめたもの
all.month.module <- rbind(all.month.module , data.set_02 )

}


#####################
# Output
#####################

col.vector <- rev(c('darkseagreen3','lightgoldenrod4','chocolate2','grey28','grey50'))

all.month.module_2 <- all.month.module[, colnames(all.month.module)!= "spider"  ]

lf  <- all.month.module_2 %>%
  pivot_longer(col = c(-month,-module), names_to = "diet", values_to = "amount")
lf$amount <- as.numeric(as.vector(lf$amount))  

# factor のlevels で並び替える
lf$module<- factor(lf$module, level=c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21')) 
lf$diet <- factor(lf$diet, level=rev(c("herbivorous", "predatory/parasitic" ,"detritivorous","variable",'unidentified'))  ) 
lf$month <- factor( lf$month, level= c('4','5','6','7','8','9','10','11'))

### percent barplot ###  
    	
    	
    	
pdf("../Output/Infomap/module.barplot.diet_01.pdf", w=28, h=20)    	  		 	
g <- ggplot(lf, aes(x = module, y = amount, fill =diet))　+
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col.vector)+ 
	  #guides(fill=guide_legend( ncol=1)) +
	  # guides(fill= F) +
	   labs(x = "month", title = 'all.month.data') +
	  scale_y_continuous(expand=c(0,0)) +
	    facet_wrap(~month, ncol=2) 
    				   
g <- g + theme(text = element_text(size = 30))
plot(g)
dev.off()

pdf("../Output/Infomap/module.barplot.diet_02.pdf", w=10, h=18)    	  		 	
g <- ggplot(lf, aes(x = module, y = amount, fill =diet))　+
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col.vector)+ 
	  #guides(fill=guide_legend( ncol=1)) +
	  # guides(fill= F) +
	   labs(x = "month", title = 'all.month.data') +
	  scale_y_continuous(expand=c(0,0)) +
	    facet_wrap(~month, ncol=1) 
    				   
g <- g + theme(text = element_text(size = 20))
plot(g)
dev.off()



#ggplot(month.d, aes(x= module, y=diet.set,fill= diet.set )) +geom_col(position= 'fill')

	
	








		
