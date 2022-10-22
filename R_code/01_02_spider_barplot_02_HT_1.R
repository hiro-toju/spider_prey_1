######################
# 2020.11.26.
# fujita edit
# 2020.12.09
# Suzuki edit
# 2021.08.26
# Suzuki edit
# spider composition barplot
######################


library(tidyr)
library(ggplot2)
library(ggsci)
library(scales)
library(RColorBrewer)

level<- "Species" #Order, Genusなど
## ==================================================================== ##

sample.data <- t(read.table("../Data/spider.sp.compo_dot.txt", header=T, row.names=1)　)

#色を原色▶パステル色の組み合わせに変更（ポスター用)
col.vector <- c(brewer.pal(8,"Set1"),
                brewer.pal(7,"Pastel1"),
				brewer.pal(10,"Paired"),
				brewer.pal(12,"Set3")[-9],
				brewer.pal(7,"Set2"),
				brewer.pal(5,"Pastel2"),
				brewer.pal(9,"Reds"),
				brewer.pal(6,"PuBu")
	 	      )　
	
			 	      
	 	      
# length check
length(col.vector)

unidendified <- which(colnames(sample.data)=="predator_unidentified")

###
#dominant <- sample.data[, -unidendified]
dominant <- sample.data



dominant2 <- dominant[,order(colSums(dominant), decreasing=T) ]
# error:添え字が許される範囲外です 
#other <- rowSums(dominant2[,(length(col.vector)+1):nrow(dominant2)])
other <- rowSums(dominant2[,(length(col.vector)):nrow(dominant2)])


dominant3 <- as.data.frame(cbind(sample=rownames(dominant2), dominant2[,1:length(col.vector)],unidentified =sample.data[, unidendified]))

# ggplot にプロットできるようにデータを変形させる				
lf <- gather(dominant3, key, value, -1)

lf$value <- as.numeric(as.vector(lf$value))

pal <- unique(lf$key)

#col <- c(col.vector[1:(length(pal)-1)],"grey30" )
col <- rev(c(col.vector[1:(length(pal))] ))
# , "grey90")
#names(col) <- c(pal[-which(pal%in%c("unidentified"))], c("unidentified"))

pal2 <- cbind(pal, col.vector[1:(length(pal))])

lf$key <- factor(lf$key, level=c( c("unidentified","other"),colnames(dominant2[,1:length(col.vector)])[order(colSums(dominant2[,1:length(col.vector)]))]))
lf$sample <- factor(lf$sample, level=c("X4","X5","X6","X7","X8","X9","X10","X11")) 


### percent barplot ###

g <- ggplot(lf, aes(x = sample, y = value, fill = key))　+
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col)+
	  guides(fill=guide_legend(#reverse=T, 
	  			 			   ncol=2))
g <- g + theme_minimal() + scale_y_continuous(expand=c(0,0))+scale_y_continuous(labels = percent)
g <- g + theme(text = element_text(size = 25))

g <- g + xlab(NULL)
g <- g + ylab(NULL)

pdf(sprintf("../Output/%s_barplot.per.pdf", level), w=20, h=12) 
plot(g)
dev.off()

### percent barplot ###

### number stacking barplot ###

h <- ggplot(lf, aes(x = sample, y = value, fill = key))　+
	 geom_bar(stat = "identity")+
	  scale_fill_manual(values=col)+
	  guides(fill=guide_legend(#reverse=T, 
	  			 			   ncol=2))
    			   	  			 			   
h <- h + theme_minimal() + scale_y_continuous(expand=c(0,0))


pdf(sprintf("../Output/%s_barplot.number.pdf", level), w=10, h=10) 

plot(h)
dev.off()

### number stacking barplot ###
