
# 2021.11.16 writing
# 
# 2022.02.18 edit

library(tidyr)
library(ggplot2)
library(ggsci)
library(scales)
library(RColorBrewer)
library(tidyverse)

##########################
###############
# all month data
###############
##########################	

# input data
sample.data <-t(as.data.frame(read.table('../Data/month.diet.result.txt',header=T,sep ='\t',row.names=1)  ))

rownames(sample.data)   <- as.character(4:11)
colnames(sample.data)

dominant3 <- as.data.frame(cbind(month=rownames(sample.data), sample.data))


# ggplot にプロットできるようにデータを変形させる				
lf  <- dominant3 %>%
  pivot_longer(col = -month, names_to = "diet", values_to = "amount")
   

lf$amount <- as.numeric(as.vector(lf$amount))


# 順番を指定したいので、手動で入力している
#pal <- unique(lf$order)
col.vector <- rev(c('darkseagreen3','lightgoldenrod4','chocolate2','grey28'))

pal <- rev(c("herbivorous", "predatory/parasitic" ,"detritivorous","variable"))


pal2 <- cbind(pal, col.vector[1:(length(pal))])

#lf$diet <- factor(lf$diet,colnames(sample.data[,1:length(col.vector)])[order(colSums(sample.data[,1:length(col.vector)]))])

lf$diet <- factor(lf$diet, level=rev(c("herbivorous", "predatory/parasitic" ,"detritivorous","variable"))  ) 

lf$month <- factor(lf$month, level=as.character(c(4:11) ))


### percent barplot ###  
    	  		 	
g <- ggplot(lf, aes(x = month, y =amount, fill =diet))　+
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col.vector)+ 
	  #guides(fill=guide_legend( ncol=1)) +
	   guides(fill= F) +
	   labs(x = "month", title = 'all.month.data') +
	  scale_y_continuous(labels = percent,expand=c(0,0)
	   ) 
    				   

g <- g + theme(text = element_text(size = 40))

g <- g + xlab(NULL)
g <- g + ylab(NULL)

 #ここにfacet_wrap()でlfデータの列内容に合わせてソートできる。


pdf("../Output/all.month_barplot.per.pdf", w=10, h=8)

plot(g)
dev.off()
### percent barplot ###


### number stacking barplot ###
# error ???
############


# FIX #####
h <- ggplot(lf, aes(x = month, y =amount, fill =diet))　+
	 geom_bar(stat = "identity")+
	  scale_fill_manual(values=col.vector)+
	  #theme_minimal() + 
	  scale_y_continuous(expand=c(0,0))+
      labs(x = "month", title = 'all.month.data' ) +
	  #guides(fill=guide_legend(#reverse=T,ncol=1))
	  guides(fill= F) 

h <- h + theme(text = element_text(size = 40))
h <- h + xlab(NULL)
h <- h + ylab(NULL)

pdf("../Output/all.month_barplot.number.pdf", w=10, h=8) #"%s以下に文字を入れると名前を付けられます
plot(h)
dev.off()

### number stacking barplot ###
#}

##########################
###############
# herbivorous
###############
##########################
level = 

sample.data <-t(as.data.frame(read.table('../Data/month.diet.result.HERB.txt',header=T,sep ='\t',row.names=1)  ))
dim(sample.data)


length(colnames(sample.data))
# カラーを選択。似た色は[-x]で除くことができる				
#色を原色▶パステル色の組み合わせに変更（ポスター用)
col.vector <- c(brewer.pal(7,"Accent"),
                brewer.pal(7,"Dark2"),
				brewer.pal(7,"Pastel2"),
				brewer.pal(7,"Pastel1"),
				brewer.pal(8,"Set2"),
				brewer.pal(8,"Set1"),
				brewer.pal(8,"Set3"))　
# length check
length(col.vector)

unidendified <- which(colnames(sample.data)=="predator_unidentified")

###
#dominant <- sample.data[, -unidendified]
dominant <- sample.data
dominant2 <- dominant[,order(colSums(dominant), decreasing=T) ]
# error:添え字が許される範囲外です 
#other <- rowSums(dominant2[,(length(col.vector)+1):nrow(dominant2)])
#other <- rowSums(dominant2[,(length(col.vector)):nrow(dominant2)])

dominant3 <- as.data.frame(cbind(sample=rownames(dominant2), dominant2[,1:length(colnames(sample.data))]))

# ggplot にプロットできるようにデータを変形させる				
lf <- gather(dominant3, key, value, -1)

lf$value <- as.numeric(as.vector(lf$value))

pal <- unique(lf$key)

#col <- c(col.vector[1:(length(pal)-1)],"grey30" )
col <- rev(c(col.vector[1:(length(pal))] ))
# , "grey90")
#names(col) <- c(pal[-which(pal%in%c("unidentified"))], c("unidentified"))

pal2 <- cbind(pal, col.vector[1:(length(pal))])

lf$key <- factor(lf$key, level=c( colnames(dominant2[,1:length(colnames(sample.data))])[order(colSums(dominant2[,1:length(colnames(sample.data))]))]))

lf$sample <- factor(lf$sample, level=c("X4","X5","X6","X7","X8","X9","X10","X11")) 


### percent barplot ###

g <- ggplot(lf, aes(x = sample, y = value, fill = key))　+
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col)+
	  guides(fill=guide_legend(#reverse=T, 
	  			 			   ncol=2))
g <- g + theme_minimal() + scale_y_continuous(expand=c(0,0))+scale_y_continuous(labels = percent) 

pdf("../Output/herb_barplot.per.pdf",w=10, h=6) #"%s以下に文字を入れると名前を付けられます
plot(g)
dev.off()
### number stacking barplot ###

h <- ggplot(lf, aes(x = sample, y = value, fill = key))　+
	 geom_bar(stat = "identity")+
	  scale_fill_manual(values=col)+
	  guides(fill=guide_legend(#reverse=T, 
	  			 			   ncol=2))
    			   
	  			 			   
h <- h + theme_minimal() + scale_y_continuous(expand=c(0,0))
 #ここにfacet_wrap()でlfデータの列内容に合わせてソートできる。


pdf("../Output/herb_barplot.number.pdf", w=10, h=6) #"%s以下に文字を入れると名前を付けられます
plot(h)
dev.off()



##########################
###############
# detrivorous
###############
##########################

sample.data <-t(as.data.frame(read.table('../Data/month.diet.result.det.txt',header=T,sep ='\t',row.names=1)  ))
rownames(sample.data) <- as.character(c(04,05,06,07,08,09,10,11))

unidendified <- which(colnames(sample.data)=="predator_unidentified")

###
#dominant <- sample.data[, -unidendified]
dominant <- sample.data
dominant2 <- dominant[,order(colSums(dominant), decreasing=T) ]
# error:添え字が許される範囲外です 
#other <- rowSums(dominant2[,(length(col.vector)+1):nrow(dominant2)])
#other <- rowSums(dominant2[,(length(col.vector)):nrow(dominant2)])

dominant3 <- as.data.frame(cbind(sample=rownames(dominant2), dominant2[,1:length(colnames(sample.data))]))

# ggplot にプロットできるようにデータを変形させる				
lf <- gather(dominant3, key, value, -1)

lf$value <- as.numeric(as.vector(lf$value))

pal <- unique(lf$key)

#col <- c(col.vector[1:(length(pal)-1)],"grey30" )
col <- rev(c(col.vector[1:(length(pal))] ))
# , "grey90")
#names(col) <- c(pal[-which(pal%in%c("unidentified"))], c("unidentified"))

pal2 <- cbind(pal, col.vector[1:(length(pal))])

lf$key <- factor(lf$key, level=c( colnames(dominant2[,1:length(colnames(sample.data))])[order(colSums(dominant2[,1:length(colnames(sample.data))]))]))

lf$sample <- factor(lf$sample, level=as.character(c(04,05,06,07,08,09,10,11)))



### percent barplot ###

g <- ggplot(lf, aes(x = sample, y = value, fill = key))　+
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col)+ 
	  #guides(fill=guide_legend( ncol=1)) +
	   guides(fill= F) +
	   labs(x = "month", title = 'all.month.data') +
	  scale_y_continuous(labels = percent,expand=c(0,0)
	   )
g <- g + theme(text = element_text(size = 52))
	   
 g <- g + xlab(NULL)
g <- g + ylab(NULL)

pdf("../Output/det_barplot.per.pdf", w=12, h=10) #"%s以下に文字を入れると名前を付けられます
plot(g)
dev.off()


### number stacking barplot ###

g <- ggplot(lf, aes(x = sample, y = value, fill = key))　+
	 geom_bar(stat = "identity" )+
	  scale_fill_manual(values=col)+ 
	  #guides(fill=guide_legend( ncol=1)) +
	   guides(fill= F) +
	   labs(x = "month", title = 'all.month.data') +
	  scale_y_continuous(expand=c(0,0)
	   )
g <- g + theme(text = element_text(size = 52))
	   
 g <- g + xlab(NULL)
g <- g + ylab(NULL)

 #ここにfacet_wrap()でlfデータの列内容に合わせてソートできる。


pdf("../Output/det_barplot.number.pdf",w=12, h=10) #"%s以下に文字を入れると名前を付けられます
plot(g )
dev.off()

##########################
###############
# predatory/parasitic
###############
##########################


sample.data <-t(as.data.frame(read.table('../Data/month.diet.result.predatory.txt',header=T,sep ='\t',row.names=1)  ))
rownames(sample.data) <- as.character(c(04,05,06,07,08,09,10,11))

unidendified <- which(colnames(sample.data)=="predator_unidentified")

###
#dominant <- sample.data[, -unidendified]
dominant <- sample.data
dominant2 <- dominant[,order(colSums(dominant), decreasing=T) ]
# error:添え字が許される範囲外です 
#other <- rowSums(dominant2[,(length(col.vector)+1):nrow(dominant2)])
#other <- rowSums(dominant2[,(length(col.vector)):nrow(dominant2)])

dominant3 <- as.data.frame(cbind(sample=rownames(dominant2), dominant2[,1:length(colnames(sample.data))]))

# ggplot にプロットできるようにデータを変形させる				
lf <- gather(dominant3, key, value, -1)

lf$value <- as.numeric(as.vector(lf$value))

pal <- unique(lf$key)

#col <- c(col.vector[1:(length(pal)-1)],"grey30" )
col <- rev(c(col.vector[1:(length(pal))] ))
# , "grey90")
#names(col) <- c(pal[-which(pal%in%c("unidentified"))], c("unidentified"))

pal2 <- cbind(pal, col.vector[1:(length(pal))])

lf$key <- factor(lf$key, level=c( colnames(dominant2[,1:length(colnames(sample.data))])[order(colSums(dominant2[,1:length(colnames(sample.data))]))]))

lf$sample <- factor(lf$sample, level=as.character(c(04,05,06,07,08,09,10,11)))

### percent barplot ###

g <- ggplot(lf, aes(x = sample, y = value, fill = key))　+
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col)+ 
	  #guides(fill=guide_legend( ncol=1)) +
	   guides(fill= F) +
	   labs(x = "month", title = 'all.month.data') +
	  scale_y_continuous(labels = percent,expand=c(0,0)
	   )
g <- g + theme(text = element_text(size = 12))
	   
 g <- g + xlab(NULL)
g <- g + ylab(NULL)

   		


pdf("../Output/predatory_barplot.per.pdf", w=12, h=10) #"%s以下に文字を入れると名前を付けられます
plot(g)
dev.off()


### number stacking barplot ###

g <- ggplot(lf, aes(x = sample, y = value, fill = key))　+
	 geom_bar(stat = "identity" )+
	  scale_fill_manual(values=col)+ 
	  guides(fill=guide_legend( ncol=1)) +
	  # guides(fill= F) +
	   labs(x = "month", title = 'all.month.data') +
	  scale_y_continuous(expand=c(0,0)
	   )
g <- g + theme(text = element_text(size = 12))
	   
 g <- g + xlab(NULL)
g <- g + ylab(NULL)

 #ここにfacet_wrap()でlfデータの列内容に合わせてソートできる。


pdf("../Output/predatory_barplot.number.pdf",w=12, h=10) #"%s以下に文字を入れると名前を付けられます
plot(g )
dev.off()



##########################
#日本語の凡例をつくる
##########################



diet.data <- read.table('../Data/Family.Food_2_Toju.txt',header=T,sep ='\t')
xx <- t(sample.data)
xx
xx <- cbind(rownames(xx),xx)
colnames(xx) <- c('family',as.character(c(4:11)))
xx
mer <- merge(diet.data,xx,  by.x ='family', by.y = 'family')
mer.japan <- mer[ , c(-1,-3,-4)]


write.table(mer.japan,file= '../Data/month.det.nihongo.txt',sep='\t',quote=F,row.names=F)
nihongo.mat <- read.table('month.det.nihongo.txt', header=T,sep ='\t',row.name=1)
colnames(nihongo.mat) <- as.character(c(4:11) )

sample.data <- t(nihongo.mat )



unidendified <- which(colnames(sample.data)=="predator_unidentified")

###
#dominant <- sample.data[, -unidendified]
dominant <- sample.data
dominant2 <- dominant[,order(colSums(dominant), decreasing=T) ]
# error:添え字が許される範囲外です 
#other <- rowSums(dominant2[,(length(col.vector)+1):nrow(dominant2)])
#other <- rowSums(dominant2[,(length(col.vector)):nrow(dominant2)])

dominant3 <- as.data.frame(cbind(sample=rownames(dominant2), dominant2[,1:length(colnames(sample.data))]))

# ggplot にプロットできるようにデータを変形させる				
lf <- gather(dominant3, key, value, -1)

lf$value <- as.numeric(as.vector(lf$value))

pal <- unique(lf$key)

#col <- c(col.vector[1:(length(pal)-1)],"grey30" )
col <- rev(c(col.vector[1:(length(pal))] ))
#col <- c(col.vector[1:(length(pal))] )
# , "grey90")
#names(col) <- c(pal[-which(pal%in%c("unidentified"))], c("unidentified"))

pal2 <- cbind(pal, col.vector[1:(length(pal))])

lf$key <- factor(lf$key, level=c( colnames(dominant2[,1:length(colnames(sample.data))])[order(colSums(dominant2[,1:length(colnames(sample.data))]))]))

lf$sample <- factor(lf$sample, level= as.character(4:11)  )
lf

g <- ggplot(lf, aes(x = sample, y = value, fill = key, family = "HiraKakuPro-W3" ))　+
theme_gray (base_family = "HiraKakuPro-W3") +
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col)+ 
	  guides(fill=guide_legend( ncol=1)) +
	   #guides(fill= F) +
	   labs(x = "month", title = 'all.month.data') +
	  scale_y_continuous(labels = percent,expand=c(0,0)
	   )
g <- g + theme(text = element_text(size = 17))
	   
 g <- g + xlab(NULL)
g <- g + ylab(NULL)
g


pdf(sprintf("../Output/nihongo.pdf"), w=18, h=12) #"%s以下に文字を入れると名前を付けられます
plot(g)
dev.off()

##########################
#日本語の凡例をつくる
##########################  





