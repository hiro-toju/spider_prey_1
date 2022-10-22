

library(tidyr)
library(ggplot2)
library(ggsci)
library(scales)
library(RColorBrewer)
library(tidyverse)


#hunt.type <- read.table('spider_ecology_20220301_2.txt',header=T,sep ='\t',row.names=1)
# [1] "unidentified"   "sit-and-wait"   "web-weabing"    "active-hunting" "Attack"    [6] "freeloader"  
#hunt.uni <- unique(hunt.type$hunting_mode)

dominant.spider.sp <- c('Argiope_bruennichi','Tetragnatha_caudicula','Leucauge_blanda','Ebrechtella_tricuspidata','Xysticus_sp.','Larinia_argiopiformis','Neoscona_scylloides','Tetragnatha_praedonia','Nephila_clavata','Mendoza_elongata','Araneidae_sp.1','Neoscona_mellotteei')


####################################
# each species
####################################

for (u in dominant.spider.sp) {	
	

# input data
sample.data <-t(as.data.frame(read.table(sprintf('../Data/diet.compo.%s.txt',u),header=T,sep ='\t',row.names=1)  ))

rownames(sample.data) <- as.character(4:11)
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

lf$month <- factor(lf$month, level= as.character(4:11)
) 


### percent barplot ###
g <- ggplot(lf, aes(x = month, y =amount, fill =diet))　+
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col.vector)+ 
	  #guides(fill=guide_legend( ncol=1)) +
	   guides(fill= F) +
	   labs(x = "month", title = sprintf(u) ) +
	  scale_y_continuous(labels = percent,expand=c(0,0)
	   ) 
    				  			 			   

g <- g + theme(text = element_text(size = 50))

g <- g + xlab(NULL)
g <- g + ylab(NULL)

 #ここにfacet_wrap()でlfデータの列内容に合わせてソートできる。

pdf(sprintf("../Output/%s_barplot.per.pdf",u), w=12, h=8)

plot(g)
dev.off()
### percent barplot ###

### number stacking barplot ###

h <- ggplot(lf, aes(x = month, y =amount, fill = diet))　+
	 geom_bar(stat = "identity")+
	  scale_fill_manual(values=col.vector)+
	  #theme_minimal() + 
	  scale_y_continuous(limits = c(0,200),expand=c(0,0))+
  labs(x = "month", title = sprintf(u) ) +
	 # guides(fill=guide_legend(#reverse=T,
	 # ncol=1)
	    guides(fill= F) 
	   
	   	
h <- h + theme(text = element_text(size = 50))
h <- h + xlab(NULL)
h <- h + ylab(NULL)

pdf(sprintf("../Output/%s_barplot.number.pdf",u),w=10, h=8) #"%s以下に文字を入れると名前を付けられます
plot(h)
dev.off()

### number stacking barplot ###
}
####################################
# each species end
####################################

####################################
# each species sasagumo
####################################

for (u in 'Oxyopes_sertatus') {	
	


# input data
sample.data <-t(as.data.frame(read.table(sprintf('../Data/diet.compo.%s.txt',u),header=T,sep ='\t',row.names=1)  ))

rownames(sample.data) <- as.character(4:11)
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

lf$month <- factor(lf$month, level= as.character(4:11)
) 


### percent barplot ###
g <- ggplot(lf, aes(x = month, y =amount, fill =diet))　+
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col.vector)+ 
	  #guides(fill=guide_legend( ncol=1)) +
	   guides(fill= F) +
	   labs(x = "month", title = sprintf(u) ) +
	  scale_y_continuous(labels = percent,expand=c(0,0)
	   ) 
    				  			 			   

g <- g + theme(text = element_text(size = 50))

g <- g + xlab(NULL)
g <- g + ylab(NULL)


pdf(sprintf("../Output/%s_barplot.per.pdf",u), w=12, h=8)

plot(g)
dev.off()
### percent barplot ###

### number stacking barplot ###

h <- ggplot(lf, aes(x = month, y =amount, fill = diet))　+
	 geom_bar(stat = "identity")+
	  scale_fill_manual(values=col.vector)+
	  #theme_minimal() + 
	  scale_y_continuous(limits = c(0,500),expand=c(0,0))+
  labs(x = "month", title = sprintf(u) ) +
	 # guides(fill=guide_legend(#reverse=T,
	 # ncol=1)
	    guides(fill= F) 
	   
	   	
h <- h + theme(text = element_text(size = 50))
h <- h + xlab(NULL)
h <- h + ylab(NULL)

pdf(sprintf("../Output/%s_barplot.number.pdf",u),w=10, h=8) #"%s以下に文字を入れると名前を付けられます
plot(h)
dev.off()
}

####################################
# hunt mode
####################################

hunt.type <- read.table('../Data/spider_ecology_20220301.txt',header=T,sep ='\t',row.names=1)
hunt.uni <- unique(hunt.type$hunting_mode)

for (u in hunt.uni) {	
# input data
sample.data <-t(as.data.frame(read.table(sprintf('../Data/diet.compo.%s.txt',u),header=T,sep ='\t',row.names=1)  ))

rownames(sample.data)
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

lf$month <- factor(lf$month, level=as.character(4:11) )

### percent barplot ###
g <- ggplot(lf, aes(x = month, y =amount, fill =diet))　+
	 geom_bar(stat = "identity", position='fill')+
	  scale_fill_manual(values=col.vector)+ 
	  guides(fill=guide_legend( ncol=1)) +
	   labs(x = "month", title = sprintf(u) ) +
	  scale_y_continuous(labels = percent,expand=c(0,0)
	   ) 
    				  			 			   

g <- g + theme(text = element_text(size = 20))
g <- g + xlab(NULL)
g <- g + ylab(NULL)



pdf(sprintf("../Output/%s_barplot.per.pdf",u), w=10, h=6)

plot(g)
dev.off()
### percent barplot ###

### number stacking barplot ###

h <- ggplot(lf, aes(x = month, y =amount, fill = diet))　+
	 geom_bar(stat = "identity")+
	  scale_fill_manual(values=col.vector)+
	  #theme_minimal() +
	  guides(fill=guide_legend(#reverse=T,
	   ncol=1) +
	    labs(x = "month", title = sprintf(u) ) +
	   scale_y_continuous(expand=c(0,0),limits = c(0,650)) 
	   )
	
h <- h + theme(text = element_text(size = 20))
h <- h + xlab(NULL)
h <- h + ylab(NULL)

pdf(sprintf("../Output/%s_barplot.number.pdf",u), w=10, h=6) #"%s以下に文字を入れると名前を付けられます
plot(h)
dev.off()
### number stacking barplot ###
}
####################################
# hunt mode end
####################################

 warnings() 