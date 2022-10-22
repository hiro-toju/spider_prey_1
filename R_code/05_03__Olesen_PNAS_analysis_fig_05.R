#########################
# 2022.05.26
# Suzuki writing

# Olsen module analysis
# plot data
#########################
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(ggrepel)


# Merging Individual Numbers Data
prey.indi <- as.data.frame(read.table('../Data/prey.detect.all.month.agg.txt',header=T,sep ='\t')  )
spider.indi <- as.data.frame (read.table('../Data/spider.indi.all.month.agg.txt',header=T,sep ='\t') )

prey.indi <- cbind(prey.indi, class= 'prey')
head(prey.indi)
spider.indi <-  cbind(spider.indi, class= 'spider')
head(spider.indi)

indi.all <- rbind(prey.indi,spider.indi)
head(indi.all)

data <- as.data.frame(read.table('../Data/forplot.data.txt',header=T,sep ='\t') )
head(data)
dim(data)
join.data <- left_join(data, indi.all,by = c('node_name'= 'species'))
head(join.data)
write.table(cbind(rownames(join.data), join.data),file= '../Output/Olesen/forplot.data_02.txt' ,sep="\t",quote=F,row.name=F,col.name=T)


library(ggplot2)


#  not label 
pdf('../Output/Olesen/network_role_ggplot_03.pdf',h = 6 ,w = 8)
g <- ggplot(join.data) +
 aes(x = c.score, y = degree.z, shape = class,fill= class ) +
    geom_hline(aes(yintercept= 2.5,colour = 'grey50'))+
  geom_vline(aes(xintercept= 0.6,colour = 'grey50')) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = (total.indi)^0.5 ) , alpha=0.8) +
 #scale_color_hue(direction = 1) +
 labs(labs(x = "Among-module connectivity", y = "Within-module degree")) +
 theme_bw() +
theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
  scale_size_area(max_size=12,breaks = c(1,5, 10,20) ) +
  scale_fill_manual(values = c('indianred3','cadetblue'))+
  scale_colour_manual(values = c('grey28','grey28')) 
    #scale_size_continuous() ) 
plot(g) 

dev.off()

#  label　あり 
pdf('../Output/Olesen/network_role_ggplot_label_03.pdf',h = 9 ,w = 10)
g <- ggplot(join.data) +
 aes(x = c.score, y = degree.z, shape = class,fill= class ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = (total.indi)^0.5  ) , alpha=0.8) +
 #scale_color_hue(direction = 1) +
 labs(x = "Among-module connectivity,c", y = "Within-module degree,z", 
 title = "Network role") +
 theme_bw() +
theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
  scale_size_area(max_size=12,breaks =c(1,5, 10,30,50,100,150,300) ) +
  scale_fill_manual(values = c('indianred3','cadetblue'))+
  scale_colour_manual(values = c('grey28','grey28'))+
  geom_label_repel(aes(label = node_name))
  #scale_size_continuous() ) 
  
  
plot(g)

dev.off()
