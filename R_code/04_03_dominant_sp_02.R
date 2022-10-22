library(ggplot2)
library(palmerpenguins)
library(cowplot)
library(RColorBrewer)
#library(tidyr)

month.uni <- c(4:11)


glist_bet <- list()
glist_deg <- list()

datam <- c()
for (u in month.uni ) { 


data <-  as.data.frame(read.table(sprintf('../Data/eachmonth_dprime_table.%s.txt',u),header=T, sep ='\t',row.names=1))
head(data)

#datam <- rbind(datam, cbind(month=paste("",u), data))
datam <- rbind(datam, cbind(month=paste(u), data))
head(datam)
}

datam$month <- factor(datam$month ,level= c('4','5','6','7','8','9','10','11'))

# 'Oxyopes_sertatus'を含むものを抽出する
Ox <- rownames(datam) %in% grep('Oxyopes_sertatus',rownames(datam), value = TRUE)
ox.sub <- subset(datam,Ox)
Xy <- rownames(datam) %in% grep('Xysticus_sp.',rownames(datam), value = TRUE)
Xy.sub <- subset(datam, Xy)

eb <- rownames(datam) %in% grep('Ebrechtella_tricuspidata',rownames(datam), value = TRUE)
eb.sub <- subset(datam, eb)

# guides(colour = guide_legend(override.aes = list(size=10))) →凡例のアイコンの大きさを変える
#　ササグモ
pdf('../Output/dprime_hosei_betweenness_Oxyopes_sertatus.pdf',w=6,h=5)
g <- ggplot(ox.sub) +
 aes(x = dprime.hosei, y = betweenness , shape = class,colour = month,fill= month) +
  scale_shape_manual(values = c(23)) +
 geom_point(aes(  size = individuals ) , alpha=1.0) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("Oxyopes_sertatus_1-dprime x centrality")) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 6),axis.text.y = element_text(size = 6),
 legend.text = element_text(size = 6))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=12,breaks = c(1,5, 10,20,30,60,120) ) +
  scale_fill_manual(values = c(brewer.pal(8,'Blues')))+
  scale_colour_manual(values = c(brewer.pal(8,'Blues')))+
    guides(colour = guide_legend(override.aes = list(size=8)))
  #scale_size_continuous() ) 
plot(g)

dev.off()  　

pdf('../Output/dprime_hosei_betweenness_Oxyopes_sertatus_2.pdf',w=6,h=5)
g <- ggplot(ox.sub) +
 aes(x = dprime.hosei, y = betweenness , shape = class,fill= month) +
  scale_shape_manual(values = c(23)) +
 geom_point(aes(  size = individuals ) , alpha=1.0) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("Oxyopes_sertatus_1-dprime x centrality")) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 6), axis.title.x = element_text(size = 6)
 ,axis.text.x = element_text(size = 6),axis.text.y = element_text(size = 6),
 legend.text = element_text(size = 6))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=12,breaks = c(1,5, 10,20,30,60,120) ) +
  scale_fill_manual(values = c(brewer.pal(8,'Blues')))+
  scale_colour_manual(values = c('grey28','grey28'))+
    guides(colour = guide_legend(override.aes = list(size=8)))

  #scale_size_continuous() ) 
plot(g)

dev.off()  　
#　カニグモ属
pdf('../Output/dprime_hosei_betweenness_Xysticus_sp.pdf',w=6,h=5)
g <- ggplot(Xy.sub) +
 aes(x = dprime.hosei, y = betweenness , shape = class,fill= month) +
  scale_shape_manual(values = c(23)) +
 geom_point(aes(  size = individuals ) , alpha=1.0) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("Xysticus_sp._1-dprime x centrality")) +
 theme_bw() +
 theme(plot.title = element_text(size = 6, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 6), axis.title.x = element_text(size = 6)
 ,axis.text.x = element_text(size = 6),axis.text.y = element_text(size =6),
 legend.text = element_text(size = 6))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=12,breaks = c(1,5, 10,20,30,60,120) ) +
  scale_fill_manual(values = c(brewer.pal(8,'Blues')))+
  scale_colour_manual(values = c('grey28','grey28'))+
    guides(colour = guide_legend(override.aes = list(size=8)))
plot(g)

dev.off() 

#　ハナグモ
pdf('../Output/dprime_hosei_betweenness_ Ebrechtella_tricuspidata.pdf',w=6,h=5)
g <- ggplot(eb.sub) +
 aes(x = dprime.hosei, y = betweenness , shape = class,fill= month) +
  scale_shape_manual(values = c(23)) +
 geom_point(aes(  size = individuals ) , alpha=1.0) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("Ebrechtella_tricuspidata_1-dprime x centrality")) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 6), axis.title.x = element_text(size = 6)
 ,axis.text.x = element_text(size = 6),axis.text.y = element_text(size = 6),
 legend.text = element_text(size = 6))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=12,breaks = c(1,5, 10,20,30,60,120) ) +
  scale_fill_manual(values = c(brewer.pal(8,'Blues')))+
  scale_colour_manual(values = c('grey28','grey28'))+
    guides(colour = guide_legend(override.aes = list(size=8)))
plot(g)

dev.off() 

