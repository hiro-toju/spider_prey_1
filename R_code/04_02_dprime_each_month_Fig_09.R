#########################
# 2022.05.26
# Suzuki writing
#　

# Olsen module analysis
# plot data
#########################

#昆虫は生食、腐食で色分けする、色も生食腐食に対応させる？

library(ggplot2)
library(palmerpenguins)
library(cowplot)
library(RColorBrewer)
library(ggrepel)

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



####################
# Output 
#####################

#######################
#　alldata
#######################
# standard
#######################
pdf('../Output/dprime_hosei_betweenness_all.data.pdf')
g <- ggplot(datam) +
 aes(x = dprime.hosei, y = betweenness , shape = class,fill= class ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = individuals ) , alpha=0.8) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("1-dprime x centrality alldata")) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=12,breaks = c(1,5, 10,20,30,60) ) +
  scale_fill_manual(values = c('indianred3','cadetblue'))+
  scale_colour_manual(values = c('grey28','grey28'))
  #scale_size_continuous() ) 
plot(g)

dev.off()  　
#######################
#　fill each month
#######################
pdf('../Output/dprime_hosei_betweenness_all.data_2.pdf')
g <- ggplot(datam) +
 aes(x = dprime.hosei, y = betweenness , shape = class, fill=month ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = individuals ) , alpha=0.8) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("1-dprime x centrality alldata")) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=12,breaks = c(1,5, 10,20,30,60) ) +
  scale_fill_manual(values = c(brewer.pal(8,'Blues')))+
  scale_colour_manual(values = c('grey28','grey28'))
  #scale_size_continuous() ) 
plot(g)

dev.off()  　

#-----------------------------------#
# 2 x 4 layout
#-----------------------------------#

# dprime x betweenness
pdf('../Output/dprime_hosei_betweenness_each.month.pdf', w= 9, h= 15)
g <- ggplot(datam) +
 aes(x = dprime.hosei, y = betweenness , shape = class,fill= class ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = individuals ) , alpha=0.8) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("1-dprime x centrality")) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=10,breaks = c(1,5, 10,20,30,60) ) +
  scale_fill_manual(values = c('indianred3','cadetblue'))+
  scale_colour_manual(values = c('grey28','grey28'))+
  facet_wrap(~month, ncol=2)
  
  #scale_size_continuous() ) 
plot(g)
dev.off()
#-----------------------------------#
#label 
# dprime x betweenness
pdf('../Output/dprime_hosei_betweenness_each.month.label.pdf', w= 13, h= 23)
g <- ggplot(datam) +
 aes(x = dprime.hosei, y = betweenness , shape = class,fill= class ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = individuals ) , alpha=0.8) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("1-dprime x centrality")) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=10,breaks = c(1,5, 10,20,30,60) ) +
  scale_fill_manual(values = c('indianred3','cadetblue'))+
  scale_colour_manual(values = c('grey28','grey28'))+
  facet_wrap(~month, ncol=2)+
   geom_label_repel(aes(label =rownames(datam)))
  
  
  #scale_size_continuous() ) 
plot(g)
dev.off()
#-----------------------------------#

#-----------------------------------#
# 4 x 2 layout
#-----------------------------------#
# dprime x betweenness
pdf('../Output/dprime_hosei_betweenness_each.month_4x2.pdf', w= 15, h= 7)
g <- ggplot(datam) +
 aes(x = dprime.hosei, y = betweenness , shape = class,fill= class ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = individuals ) , alpha=0.8) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("1-dprime x centrality")) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=10,breaks = c(1,5, 10,20,30,60) ) +
  scale_fill_manual(values = c('indianred3','cadetblue'))+
  scale_colour_manual(values = c('grey28','grey28'))+
  facet_wrap(~month, ncol=4)
  
  #scale_size_continuous() ) 
plot(g)
dev.off()
#-----------------------------------#
#label 
# dprime x betweenness
pdf('../Output/dprime_hosei_betweenness_each.month.label.4x2.pdf', w= 23, h= 10)
g <- ggplot(datam) +
 aes(x = dprime.hosei, y = betweenness , shape = class,fill= class ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = individuals ) , alpha=0.8) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("1-dprime x centrality")) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=10,breaks = c(1,5, 10,20,30,60) ) +
  scale_fill_manual(values = c('indianred3','cadetblue'))+
  scale_colour_manual(values = c('grey28','grey28'))+
  facet_wrap(~month, ncol=4)+
   geom_label_repel(aes(label =rownames(datam)))
  
  
  #scale_size_continuous() ) 
plot(g)
dev.off()
#-----------------------------------#


#-----------------------------------#
  　
# dprime x degree
pdf('../Output/dprime_hosei_degree_each.month.pdf', w= 9, h= 15)
g <- ggplot(datam) +
 aes(x = dprime.hosei, y =degree.st , shape = class,fill= class ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = individuals) ,alpha=0.8) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "degree", 
 title = sprintf("1-dprime x centrality")) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=10,breaks = c(1,5, 10,20,30,60) ) +
  scale_fill_manual(values = c('indianred3','cadetblue'))+
  scale_colour_manual(values = c('grey28','grey28'))+
  facet_wrap(~month, ncol=2)
  
  #scale_size_continuous() ) 
plot(g)


dev.off()  　





#ggplot 
# 直感的に描画できるパッケージを使ってみる
#install.packages("palmerpenguins")
#library(palmerpenguins)
#data(package = 'palmerpenguins')

#install.packages("esquisse")
#esquisse::esquisser(data)

# dprime_ betweenness
#######################
# 補正あり
# data.frame内の列名を使用するときは
#####################
pdf(sprintf('../Output/dprime_hosei_betweenness.%s.pdf',u),h = 6 ,w = 6.5)
g <- ggplot(data) +
 aes(x = dprime.hosei, y = betweenness , shape = class,fill= class ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = individuals ) , alpha=0.8) +
 #scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "betweenness", 
 title = sprintf("1-dprime x centrality.%s",u)) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=6,breaks = c(1,5, 10,20,30) ) +
  scale_fill_manual(values = c('indianred3','cadetblue'))+
  scale_colour_manual(values = c('grey28','grey28'))
  #scale_size_continuous() ) 
  　
plot(g)
dev.off()

glist_bet[[as.character(u)]] <- g




# dprime_ dgree
pdf(sprintf('../Output/dprime_hosei_degree.%s.pdf',u),h = 6 ,w = 6.5)
g <- ggplot(data) +
 aes(x = dprime.hosei, y = degree.st , shape = class,fill= class ) +
  scale_shape_manual(values = c(21,23)) +
 geom_point(aes(  size = individuals ) , alpha=0.8) +
 scale_color_hue(direction = 1) +
 labs(x = "1-d'prime", y = "degree", 
 title = sprintf("1-dprime x centrality.%s",u)) +
 theme_bw() +
 theme(plot.title = element_text(size = 20L, face = "bold", 
 hjust = 0.5), axis.title.y = element_text(size = 22L), axis.title.x = element_text(size = 22L)
 ,axis.text.x = element_text(size = 18L),axis.text.y = element_text(size = 18L))+
 scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))  + 
  scale_size_area(max_size=6,breaks = c(1,5, 10,20,30) ) +
  scale_fill_manual(values = c('indianred3','cadetblue'))+
  scale_colour_manual(values = c('grey28','grey28'))
  #scale_size_continuous() ) 
  　
plot(g)
dev.off()


glist_deg[[as.character(u)]] <- g

######################
# outputしたデータをまとめて出力する方法
#　library(cowplot)　パッケージを使用
#　サイズは要検討
# 　A4サイズにも対応可能
#　空のリストを作って、そこに挿入していく
#　最後にリスト形式でまとめて出力
#  h: 29/2.5 ? w: 21/2.5 ?
######################  
#glist[[as.character(u)]] <- g



# どちらの方法でも出力可能
# ggsave(plot=plot_grid(plotlist=glist, ncol=2),
#		filename='dprime_degree.pdf',h = 30 ,w =12  )


pdf('../Output/dprime_betweenness.pdf',h = 18,w = 11)
plot_grid(plotlist= glist_bet, nrow=4)
dev.off()

pdf('../Output/dprime_degree.pdf',h = 18,w = 11)
plot_grid(plotlist= glist_deg, nrow=4)
dev.off()


