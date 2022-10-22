library(ggplot2)
library(gcookbook)


#------------------------------------------#
ex <- as.data.frame( read.table('../Data/spider.hanrei.example.txt',header=T) )
col_1 <- read.table('../Data/spider.hunt_mode.color.txt',header=T)
col_2 <-  as.vector(col_1[,2])
ex


pdf('../Output/spider_hanrei.pdf')
ggplot(ex, aes(x = gr, y=number, fill= hunt_mode))   +
geom_col(position= 'dodge')+
 scale_fill_manual(values = col_2)
 
 dev.off()
#------------------------------------------# 
 
