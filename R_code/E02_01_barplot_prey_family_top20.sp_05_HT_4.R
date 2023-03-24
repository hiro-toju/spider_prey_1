######################
# 2020.12.9.
# Suzuki edit
# 2021.01.19
# Suzuki Edit
# 2021.08.25
# Suzuki edit
# prey composition barplot
######################


library(tidyr)
library(ggplot2)
library(ggsci)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(dplyr)

#fujita's code
source('functions.R')
level<- "family" #Family, Genusなど

spider.data <- read.table('../Data/sampling.spider.dominant.sp.txt',header=T,row.names=1)

dominant.rank <- spider.data[,1]

## ==================================================================== ##

color.mat <- read.table('../Data/prey.color_2.txt',header=T)
color.mat_2 <-cbind(narabi=rownames(color.mat), color.mat )
color.family <- color.mat[,'family']

# family level の場合、25色使用する
#col.vector <- color.mat[,'color']
col.vector <- rev(color.mat[,'color'])					
length(col.vector)		

## ==================================================================== ##

for (i in dominant.rank) {
	
mat <- read.table(sprintf('../Data/family.level.each.sp/prey.family.compo.%s.txt',i),header=T,row.names=1)

# only 'other' prey family
mat.other  <- filter(mat , !rownames(mat) %in% color.family )
dim(mat.other)
mat.other.sum <- as.matrix( colSums(mat.other))
colnames(mat.other.sum) <- 'other'

# Unidentifiedが含まれるので +1 			
dominant <- t(subset(mat , rownames(mat) %in% color.family ))
dim(dominant)
dominant.mer <- as.data.frame(cbind(family=colnames(dominant),t(dominant)) )
dominant.mer_2 <- inner_join(color.mat_2 , dominant.mer ,by = "family" )
rownames(dominant.mer_2) <- dominant.mer_2[, 'family']
dominant.mer_3 <- as.data.frame(t(dominant.mer_2[, c(-1:-3)]))

# Unidentified
unidendified <- which(colnames(dominant.mer_3)=="Unidentified")
dominant2 <- dominant.mer_3[, -unidendified]
dim(dominant2)
Unidentified.col <- t(as.matrix(mat['Unidentified',]) )
colnames(Unidentified.col) <- 'Unidentified'

dominant3 <- as.data.frame(cbind(month=rownames(dominant2),dominant2, mat.other.sum ,Unidentified.col))
 
lf <- gather(dominant3, key, value,-1)
lf$value <- as.numeric(as.vector(lf$value))
#lf$key <- factor(lf$key, level= color.family)
lf$key <- factor(lf$key, level= rev(color.family))
#pal <- unique(lf$key)

lf$month <- factor(lf$month, level=c( "X4", "X5" , "X6",  "X7",  "X8" , "X9" , "X10", "X11")) 
  
### number stacking barplot ### 250

ylim = max(colSums(mat))

h <- ggplot(lf, aes(x = month, y = value, fill = key))　+
	 geom_bar(stat = "identity")+
	  scale_fill_manual(values= col.vector)+
	  #theme_minimal() +
	  theme_light() + ggtitle(sprintf("%s", i)) +
	   scale_y_continuous(limits = c(0, 1.1 * ylim), expand=c(0,0))
	  #guides(fill=guide_legend(reverse=T,
	   #ncol=1))
	
h <- h + theme(text = element_text(size = 10))

pdf(sprintf("../Output/%s_barplot.number.pdf", i), w=7, h=4) 
plot(h)
dev.off() 

}

### number stacking barplot ###


