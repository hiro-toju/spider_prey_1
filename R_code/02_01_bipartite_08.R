
######################
# 2020.12.04.
# Suzuki writing
# 2021.08.25
# Suzuki edit
# R 4.0.4 GUI 1.74 Catalina build (7936)
# bipartite network 
# using for function
######################

library(bipartite)
library(RColorBrewer)
library('dplyr')
library(parallel)
library(tictoc)
library(foreach)
library(doParallel)

# check core
detectCores(logical = FALSE)
detectCores()
registerDoParallel(cores=4)

######################

spider.info <- read.table('../Data/spider_ecology_20220301.txt',header=T,sep ='\t',row.names=1)	
hexa.info <- read.table('../Data/03_taxonomy_table_mer.Hexa_ID_notUn.2.txt',header=T,sep ='\t',row.names=1)	
########################
# each month data analysis
########################

month.uni <- c(4:11)

for (i in month.uni) { 

mat <- read.table(sprintf('../Data/prey.id.prey.matrix.agg/prey.id.prey.matrix.agg.%s.txt',i), header=T, sep ='\t', row.names=1)
mat2 <- t(subset(t(mat), rowSums(t(mat)) > 0 ))
mat3 <-   subset(mat2, rowSums(mat2)  > 0 )
mat3.t <- t(mat3)

write.table(mat3.t, sprintf('../Data/prey.matrix.agg.0OTU.%s.txt',i), sep='\t', quote=F, row.names=T)

spi.indi <- as.matrix(rowSums(mat3))
spi.indi <- as.data.frame(rowSums(mat3))
spi.indi2 <- cbind(species=rownames(spi.indi), as.data.frame(spi.indi))
colnames(spi.indi2) <- c('species','indi')

sort.spi <- spi.indi2[order(spi.indi2$indi,decreasing = T), ]

# 各月のクモ種だけを取り出す
spi.eco <- subset(spider.info, spider.info[,4] %in% rownames(mat3) )
#length(rownames(spi.eco))

# 各月のHexa OTUだけを取り出す
hexa.tax <- subset(hexa.info, rownames(hexa.info) %in% rownames(mat3.t)  )
length(rownames(hexa.tax))


tmp <-spi.eco

tmp$col <- "grey70"

tmp[tmp[,7]== "sit-and-wait","col"] <-'lightpink1'
tmp[tmp[,7]=="web-weaving" ,"col"] <-  'darkseagreen'
tmp[tmp[,7]=="active-hunting","col"] <-　'steelblue'
tmp[tmp[,7]== "Attack" ,"col"] <-    'darkslategray'   
tmp[tmp[,7]== "freeloader" ,"col"] <-'grey63'

spi.col <-tmp$col
mer.spi <- tmp[,c(4,7,8)]
length(rownames(mer.spi))

###############

narabi <- left_join(sort.spi, mer.spi,by='species' )
mat.spname <- cbind(species=rownames(mat3), as.data.frame(mat3))
length(rownames(mat3))
#head(mat.spname)

join <- left_join(narabi, mat.spname,by='species' )
head(join)

as.matrix(join[,1])
#join[,c('species','col')]

join.col <- join[,'col']
join.mat <- join[,c(-1:-4)]
#head(join.mat,5)
rownames(join.mat) <-join[,1]
	

# hexa
brewer.pal(8,"Set2")
# "#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"

brewer.pal(9,"Pastel1")
# "#FBB4AE" "#B3CDE3" "#CCEBC5" "#DECBE4" "#FED9A6" "#FFFFCC" "#E5D8BD" "#FDDAEC" "#F2F2F2"



tmp <-　hexa.tax
unique(tmp[,6])
# colour for spider ecology
tmp$col <- "grey70"


#各Orderごとに色を挿入していく
# こちらの組み合わせに変更の可能性あり
tmp[tmp[,6]=='Diptera',"col"] <- "#66C2A5"
tmp[tmp[,6]=='Hemiptera',"col"] <-"#FC8D62"
tmp[tmp[,6]=='Hymenoptera',"col"] <-"#8DA0CB"
tmp[tmp[,6]=='Orthoptera',"col"] <-"#E78AC3"
tmp[tmp[,6]=='Collembola',"col"] <-"#A6D854"
tmp[tmp[,6]=='Lepidoptera',"col"] <-"#FFD92F"
tmp[tmp[,6]=='Coleoptera',"col"] <-"#E5C494"
tmp[tmp[,6]=='Ephemeroptera',"col"] <-"#B3B3B3"
tmp[tmp[,6]=='Blattodea',"col"] <-"#FBB4AE"
tmp[tmp[,6]=='Thysanoptera',"col"] <-"#B3CDE3"
tmp[tmp[,6]=='Odonata',"col"] <-"#DECBE4"
tmp[tmp[,6]=='Psocoptera',"col"] <-"#FED9A6"
tmp[tmp[,6]=='Dermaptera',"col"] <-"#FFFFCC"
tmp[tmp[,6]=='Strepsiptera',"col"] <-"#E5D8BD"
tmp[tmp[,6]=='Isopoda',"col"] <-"#FDDAEC"
tmp[tmp[,6]=='Lithobiomorpha',"col"] <-"#F2F2F2"

hexa.col <-tmp$col


dim(hexa.tax)
dim(join.mat)

x1 <- rownames(hexa.tax)
x2 <- colnames(join.mat)


pdf(sprintf("../Output/bipartite.%s.pdf",i),width=10,heigh=4.4)
   
plotweb(join.mat,
          text.rot=90,
          labsize= 0.4,
          empty=F,
          col.interaction= "grey40",
          bor.col.interaction = "grey40" ,
          ybig= 1.2,
          col.high = hexa.col,
          col.low  = join.col,
          bor.col.high='white',
          bor.col.low='white'
                   )

          
   dev.off()

}


########################
# all month data analysis
#
########################


#　各月ごとのバイナリデータ　for 関数に変えたい
mat <- read.table('../Data/hexa_id.prey.matrix.agg.all.txt',header=T,sep ='\t',row.names=1)
mat2 <- t(subset(t(mat), rowSums(t(mat)) > 0 ))
mat3 <-   subset(mat2, rowSums(mat2)  > 0 )
mat3.t <- t(mat3)

dim(mat3.t)

# 0OTU OUTPUT

write.table(mat3.t,file ='../Data/prey.matrix.agg.0OTU.ALL.txt',sep='\t',quote=F,row.names=T)

spi.indi <- as.matrix(rowSums(mat3))
spi.indi <- as.data.frame(rowSums(mat3))
spi.indi2 <- cbind(species=rownames(spi.indi), as.data.frame(spi.indi))
colnames(spi.indi2) <- c('species','indi')

sort.spi <- spi.indi2[order(spi.indi2$indi,decreasing = T), ]

# 各月のクモ種だけを取り出す
spi.eco <- subset(spider.info, spider.info[,4] %in% rownames(mat3) )
#length(rownames(spi.eco))

# 各月のHexa OTUだけを取り出す
hexa.tax <- subset(hexa.info, rownames(hexa.info) %in% rownames(mat3.t)  )
length(rownames(hexa.tax))
write.table(hexa.tax ,file ='../Data/hexa.tax.info.ALL.txt',sep='\t',quote=F,row.names=T)



# 属性ごとに色分けする方法　色の列名を加える
# spider
#################
#　やり方その１
tmp <- spi.eco

tmp$col <- "grey70"

tmp[tmp[,7]== "sit-and-wait","col"] <-'lightpink1'
tmp[tmp[,7]=="web-weaving" ,"col"] <-  'darkseagreen'
tmp[tmp[,7]=="active-hunting","col"] <-　'steelblue'
tmp[tmp[,7]== "Attack" ,"col"] <-    'darkslategray'   
tmp[tmp[,7]== "freeloader" ,"col"] <-'grey63'


spi.col <-tmp$col
#mer.spi <- tmp[,c(1,4,6)]
mer.spi <- tmp[,c(4,7,8)]

length(rownames(mer.spi))

###############

narabi <- left_join(sort.spi, mer.spi,by='species' )
mat.spname <- cbind(species=rownames(mat3), as.data.frame(mat3))
length(rownames(mat3))

head(mat.spname)

join <- left_join(narabi, mat.spname, by='species' )
head(join)

as.matrix(join[,1])
#join[,c('species','col')]

join.col <- join[,'col']
join.mat <- join[,c(-1:-4)]
#head(join.mat,5)
rownames(join.mat) <-join[,1]

# hexa
brewer.pal(8,"Set2")
# "#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"

brewer.pal(9,"Pastel1")
# "#FBB4AE" "#B3CDE3" "#CCEBC5" "#DECBE4" "#FED9A6" "#FFFFCC" "#E5D8BD" "#FDDAEC" "#F2F2F2"



tmp <-　hexa.tax
dim(tmp)
#head(tmp)


unique(tmp[,6])
# colour for spider ecology
tmp$col <- "grey70"


#各Orderごとに色を挿入していく
# こちらの組み合わせに変更の可能性あり
#　2022.01.26
tmp[tmp[,6]=='Diptera',"col"] <- "#66C2A5"
tmp[tmp[,6]=='Hemiptera',"col"] <-"#FC8D62"
tmp[tmp[,6]=='Hymenoptera',"col"] <-"#8DA0CB"
tmp[tmp[,6]=='Orthoptera',"col"] <-"#E78AC3"
tmp[tmp[,6]=='Collembola',"col"] <-"#A6D854"
tmp[tmp[,6]=='Lepidoptera',"col"] <-"#FFD92F"
tmp[tmp[,6]=='Coleoptera',"col"] <-"#E5C494"
tmp[tmp[,6]=='Ephemeroptera',"col"] <-"#B3B3B3"
tmp[tmp[,6]=='Blattodea',"col"] <-"#FBB4AE"
tmp[tmp[,6]=='Thysanoptera',"col"] <-"#B3CDE3"
tmp[tmp[,6]=='Odonata',"col"] <-"#DECBE4"
tmp[tmp[,6]=='Psocoptera',"col"] <-"#FED9A6"
tmp[tmp[,6]=='Dermaptera',"col"] <-"#FFFFCC"
tmp[tmp[,6]=='Strepsiptera',"col"] <-"#E5D8BD"
tmp[tmp[,6]=='Isopoda',"col"] <-"#FDDAEC"
tmp[tmp[,6]=='Lithobiomorpha',"col"] <-"#F2F2F2"


# colour for hexa 
hexa.col <-tmp$col

#check
dim(hexa.tax)
dim(join.mat)

x1 <- rownames(hexa.tax)
x2 <- colnames(join.mat)
#write.table(x1,sep='\t',quote=F,row.names=T)
#write.table(colnames(join.mat),sep='\t',quote=F,row.names=T)
#################	

	


pdf("../Output/bipartite.ALL.pdf",width=120,heigh=40)
   
plotweb(join.mat,
          text.rot=90,
          labsize= 1.5,
          empty=F,
          col.interaction= "grey40",
          bor.col.interaction = "grey30" ,
          ybig= 1.2,
          col.high = hexa.col,
          col.low  = join.col,
          bor.col.high='white',
          bor.col.low='white'
                   )

          
   dev.off()


