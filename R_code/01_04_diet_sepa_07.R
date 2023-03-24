############################################
# 2021.11.15
# Suzuki
# grazing/detrital matrix
# R 4.0.4
############################################


library('dplyr')


# prey.df = order まで分類されているprey の分類群情報＋シーケンスデータ
prey.df <- read.table("../Data/03_taxonomy_table_mer.Hexa_ID_notUn.2.txt",header=T,sep ='\t')
prey.df[1:15,1:15]
# select hexa_id and family
prey.id <- select(prey.df, hexa_id, family)

# grazing/detritus infomation, family level
diet.data <- read.table('../Data/Family.Food_2_Toju.txt',header=T,sep ='\t')
diet.gd <- select(diet.data,1,4)

herb.uni <- unique(subset(diet.gd ,diet.gd$diet == 'herbivorous'))
det.uni <- unique(subset(diet.gd ,diet.gd$diet == 'detritivorous'))
predatory.uni <- unique(subset(diet.gd ,diet.gd$diet == 'predatory/parasitic'))

#################################################################

# April - November 
month.uni <- as.character(c(4:11))
row.species <- c("herbivorous", "predatory/parasitic" ,"detritivorous","variable")


# 全てのポジティブサンプルにおける生食/腐食/不明　X 各月（4-11月）の空マトリクス
month.level <- matrix(NA, nrow=length(row.species),ncol=length(month.uni))
rownames(month.level) <- row.species
colnames(month.level) <- month.uni

# herbivorous each month matrix
month.level.herb <- matrix(NA, nrow=length(herb.uni$family),ncol=length(month.uni))
rownames(month.level.herb) <-  herb.uni$family
colnames(month.level.herb) <- month.uni

# detritivorous each month matrix
month.level.det <- matrix(NA, nrow=length(det.uni$family),ncol=length(month.uni))
rownames(month.level.det) <-  det.uni$family
colnames(month.level.det) <- month.uni

# predatory each month matrix
month.level.predatory <- matrix(NA, nrow=length(predatory.uni$family),ncol=length(month.uni))
rownames(month.level.predatory) <-  predatory.uni$family
colnames(month.level.predatory) <- month.uni
############################################


for (i in month.uni) {
 
  
# input 'spider sp. x hexa id(OTU)level matrix'
month.agg.data <- 	read.table(sprintf('../Data/prey.id.prey.matrix.agg.%s.txt',i),header=T,sep ='\t',row.names=1)
# except 0 OTU
month.agg.data <-   t(subset(t(month.agg.data), rowSums(t(month.agg.data)) > 0 ))
dim(month.agg.data)
#head(month.agg.data)


month.agg.data.t <- as.data.frame(t(month.agg.data))
month.agg.data.t <- cbind(colnames(month.agg.data),month.agg.data.t)
head(month.agg.data.t)
# merge hexa_id x diet info
merge.data.diet <- merge(prey.id, diet.gd,by.x='family', by.y='family' )

write.table(merge.data.diet,file= '../Data/hexa.id.diet.merge.txt', sep='\t',quote=F,row.names=F)

merge.data.diet.order <- merge.data.diet[order(merge.data.diet$hexa_id), ]

# dietデータと各月のデータ

merge.month <- merge(merge.data.diet.order,month.agg.data.t,by.x='hexa_id',by.y='colnames(month.agg.data)')
head(merge.month)

write.table(merge.month,file= sprintf('../Data/month.diet.merge.%s.txt',i), sep='\t',quote=F,row.names=F)

merge.month.sum.mat  <- merge.month[c(-1:-3)]

sp.level <- matrix(NA, nrow=length(row.species),ncol=(ncol(merge.month.sum.mat)))
for(k in 1:length(row.species)){
  sp.level[k, ] <- colSums(subset(merge.month.sum.mat,as.vector(merge.month[,3]) == row.species[k] ))
}
colnames(sp.level) <- colnames(merge.month.sum.mat)
rownames(sp.level) <- row.species

write.table(cbind(as.matrix(row.species), sp.level),file= sprintf('../Data/month.diet.compo.%s.txt',i), sep='\t',quote=F,row.names=F)

# month.level  に代入
month.level[,i] <-as.matrix(rowSums(sp.level))


#######################################
# each diet composition
# 各食性ごとにカテゴリ分けしてその内訳をだす
####################################

herb <- subset(merge.data.diet.order ,merge.data.diet.order$diet == 'herbivorous')
det <- subset(merge.data.diet.order ,merge.data.diet.order$diet == 'detritivorous')
predatory <- subset(merge.data.diet.order ,merge.data.diet.order$diet == 'predatory/parasitic')


###############
# herbivorous
###############
merge.herb <- merge(herb,month.agg.data.t,by.x='hexa_id',by.y='colnames(month.agg.data)')

write.table(merge.herb,file= sprintf('../Data/merge.herb.%s.txt',i), sep='\t',quote=F,row.names=F)

# hexa_id ,diet 列を除く
merge.herb_2  <- merge.herb[c(-1:-3)]

row.species.herb <- unique(merge.herb$family)


sp.level.herb <- matrix(NA, nrow=length(row.species.herb),ncol=(ncol(merge.month.sum.mat)))

for(k in 1:length(row.species.herb) ){
  sp.level.herb[k, ] <- colSums(subset(merge.herb_2,as.vector(merge.herb[,2]) == row.species.herb[k] ))
}
colnames(sp.level.herb) <- colnames(merge.month.sum.mat)
rownames(sp.level.herb) <- row.species.herb

write.table(cbind(as.matrix(row.species.herb), sp.level.herb),file= sprintf('../Data/sp.level.herb.compo.%s.txt',i), sep='\t',quote=F,row.names=F)

# month.level  に代入
agg.herb <- as.matrix(rowSums(sp.level.herb))
colnames(agg.herb) <- c(i)
# as.data.frame()にするとうまくマトリクスに代入できる.
month.level.herb[,i] <-  as.data.frame(agg.herb)[rownames(month.level.herb), ]

###############
# detritivorous
###############
merge.det <- merge(det,month.agg.data.t,by.x='hexa_id',by.y='colnames(month.agg.data)')

write.table(merge.det,file= sprintf('../Data/merge.det.%s.txt',i), sep='\t',quote=F,row.names=F)

# hexa_id ,diet 列を除く
merge.det_2  <- merge.det[c(-1:-3)]

row.species.det <- unique(merge.det$family)


sp.level.det <- matrix(NA, nrow=length(row.species.det),ncol=(ncol(merge.month.sum.mat)))

for(k in 1:length(row.species.det) ){
  sp.level.det[k, ] <- colSums(subset(merge.det_2,as.vector(merge.det[,2]) == row.species.det[k] ))
}
colnames(sp.level.det) <- colnames(merge.month.sum.mat)
rownames(sp.level.det) <- row.species.det

write.table(cbind(as.matrix(row.species.det), sp.level.det),file= sprintf('../Data/sp.level.det.compo.%s.txt',i), sep='\t',quote=F,row.names=F)

# month.level  に代入
agg.det <- as.matrix(rowSums(sp.level.det))
colnames(agg.det) <- c(i)
# as.data.frame()にするとうまくマトリクスに代入できる.
month.level.det[,i] <-  as.data.frame(agg.det)[rownames(month.level.det), ]


###############
# predatory/parasitic
###############
merge.predatory <- merge(predatory,month.agg.data.t,by.x='hexa_id',by.y='colnames(month.agg.data)')

write.table(merge.predatory,file= sprintf('../Data/merge.predatory.%s.txt',i), sep='\t',quote=F,row.names=F)

# hexa_id ,diet 列を除く
merge.predatory_2  <- merge.predatory[c(-1:-3)]

row.species.predatory <- unique(merge.predatory$family)


sp.level.predatory <- matrix(NA, nrow=length(row.species.predatory),ncol=(ncol(merge.month.sum.mat)))

for(k in 1:length(row.species.predatory) ){
  sp.level.predatory[k, ] <- colSums(subset(merge.predatory_2,as.vector(merge.predatory[,2]) == row.species.predatory[k] ))
}
colnames(sp.level.predatory) <- colnames(merge.month.sum.mat)
rownames(sp.level.predatory) <- row.species.predatory

write.table(cbind(as.matrix(row.species.predatory), sp.level.predatory),file= sprintf('../Data/sp.level.predatory.compo.%s.txt',i), sep='\t',quote=F,row.names=F)

# month.level  に代入
agg.predatory <- as.matrix(rowSums(sp.level.predatory))
colnames(agg.predatory) <- c(i)
# as.data.frame()にするとうまくマトリクスに代入できる.
month.level.predatory[,i] <-  as.data.frame(agg.predatory)[rownames(month.level.predatory), ]

}

month.level.herb[is.na(month.level.herb)] <- 0 
month.level.det[is.na(month.level.det)] <- 0 
month.level.predatory[is.na(month.level.predatory)] <- 0 

write.table(cbind(as.matrix(herb.uni$family), month.level.herb),file='../Data/month.diet.result.HERB.txt', sep='\t',quote=F,row.names=F)
write.table(cbind(as.matrix(det.uni$family), month.level.det),file='../Data/month.diet.result.DET.txt', sep='\t',quote=F,row.names=F)
write.table(cbind(as.matrix(predatory.uni$family), month.level.predatory),file='../Data/month.diet.result.predatory.txt', sep='\t',quote=F,row.names=F)

write.table(cbind(as.matrix(row.species), month.level),file='../Data/month.diet.result.txt', sep='\t',quote=F,row.names=F)



 