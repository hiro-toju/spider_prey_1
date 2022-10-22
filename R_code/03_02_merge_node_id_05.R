#########################
# 2021.12.21
# Suzuki writing
# merge node id info
#########################


# infomapのcluデータからモジュールの番号を書き出す作業は既に終えている。
#　次にRcolorBlowerからSet3を抜き出して色分けする
# as.matrix()で列を結合する
#　infomap の初期グレーの色のカラー番号を要確認　：#B6B69F
# モジュールのMAXは5月の21モジュール


library(dplyr)
library(RColorBrewer)

# H_xxx の値が小さい順にprey　を並び替える
prey.diet.info <-  read.table('../Data/hexa.id.diet.merge_id_sort.txt',header=T,sep ='\t')
head(prey.diet.info)

node.info <- read.table('../Data/infomap_node_id.txt',header=T,sep ='\t')
head(node.info)


#month.uni <- as.character(c(4:11))
month.uni <- as.character(c(4:9,11))
modu.max.check <- c()

#for fanction
# 10月のみモジュールの数が1でエラーがでるため、10月を除いて for関数を回す
##################
# 4-9,11 月の場合
##################
for (i in month.uni) {

#input month data	
# 今回は区切りをスペース(' ')に設定する	
#month.d <- read.table(sprintf('prey.matrix.agg.%s.txt_adj.txt.clu.txt',i),sep =' ',header=T) 	
month.d <- read.table(sprintf('../Data/Infomap/prey.id.prey.matrix.agg.%s.txt_adj.txt.clu.txt',i),sep =' ') 	
head(month.d)
#dim(month.d)
colnames(month.d) <- c('node_id','module','flow')
# only module info, merge node info
month.d.sub <- subset(month.d,month.d$flow > 0)	
month.d.merge <- merge(node.info, month.d.sub,by.x='node_id',by.y='node_id')

diet.subset <- subset(prey.diet.info, prey.diet.info$hexa_id %in%  month.d.merge$sp_name )
dim(diet.subset)


diet.set <- as.matrix( c( rep('spider',length(rownames(month.d.merge))-length(rownames(diet.subset)))  , diet.subset$diet ))

diet.set_2 <- cbind(month.d.merge , diet.set)
head(diet.set_2)


dir.create('../Output/Infomap/month.module.matrix', showWarnings=F)
write.table(diet.set_2, sprintf('../Output/Infomap/month.module.matrix/module.matrix.%s.txt',i),row.names=F,sep ='\t')	

# module number
modu.max.check <- c(max(month.d.merge$module))
modu.max.check.allmonth <- c(modu.max.check ,max(month.d.merge$module))
# モジュールカラーのマトリクス作成
modu.col <- matrix(NA,nrow=max(modu.max.check),ncol=2)
colnames(modu.col) <- c('module','color')
modu.col[,1] <- c(1:max(modu.max.check))


# エラー　どう対応するか　

f <-  modu.max.check　
if (f < 12) {    
  modu.col[,2] <- c(brewer.pal( f,'Set3'))
  } else  {
  modu.col[,2] <-  c(brewer.pal(12,'Set3'),rep('#B6B69F',max(modu.max.check)-12))
  }
  
  write.table(modu.col,sprintf('../Output/Infomap/month.module.matrix/module.color.%s.txt',i),row.names=F,sep ='\t')

  
#for (j in 1:length(modu.max.check) ) {
for (j in 1:modu.max.check ) {	
	month.d.merge[month.d.merge$module == j ,"col"] <- modu.col[j,2]
	}

write.table(month.d.merge,sprintf('../Output/Infomap/month.module.matrix/module.matrix.col.%s.txt',i),row.names=F,sep ='\t')	

}


##################
# 10 月の場合
##################
	
	
for (i in 10) { i=10
		

#input month data	
# 今回は区切りをスペース(' ')に設定する	
#month.d <- read.table(sprintf('prey.matrix.agg.%s.txt_adj.txt.clu.txt',i),sep =' ',header=T) 	
month.d <- read.table(sprintf('../Data/Infomap/prey.id.prey.matrix.agg.%s.txt_adj.txt.clu.txt',i),sep =' ') 	
head(month.d)
#dim(month.d)
colnames(month.d) <- c('node_id','module','flow')
# only module info, merge node info
month.d.sub <- subset(month.d,month.d$flow > 0)	
month.d.merge <- merge(node.info, month.d.sub,by.x='node_id',by.y='node_id')

diet.subset <- subset(prey.diet.info, prey.diet.info$hexa_id %in%  month.d.merge$sp_name )
dim(diet.subset)


diet.set <- as.matrix( c( rep('spider',length(rownames(month.d.merge))-length(rownames(diet.subset)))  , diet.subset$diet ))

diet.set_2 <- cbind(month.d.merge , diet.set)
head(diet.set_2)


write.table(diet.set_2,sprintf('../Output/Infomap/month.module.matrix/module.matrix.%s.txt',i),row.names=F,sep ='\t')	

# module number
modu.max.check <- c(max(month.d.merge$module))
modu.max.check.allmonth <- c(modu.max.check ,max(month.d.merge$module))
# モジュールカラーのマトリクス作成
modu.col <- matrix(NA,nrow=max(modu.max.check),ncol=2)
colnames(modu.col) <- c('module','color')
modu.col[,1] <- c(1:max(modu.max.check))
modu.col[,2] <- c( "#8DD3C7" )
 write.table(modu.col,sprintf('../Output/Infomap/month.module.matrix/module.color.%s.txt',i),row.names=F,sep ='\t')

  
for (j in 1:length(modu.max.check) ) {
	month.d.merge[month.d.merge$module == j ,"col"] <- modu.col[j,2]
	}

write.table(month.d.merge, sprintf('../Output/Infomap/month.module.matrix/module.matrix.col.%s.txt',i),row.names=F,sep ='\t')	

}



###########　END　##############


