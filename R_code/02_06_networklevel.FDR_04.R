#########################
# date
# Suzuki writing
# code title
#########################


# P値についてFDR補正を行う
month.uni <- as.character(c(4:11))

##################################
# FDR of each network index
##################################

# %s の演算子は　2つ以上の変数を加えることも可能

index <- c('H2','wNODF','cscore.HL','cscore.LL')
for (q in index) {

mat <- matrix(NA, ncol=length(month.uni),nrow=4)
colnames(mat) <- month.uni
rownames(mat) <- c('zscore','p.value','p.value.corre','FDR.value')

p.values <- c(NULL)
zscore <- c(NULL)
	for (i in month.uni) {

dt <- read.table(sprintf('../Output/index_p.value/%s.p.value.%s.txt',q,i),header=T)
p.values <- c(p.values, dt[1,1] )

dt.z <- read.table(sprintf('../Output/index_z.score/%s.zscore.%s.txt',q,i),header=T)
zscore <- c(zscore, dt.z[1,1] )
}

#check
p.values
mat['p.value',] <- p.values
mat['zscore',] <- zscore

for (o in 1:8){	
p <- mat['p.value',o]	
if ( p < 0.5 ) {	
	 mat['p.value.corre',o] <- p
	 } else if (p > 0.5) {
	 mat['p.value.corre',o] <- (0.5 - (p-0.5))	}
		 
	}
	
mat['FDR.value',] <-  round(p.adjust(mat['p.value.corre',], method = "BH")	,digits = 4)
write.table(cbind(rownames(mat),mat) ,sprintf('../Output/index_p.value/%s.FDR.txt',q) ,sep='\t',row.names = F)
}

##################################
# zscore table 
##################################

for (q in index) {

z.score <- c(NULL) 
mat <- matrix(NA, ncol=length(month.uni),nrow=2)
mat[1,] <- month.uni
colnames(mat) <- month.uni
rownames(mat) <- c('month', 'zscore')

for (i in month.uni) {
dt.z <- read.table(sprintf('../Output/index_z.score/%s.zscore.%s.txt',q,i),header=T)
z.score  <- c(z.score, dt.z[1,1])
}
mat[2,] <- z.score

dir.create('../Output/index_z.score/all.data.matrix',showWarnings =F )
write.table(cbind(rownames(mat), mat) ,sprintf('../Output/index_z.score/all.data.matrix/%s.z.score.all.txt',q) ,sep='\t',row.names = F)

mat.t <- as.data.frame(t(mat))
class(mat.t)
mat.t$month <- factor(mat.t$month, level= c('4','5','6','7','8','9','10','11'))
mat.t$zscore <- as.numeric(mat.t$zscore)

library(ggplot2)
# Output Fig.
# geom_path: Each group consists of only one observation. Do you need to adjust the groupaesthetic?
# というエラーが出るときがある。ggplot(xx, aes(x =yy , y= zz ,group = 1)) という設定を加える。


pdf(sprintf('../Output/index_z.score/all.data.matrix/index.zscore.%s.pdf',q))
g <- ggplot(mat.t, aes(x =month , y= zscore ,group = 1)) +
geom_line(colour = 'grey50', size = 1.2) +
geom_point(size= 5 , shape=22 , colour= 'darkred',fill='pink')+
scale_y_continuous(limits = c( -5,5))+
geom_hline(yintercept = 0
,colour = 'grey50', linetype = 'dashed')+
labs( title = sprintf(q) )+
theme(text = element_text(size = 25))
plot(g)
dev.off()

}


###################
# H2のみ出力
####################

for (q in index) {　q= c('H2')	


z.score <- c(NULL) 
mat <- matrix(NA, ncol=length(month.uni),nrow=2)
mat[1,] <- month.uni
colnames(mat) <- month.uni
rownames(mat) <- c('month', 'zscore')

for (i in month.uni) {
dt.z <- read.table(sprintf('../Output/index_z.score/%s.zscore.%s.txt',q,i),header=T)
z.score  <- c(z.score, dt.z[1,1])
}
mat[2,] <- z.score

write.table(cbind(rownames(mat), mat) ,sprintf('../Output/index_z.score/all.data.matrix/%s.z.score.all.txt',q) ,sep='\t',row.names = F)

mat.t <- as.data.frame(t(mat))
class(mat.t)
mat.t$month <- factor(mat.t$month, level= c('4','5','6','7','8','9','10','11'))
mat.t$zscore <- as.numeric(mat.t$zscore)

library(ggplot2)
# Output Fig.
pdf(sprintf('../Output/index_z.score/all.data.matrix/index.zscore.%s.pdf',q))
g <- ggplot(mat.t, aes(x =month , y= zscore ,group = 1)) +
geom_line(colour = 'grey50', size = 1.2) +
geom_point(size= 5 , shape=22 , colour= 'darkred',fill='pink')+
scale_y_continuous(limits = c( 0,15))+
geom_hline(yintercept = 0
,colour = 'grey50', linetype = 'dashed')+
labs( title = sprintf(q) )+
theme(text = element_text(size = 25))
plot(g)
dev.off ()
}







