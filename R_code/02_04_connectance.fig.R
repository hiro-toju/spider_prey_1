

mat <- read.table('../Data/connectance.txt', header=T,row.names=1)
q= c('connectance')	

	
mat.t <- as.data.frame(t(mat))
class(mat.t)
mat.t$month <- factor(mat.t$month, level=c('4','5','6','7','8','9','10','11'))
mat.t$zscore <- as.numeric(mat.t$zscore)

library(ggplot2)
# Output Fig.
pdf(sprintf('../Output/index.%s.pdf',q))
g <- ggplot(mat.t, aes(x =month , y= zscore ,group = 1)) +
geom_line(colour = 'grey50', size = 1.2) +
geom_point(size= 5 , shape=22 , colour= 'darkred',fill='pink')+
scale_y_continuous(limits = c( 0,0.3))+
geom_hline(yintercept = 0
,colour = 'grey50', linetype = 'dashed')+
labs( title = sprintf(q) )+
theme(text = element_text(size = 25))
plot(g)
dev.off()

