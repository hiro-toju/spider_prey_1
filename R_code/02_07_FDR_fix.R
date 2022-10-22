#########################
# date
# Suzuki writing
# code title
#########################


# 手動で補正したP値をFDRで補正する


# P値についてFDR補正を行う

index <- c('H2','wNODF','cscore.HL','cscore.LL')
for (q in index) {
 data <- read.table(sprintf('../Output/index_p.value/%s.FDR.txt',q),header=T)
 
p.values <- data['p.value.fix' ,]
q.values <- round(p.adjust(p.values, method = "BH") ,4)
q.values
is.significant <- (q.values < 0.025)

output <- rbind( data, p.values, is.significant)

write.table(cbind(c(rownames(data),'FDR.fix','TorF.fix'), output) ,sprintf('../Output/index_p.value/%s.FDR_fix.txt',q) ,sep='\t',row.names = F)
}

