
#########################
# 2021.11.16
# Fujita
# each species diet
#########################


# April - November 
month.uni <- as.character(c(4:11))
row.species <- c("herbivorous", "predatory/parasitic" ,"detritivorous","variable")

##################################
# each.species
##################################
# top 30 positive sample data
# sp. list

dominant.spider.sp <- c('Oxyopes_sertatus','Argiope_bruennichi','Tetragnatha_caudicula','Leucauge_blanda','Ebrechtella_tricuspidata','Xysticus_sp.','Larinia_argiopiformis','Neoscona_scylloides','Tetragnatha_praedonia','Nephila_clavata','Mendoza_elongata','Araneidae_sp.1','Neoscona_mellotteei')

splist <- c()

for (u in dominant.spider.sp) {
	
	month.level <- matrix(0, nrow=length(row.species),ncol=length(month.uni))
	rownames(month.level) <- row.species
	colnames(month.level) <- month.uni
	
	
	for (i in month.uni) {
		
		month.agg.data <- 	as.data.frame(read.table(sprintf('../Data/month.diet.compo.%s.txt',i),header=T,sep ='\t',row.names=1)  )
		
		# month.level  に代入
		#month.level[,i] <-  month.agg.data$Oxyopes_sertatus
		
		if( any(u==colnames(month.agg.data)) )	month.level[,i] <-  month.agg.data[,u]
	
	}
	
	splist[[u]] <- month.level

write.table(cbind(rownames(month.agg.data),splist[[u]]),sprintf('../Data/diet.compo.%s.txt',u),sep='\t',row.names=F)
 
}
##################################
# each.species end
##################################


##################################
# hunt mode
##################################


hunt.type <- read.table('../Data/spider_ecology_20220301.txt',header=T,sep ='\t',row.names=1)
# [1] "unidentified"   "sit-and-wait"   "web-weabing"    "active-hunting" "Attack"    [6] "freeloader"  
hunt.uni <- unique(hunt.type$hunting_mode)

#splist <- c()

for (u in hunt.uni) {
	
 hunt.type.sub <-  subset(hunt.type, hunt.type$hunting_mode == u)
	
	# empty matrix
	month.level <- matrix(0, nrow=length(row.species),ncol=length(month.uni))
	rownames(month.level) <- row.species
	colnames(month.level) <- month.uni
	
	for (i in month.uni) {
		
		month.agg.data <- 	t(as.data.frame(read.table(sprintf('../Data/month.diet.compo.%s.txt',i),header=T,sep ='\t',row.names=1)  ) )
		month.agg.data_2 <- subset(month.agg.data, rownames(month.agg.data) %in% hunt.type.sub$species    )
		 month.agg.data_3 <- colSums(month.agg.data_2)

		
		# month.level  に代入
		#month.level[,i] <-  month.agg.data$Oxyopes_sertatus
		
		#if( any(u==colnames(month.agg.data_3)) )	
		month.level[,i] <-  month.agg.data_3
	
	}
	
	#splist[[u]] <- month.level


write.table(cbind(rownames(month.level), month.level),sprintf('../Data/diet.compo.%s.txt',u),sep='\t',row.names=F)
 
}

##################################
# hunt mode end
##################################

