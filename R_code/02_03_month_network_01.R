
#########################################
# 2020.10.20
# suzuki edit
# 2020.10.21 Fujita Edit
# 2020.12.14 Suzuki Edit
# 20201.09.08 
# Suzuki edit
# R 4.0.4 GUI 1.74 Catalina build (7936)
# month network index  
##########################################

 
# networklevel() index

data <- list.files('../Data/prey.id.prey.matrix.agg')

df <- lapply(data, function(x){t(read.table(sprintf("../Data/prey.id.prey.matrix.agg/%s",x), header=T,row.names=1)) 
	})
names(df) <- data


library(bipartite)
#install.packages('doParallel')
library(doParallel)

				
cluster = makeCluster(detectCores()-2, type = "FORK")
registerDoParallel(cluster)


s <- proc.time()[3]
tmp.result <- foreach(e = 1:length(df), .combine=cbind)%dopar%{
	c(names(df)[e], unlist(networklevel(df[[e]], index='ALLBUTDD')))
}				
e <- proc.time()[3]
			


net.all <- tmp.result




#colname1 <- as.matrix(net.all[,0])
#rownames(colname1) <- c("index")
net.all.2 <- cbind(index=rownames(net.all), net.all)





month.AtoZ <- c("index",4:11)

colnames(net.all.2) <- month.AtoZ

#net.all.2[order(colnames(net.all.2), decreasing=T),]

# except file.name prey.matrix.....txt
net.all.3 <-net.all.2[-1, ]
# except index columns
net.all.4 <- t(net.all.2[-1,-1])

#data.sort <-  net.all.4[order(rownames(net.all.4), decreasing = F ),]


#month.ture <- c(4:11)

#data.sort2 <- cbind(as.matrix(month.ture),data.sort )


write.table(net.all.3, file="../Data/net.index.allmonth_a.txt",sep="\t",quote=F,row.name=F,col.name=T)
###
write.table(net.all.4, file="../Data/net.index.allmonth_b.txt",sep="\t",quote=F,row.name=F,col.name=T)

###############


index <- as.data.frame(t(read.table("../Data/net.index.allmonth_b.txt",header=T,row.names=1, sep ='\t')))

#index.sub <- as.data.frame(read.table("~/Desktop/S46_R_analysis/Network_index_month/output/net.index.allmonth_3.txt",header=T, sep ='\t'))

#index.row <- index.sub[,1] 



#index[rownames()]



#x.aes <- c("April","May","June","July","August","September","October","November")
x.aes <- c(4:11)

index.t <- as.data.frame(t(index))


library(ggplot2)
season <- 4:11
index.t2 <- cbind(season,index.t)

indexlabel <- colnames(index.t2)

# Output figure


for(i in as.character(indexlabel)) {
#for(i in 1:length(colnames(index.t2))) {	
	
pdf(file=sprintf("../Output/index.seasonal_%s.pdf",i), width= 8, height=5)


g <-ggplot() +
	theme_classic() +
	geom_line(data= index.t2, aes(x=season, y=index.t2[,i])) +
	 scale_x_continuous(breaks=seq(4,11,1)) +
　　　#scale_x_discrete(limit=c("April","May","June","July","August","September","October","November"))+
	 geom_point(data= index.t2, aes(x=season, y=index.t2[,i] ), size=4)

g <- g +  xlab(NULL) + ylab(NULL) + theme(axis.text.x = element_text(size = 25))+ theme(axis.text.y = element_text(size = 25))
g


###!!!!!!#####	
plot(g)
###!!!!!!#####	


dev.off()
	
}

head(index.t2)


# index all in  #########

as.character(indexlabel)


library(tidyr)

lf <- gather(index.t2, key, value, -1)

g <-ggplot(lf) +
	#theme_bw() +
	theme_classic() +
	geom_line( aes(x=season, y=value) ) +
	scale_x_continuous(breaks=seq(4,11,1)) +
	geom_point(aes(x=season, y=value)) +
	facet_wrap(~key, scales="free")


pdf("../Output/index_gather.pdf",w= 15, h=15)
###!!!!!!#####	
plot(g)
###!!!!!!#####

dev.off()

# index all in  end #########

# Use R plot()
#plot(index.t2$season, index.t2$H2,type="b")



