############################################################################
####
#### R script for Fujita (2019)
####
#### Functions
#### 2019.11.04 Fujita
#### R 3.6.0
#### Set working directory of 'MTS' folder -- setwd('~/Desktop/Microbiome_TimeSeries/MTS')
############################################################################

for.parallel <- function(cores=1){
	require(doParallel)
	
	cluster = makeCluster(cores,'FORK') 
	return(registerDoParallel(cluster))
	
}

make.dir <- function (file.name) 
{
    save.dir <- file.name
    save.rdata <- sprintf("%s/RData", save.dir)
    save.fig <- sprintf("%s/Figure", save.dir)
    save.rds <- sprintf("%s/RDS", save.dir)
    save.table <- sprintf("%s/Table", save.dir)
    save.session <- sprintf("%s/session", save.dir)
    dir.create(save.dir, showWarnings = FALSE)
    dir.create(save.rdata, showWarnings = FALSE)
    dir.create(save.fig, showWarnings = FALSE)
    dir.create(save.rds, showWarnings = FALSE)
    dir.create(save.table, showWarnings = FALSE)
    dir.create(save.session, showWarnings = FALSE)
    list(dir = save.dir, rdatadir = save.rdata, rdsdir = save.rds, 
         figdir = save.fig, tabledir = save.table)
}

Session <- function(dir){
    
    sink(file=sprintf("%s/session.txt", dir), append=TRUE)
    cat('\n\n## ------------------------------------------------------ ##\n\n')
    cat(sprintf('Working at %s\n\n', Sys.time()) )
    print(sessionInfo())
    sink()
    
}

####
#### F1. Collection of helper functions for DNA extration study
#### 2017.12.1 Ushio
####

# ggplot function 1
PlotStyle <-  function(ggobject){
    return(ggobject + theme_bw() + theme(axis.text.x = element_text(angle=0),
                                         panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(),
                                         axis.text = element_text(size=12),
                                         axis.title = element_text(size=12),
                                         panel.background=element_rect(colour="black", fill=NA, size=0.8)))
}

# ggplot function 2
PlotStyle2 <- function(ggobject){
    return(ggobject + theme(axis.text.x = element_text(angle = 90, hjust = 1),
                            axis.title.x = element_blank(),
                            legend.position = "none") +
               geom_jitter(shape = 16, size = 2, alpha = 0.8, width = 0.1, height = 0))
}

# Merge standard DNA sequences
MergeSTD <- function(std.i, std.data = std.table){
    index.std <- which(match(colnames(std.table), std.i) == 1)
    if(length(index.std) > 1){
        std.tmp <- rowSums(std.table[,index.std])
    }else{
        std.tmp <- std.table[,index.std]
    }
    return(std.tmp)
}

############################################################################
# -- Interpolate missing value. Here is 2 ways
# 1. Mean of previous and former missing value 
# 2. Linear interpolation
Interpotion <- function(data,method){
    
    if(method=='mean'){
        n <- which( is.na(data[,1]) )
        
        temp <- cbind(n-1,n,n+1, data[n-1,1], data[n,7], data[n+1,1])
        
        f <- (temp[which(!is.na(temp[,4])),1])
        l <- (temp[which(!is.na(temp[,6])),3])
        
        for(i in 1:length(f)){
            int <- colMeans(rbind(data[f[i],], data[l[i],]))
            if((f[i]+1) != nrow(data)){
                for(m in (f[i]+1):(l[i]-1)) {  data[m,] <- int }
            }else{
                data[(f[i]+1),] <- data[f[i],]
            }
        }
        
    }
    
    if(method=='linear'){
        data <- na_interpolation(data,option='linear')		
    }
    
    data
}

load.lib <- function (libs, install=FALSE) 
{	
    if(install){
        lib.all <- library()
        lib.name <- lib.all$results
        if(sum(libs %in% lib.name )!=length(libs)){
            
            installed <- which(libs %in% lib.name)
            uninstalled <- libs[-installed]
            
            invisible(lapply(uninstalled, function(x) invisible(install.packages(package = x))))
        }
    }
    invisible(lapply(libs, function(x) invisible(library(package = x, 
                                                         character.only = TRUE))))
    invisible(sapply(libs, function(x) cat(sprintf("%s %s\n", 
                                                   x, packageVersion(x)))))
}

############################################################################

scientific_notation <- function(x) {
     x <- format(x, scientific = TRUE)
     x <- gsub("^(.*)e", "'\\1'e", x)
     x <- gsub("e", "%*%10^", x)
     x <- gsub('\\+', '', x)
     parse(text = x)
}

# For  sample which standard deviation = 0
scale_2 <- function(x){
  if(sd(x, na.rm = T) == 0){
    x[!is.na(x)] <- 0
    return(x)
    }else{
    return(as.numeric(scale(x)))
  }
}

# Subset each treatment
Subset <- function(data,info,row){ 
		
	k <- colnames(info) # get colnames to subset each treatment
	for(i in 1:length(k)) { data <- data[which(data[,k[i]] == info[row,k[i]]),] }
	data
	
}

# Make each taxnomy matrix
Taxa.mat <- function(x,y, taxaLabel){
	
	Taxa.ID <- colnames(x)
	matTT <- t(x)
	mergeMat<- cbind(y[as.matrix(rownames(matTT)),], matTT)

	TaxaList <- unique(mergeMat[, taxaLabel])
	mat.Genus <- matrix(NA, nrow=length(TaxaList), ncol=nrow(x))
	rownames(mat.Genus) <- TaxaList
	colnames(mat.Genus) <- rownames(x)
    
    for (i in 1:length(TaxaList)) {
    if(length(which(mergeMat[,taxaLabel] == TaxaList[i]))!=1){
      mat.Genus[i,] <- colSums(matTT[which(mergeMat[,taxaLabel] == TaxaList[i]),])
      }else{
      	mat.Genus[i,] <- matTT[which(mergeMat[,taxaLabel] == TaxaList[i]),]
      }
    }
	mat.Genus
}

# -- Extract legend
take_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)}
  
  
 
# -- bow-tie structure

bow.tie <- function(adj=x){ # adj = abs(adj)
	
	#################################################################
	# 
	# 
	# 
	# 
	# 
	#################################################################
	
	graph <- graph.adjacency(adj, mode='directed', weighted=TRUE)
	
	# -- Core nodes (stongly connected components)
	comp <- components(graph, mode =  "strong")
	SCC = names(which( comp$membership == order(comp$csize,decreasing=TRUE)[1] ))
	
	no.SCC <- setdiff(V(graph)$name,SCC)
	
	# -- Find reachable nodes from periphery 'In' to SCC
	min.path <- shortest.paths(graph,m="out") ; min.path[min.path==Inf] <- 0
	min.path[min.path==Inf] <- 0
	
	# nodes reach to SCC
	if( is.null(nrow(min.path[no.SCC, SCC])) ){
		to.scc <- names(which( min.path[no.SCC, SCC]>0 ))
	}else{
		to.scc <- names(which(rowSums(min.path[no.SCC, SCC])>0))
	}
	
	if( is.null(nrow(min.path[SCC, no.SCC])) ){
		from.scc <- names(which( min.path[SCC, no.SCC]>0 ))
	}else{
		from.scc <- names(which(colSums(min.path[SCC, no.SCC])>0))
	}
	if(length(from.scc)==0) from.scc<- to.scc	
	In <- setdiff(to.scc,from.scc)
	
	# -- Find reachable nodes from SCC to periphery 'Out'
	if( is.null(nrow(min.path[from.scc, SCC])) ){
		Out <- from.scc
	}else{
		Out<- names(which(rowSums(min.path[from.scc, SCC])==0))
	}
	
	# -- Others
	if(is.null(nrow(min.path[In, setdiff(no.SCC, c(In,Out))]))){
		Tube <- NA
	} else{ 
		if(is.null(nrow(min.path[In, setdiff(no.SCC, c(In,Out))])) | sum(min.path[In, setdiff(no.SCC, c(In,Out))])==0){
			Tube <- NA
		}else{
			in.tube <- names(which(colSums(min.path[In, setdiff(no.SCC, c(In,Out))])>0))		
			
			if( is.matrix(min.path[in.tube, Out]) )	{
				Tube <- names(which(rowSums(min.path[in.tube, Out])>0))
			}else{
				Tube <- NA			
			}
			
		}
	}
	
	if(is.null(nrow(min.path[In, setdiff(no.SCC, c(In))]))){
		indril.tmp <- names(which(min.path[In, setdiff(no.SCC, c(In))]>0))
		INTENDRILS <- indril.tmp[-which(indril.tmp%in%c(Out, Tube))]
	} else{ 
		indril.tmp <- names(which(colSums(min.path[In, setdiff(no.SCC, c(In))])>0))
		INTENDRILS <- indril.tmp[-which(indril.tmp%in%c(Out, Tube))]
	}
	
	if(is.null(nrow(min.path[setdiff(no.SCC, c(Out)),Out]))){
		indril.tmp <- names(which(min.path[setdiff(no.SCC, c(Out)),Out]>0))
		OUTTENDRILS <- indril.tmp[-which(indril.tmp%in%c(In, Tube))]
	} else{ 				   
		indril.tmp <- names(which(rowSums(min.path[setdiff(no.SCC, c(Out)),Out])>0))
		OUTTENDRILS <- indril.tmp[-which(indril.tmp%in%c(In, Tube))]
	}
	
	OTHERS <- setdiff(V(graph)$name, c(SCC, In, Out, Tube, INTENDRILS, OUTTENDRILS))
	
	# Result
	a <- list(In=ifelse(V(graph)$name%in%In, 1, 0), 
		 	  Out=ifelse(V(graph)$name%in%Out, 2, 0), 
		 	  Core = ifelse(V(graph)$name%in%SCC, 3, 0),
		 	  Tube = ifelse(V(graph)$name%in%Tube, 4, 0),
		 	  INTENDRILS = ifelse(V(graph)$name%in%INTENDRILS, 5, 0),
		 	  OUTTENDRILS = ifelse(V(graph)$name%in%OUTTENDRILS, 6, 0),
		 	  OTHERS = ifelse(V(graph)$name%in% OTHERS, 7, 0)
		 	  ) 
		
	b <- lapply(a, function(x){
				    names(x)<-V(graph)$name
					x} )	
	b[['all']] <- colSums(do.call(rbind,b))
	b
	}
	
	
	
	mAr.est_2 <- function (x, p, ...) { #x=y
    x = as.matrix(x)
    n = dim(x)[1]
    m = dim(x)[2]
    ne = n - p
    np = m * p + 1
    
    if (ne <= np) {
        return(list(SBC = NA, wHat = NA, AHat = NA, CHat = NA, resid = NA))
    }else{
	    K = matrix(nrow = ne, ncol = np + m)
	    K[, 1] = rep(1, ne)
	    for (j in 1:p) {
	        K[, seq(2 + m * (j - 1), 1 + m * j)] = data.matrix(x[seq(p - 
	            j + 1, n - j), 1:m])
	    }
	    K[, seq(np + 1, np + m)] = data.matrix(x[seq(p + 1, n), 1:m])
	    q = ncol(K)
	    delta = (q^2 + q + 1) * (.Machine$double.eps)
	    scale = sqrt(delta) * sqrt(apply(K^2, 2, sum))
	    R = qr.R(qr((rbind(K, diag(scale)))), complete = TRUE)
	    if(length(seq(np + 1, np + m))>1){
	    		R22 = R[seq(np + 1, np + m), seq(np + 1, np + m)]
	    	}else{
	    		R22 = R[seq(1, np + m), seq(1, np + m)]
	    }
	    logdp = 2 * log(abs(prod(diag(R22))))
	    sbc = logdp/m - log(ne) * (ne - np)/ne
	    R11 = R[seq(1, np), seq(1, np)]
	    R12 = R[seq(1, np), seq(np + 1, np + m)]
	    if(length(seq(np + 1, np + m))>1){
	    		R22 = R[seq(np + 1, np + m), seq(np + 1, np + m)]
	    	}else{
	    		R22 = R[seq(1, np + m), seq(1, np + m)]
	    }
	    R11[, 1] = R11[, 1] * max(scale[2:(np + m)])/scale[1]
	    B = t(solve(R11) %*% R12)
	    w = B[, 1] * max(scale[2:(np + m)])/scale[1]
	    A = B[, 2:np]
	    C = (t(R22) %*% R22)/(ne - np)
	    t = seq(1, (n - p))
	    res = matrix(nrow = (n - p), ncol = m)
	    res[t, seq(1, m)] = x[t + p, ] - (rep(1, n - p) %*% t(w))
	    for (j in seq(1, p)) {
	    	if(m>1){
	        res[t, seq(1, m)] = res[t, seq(1, m)] - (x[(t - j + p), 
	            ] %*% t(A[, seq(m * j - m + 1, j * m)]))}else{
	         res[t, seq(1, m)] = res[t, seq(1, m)] - (x[(t - j + p), 
	            ] %*% t(A[ seq(m * j - m + 1, j * m)]))
	            }
	    }
	    return(list(SBC = sbc, wHat = w, AHat = A, CHat = C, resid = res))
	}
}
 
 
##################
# -- infulence by Suzuki

## functions
nm <- function(x, z){
    x.z = cbind(abs(x),z)
    x.sum = tapply(x.z[,1], x.z[,2], sum)[x.z[,2]]
    xm = x.z[,1] / as.numeric(x.sum)
    xm}

influence <- function(mat, mode){
    l=nrow(mat)

    if (mode=='all') {
        matz <- abs(mat)
    } else if (mode=='positive') {
        mat[mat<0] <- 0
        matz <- mat
    } else if (mode=='negative') {
        mat[mat>0] <- 0
        matz <- abs(mat)
    } else {
        print("mode: Only can use \"all\", \"positive\" or \"negative\"")
    }

    I <- diag(l)
    matz=matz+0.001*I
    mm=matz
    mm[mm>0] <- 1
    g=graph_from_adjacency_matrix(mm)
    mem=components(g, mode = "weak")[['membership']]

    x.z = cbind(rep(1,length(mem)), mem)
    mnum = as.numeric(tapply(x.z[,1], x.z[,2], sum)[x.z[,2]])
    mnum[mnum==1] <- 0
    mnum[mnum!=0] <- 1

    vi <- matrix(nm(rnorm(l), mem))

    ki <- colSums(matz) ##
    ma <- matz/ki-I

    ndv = Inf

    t=0
    nmon <-c()
    while (ndv>0.01 && t <5000) {
    t = t + 1
    dv = ma %*% vi
    ndv = norm(dv*mnum)
    nmon = c(nmon, ndv)
    vi = nm(vi + 0.01 * dv, mem)
    }

    if (ndv>0.01) {print("Convergence failure: vi may incorrect")}
    
    return(matrix(c(vi, mem), l))
}

##################################################