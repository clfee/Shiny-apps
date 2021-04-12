rm(list=ls())
library(MASS)
n <- 100
y <- t(mvrnorm(n,c(0,0), matrix(c(1,0.95,0.95,1),2,2)))
s <- svd(y)
round(sqrt(2) * s$u , 3)

library(tissuesGeneExpression)
data(tissuesGeneExpression)

s = svd(e)
signflips = sample(c(-1,1),ncol(e),replace=TRUE)
signflips

newu= sweep(s$u,2,signflips,FUN="*")
newv= sweep(s$v,2,signflips,FUN="*" )
all.equal( s$u %*% diag(s$d) %*% t(s$v), newu %*% diag(s$d) %*% t(newv))

m <- rowMeans(e)
cor(s$u[,1],m)

y = e - rowMeans(e)
z = s$d * t(s$v)
a <- sqrt(crossprod(e[,3]-e[,45]))
#sqrt(crossprod(y[,3]-y[,45]))
b <- sqrt(crossprod(z[1:8,3]-z[1:8,45]))
abs(a-b)

# --- similar to elbow plot
ks = 1:189
realdistance = sqrt(crossprod(e[,3]-e[,45]))
approxdistances = sapply(ks,function(k){
  sqrt(crossprod(z[1:k,3,drop=FALSE]-z[1:k,45,drop=FALSE] )) 
})
percentdiff = 100*abs(approxdistances - realdistance)/realdistance
plot(ks,percentdiff) ##take a look
min(ks[which(percentdiff < 10)])

distances = sqrt(apply(e[,-3]-e[,3],2,crossprod))
b        <- sqrt(apply(z[1:2,-3]-z[1:2,3],2, crossprod))
cor(distances, b, method = 'spearman')
plot(distances,b)

# ---------- MDS -------------
library(tissuesGeneExpression)
data(tissuesGeneExpression)

y = e - rowMeans(e)
s = svd(y)
z = s$d * t(s$v)
tis     <- ! tissue %in% c('lung', 'kidney', 'colon')
ftis    <- factor(tissue[tis])
ftissue <- factor(tissue)

# plot
par(mfrow= c(1,2))
plot(z[1,],z[2,],col=as.numeric(ftissue))
  legend("topleft",levels(ftissue),col=seq_along(ftissue),pch=1)

d = dist(t(e))
mds = cmdscale(d)  
cor(z[1,],mds[,1])
cor(z[2,],mds[,2])
plot(z[1,],z[2,],col=as.numeric(ftissue))
plot(mds[,1],mds[,2],col=as.numeric(ftissue))

# testing the function -
library(data.table)
my_mean <- function(x){
  #x <- data.table(x)
  return(mean(x))
}
ex <- data.table(e)
my_means_ori <- apply(ex[ , .SD, ], 1, my_mean)
#my_means <- apply(ex[ , .SD, .SDcols = colnames(ex)], 1, mean) 
rowmean  <- rowMeans(e)

library(GSE5859Subset)
data(GSE5859Subset)
s = svd(geneExpression-rowMeans(geneExpression))
z = s$d * t(s$v)
# extract month data
month = format( sampleInfo$date, "%m")
month = factor( month)

cor_lst <- c()
for (i in 1: 24){
  a       <- cor(z[i,], as.numeric(month))
  cor_lst <- append(cor_lst,a)
  #return(cor_lst)
}
cor_lst
which.max(cor_lst)

# advanced question
strat <- as.factor(geneAnnotation$CHR)
s$u[,5]
x <- na.omit(data.table(cbind(strat,s$u[,6])))
plot(x$strat,x$V2)

# ---------- HC clustering -----------
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
rm(list=ls())
set.seed(1)
m = 10000
n = 24
large_m <- replicate(100,{
  x = matrix(rnorm(m*n),m,n)
  colnames(x)=1:n
  d <- dist(t(x))
  cl <- hclust(d)
  cut_hc <- cutree(cl, h = 143)
  length(unique(cut_hc))
  })



plot(table(large_m))
sqrt(var(large_m))

# ------ K means ------------
rm(list=ls())
library(GSE5859Subset)
data(GSE5859Subset)

set.seed(10)
d <- dist(t(geneExpression))
mds <- cmdscale(d)

kcluster <- kmeans(geneExpression, centers = 5)
par(mfrow= c(1,2))
plot(mds[,1], mds[,2], col=kcluster$cluster, pch=16)
plot(mds[,1], mds[,2], col=sampleInfo$date, pch=12)

table(sampleInfo$date,kcluster$cluster)[,c(4,1,5,3,2)]

# ---------- heatmap -----------------
rm(list=ls())
library(data.table)
library(RColorBrewer)
library(GSE5859Subset)
data(GSE5859Subset)

dt  <- data.table(geneExpression)
rv  <- apply(dt[ , .SD, .SDcols = colnames(dt)], 1, var)#apply(dt[ , .SD, .SDcols = colnames(dt)], 1, var)
idx <- order(-rv)[1:25] 
hmol <- colorRampPalette(brewer.pal(9,'Greens'))(100)
heatmap(geneExpression[idx,], col = hmol)

# make it more pretty
library(gplots)
a <- as.factor(colnames(geneExpression))
b <- as.numeric(sampleInfo$group) # as.numeric(a)
cols <- colorRampPalette(rev(brewer.pal(11,"RdBu")))(25)

gcol=brewer.pal(3,"Dark2")
gcol=gcol[sampleInfo$g+1]

labcol <- gsub('2005-', '', sampleInfo$date)

heatmap.2(geneExpression[idx,], labCol = labcol,
          labRow = geneAnnotation$CHR[idx],
          trace ="none",
          scale = 'row',
          ColSideColors = gcol,
          col = cols,
          key = FALSE)
