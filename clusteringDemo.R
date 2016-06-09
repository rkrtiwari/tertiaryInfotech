## k-means Demo

library(calibrate)

x <- seq(from=1,to=10,length.out = 10)
set.seed(10)
y1 <- rnorm(5, 4, 0.5)
set.seed(20)
y2 <- rnorm(5, 8, 0.5)
y <- c(y1,y2)
par(mfrow=c(1,1))
plot(x,y, cex = 1.5, pch = 19)
obs <- data.frame(x,y)
kmeansObj <-  kmeans(obs, centers = 2)
par(mfrow=c(1,1))
plot(x,y, col= kmeansObj$cluster, pch = 19, cex = 1.5, xlim = c(0,12), ylim = c(0,12))
points(kmeansObj$centers, pch = 8, cex = 2.0, col = rownames(kmeansObj$centers))
textxy(x+0.1,y + 0.1, labs = 1:10, cex = 1, col = kmeansObj$cluster)

kmeansObj <-  kmeans(obs, centers = 4)
par(mfrow=c(1,1))
plot(x,y, col= kmeansObj$cluster, pch = 19, cex = 1.5, xlim = c(0,12), ylim = c(0,12))
points(kmeansObj$centers, pch = 8, cex = 2.0, col = rownames(kmeansObj$centers))
textxy(x+0.1,y + 0.1, labs = 1:10, cex = 1, col = kmeansObj$cluster)

## Hierarchical Clustering

x <- seq(from=1,to=10,length.out = 10)
set.seed(10)
y1 <- rnorm(5, 4, 0.5)
set.seed(20)
y2 <- rnorm(5, 8, 0.5)
y <- c(y1,y2)
par(mfrow=c(1,1))
plot(x,y, xlim = c(0,10), ylim = c(0,10))
obs <- data.frame(x,y)
distM <- dist(obs)
clusters <- hclust(distM, method = "ward.D")

# Here, I have created 2 partitions
par(mfrow=c(1,2))
mem <- cutree(clusters, k =2)
plot(clusters)
abline(h=10)
text(5, 12, labels = "2 partitions")
plot(x,y, col=mem, pch = 19, cex = 1.5, xlim = c(0,12), ylim=c(0,12),
     main="Same color points form a group")
textxy(x+0.1,y + 0.1, labs = 1:10, cex = 1, col = mem)

## This time I am creating 4 partitions
par(mfrow=c(1,2))
mem <- cutree(clusters, k =4)
plot(clusters)
abline(h=3.5)
text(5, 5, labels = "4 partitions")
plot(x,y, col=mem, pch = 19, cex = 1.5, xlim = c(0,12), ylim=c(0,12),
     main="Same color points form a group")
textxy(x+0.1,y + 0.1, labs = 1:10, cex = 1, col = mem)

