## ----my_opts, cache=FALSE, include=FALSE---------------------------------
library(knitr)
knit_hooks$set(small.mar = function(before, options, envir) {
    if (before) par(mar = c(4, 4, .1, .1))  # smaller margin on top and right
})
opts_chunk$set(fig.align="center", fig.height=5.5, fig.width=6.75, collapse=TRUE, comment="", prompt=TRUE, small.mar=TRUE)
options(width=63)
library("ggplot2")
theme_set(theme_bw())
library("dplyr")
library("broom")
set.seed(201)

## ---- echo=FALSE---------------------------------------------------------
set.seed(201)
x <- c(rnorm(50, mean=2, sd=0.3), rnorm(50, mean=2, sd=0.3), rnorm(50, mean=4, sd=0.3))
y <- c(rnorm(50, mean=2, sd=0.3), rnorm(50, mean=4, sd=0.3), rnorm(50, mean=3, sd=0.3))
data1 <- data.frame(x=x, y=y, true_clusters=factor(c(rep(1,50), rep(2, 50), rep(3, 50))))
ggplot(data1) + geom_point(aes(x=x,y=y), size=2)

## ---- echo=FALSE---------------------------------------------------------
ggplot(data1) + geom_point(aes(x=x,y=y, col=true_clusters), size=2) +
  scale_color_manual(values=c("red", "blue", "gray47"))

## ---- echo=FALSE---------------------------------------------------------
set.seed(201)
x <- c(rnorm(60, mean=2, sd=0.3), rnorm(80, mean=4, sd=0.3))
y <- c(rnorm(60, mean=2, sd=0.3), rnorm(80, mean=runif(80, min=1, max=3), sd=0.3))
data2 <- data.frame(x=x, y=y, true_clusters=factor(c(rep(1,60), rep(2, 80))))
ggplot(data2) + geom_point(aes(x=x,y=y), size=2)

## ---- echo=FALSE---------------------------------------------------------
ggplot(data2) + geom_point(aes(x=x,y=y, col=true_clusters), size=2) +
  scale_color_manual(values=c("red", "blue"))

## ------------------------------------------------------------------------
str(dist)

## ------------------------------------------------------------------------
sub_data1 <- data1[1:4, c(1,2)]
sub_data1
mydist <- dist(sub_data1)
print(mydist)

## ------------------------------------------------------------------------
(sub_data1[1,] - sub_data1[2,])^2 %>% sum() %>% sqrt()

## ------------------------------------------------------------------------
str(hclust)

## ---- echo=FALSE---------------------------------------------------------
ggplot(data1) + geom_point(aes(x=x,y=y), size=2)

## ------------------------------------------------------------------------
mydist <- dist(data1, method = "euclidean")
myhclust <- hclust(mydist, method="complete")
plot(myhclust)

## ------------------------------------------------------------------------
plot(as.dendrogram(myhclust))

## ---- message=FALSE, warning=FALSE---------------------------------------
library(dendextend)
dend1 <- as.dendrogram(myhclust)
labels(dend1) <- data1$true_clusters
labels_colors(dend1) <- 
  c("red", "blue", "gray47")[as.numeric(data1$true_clusters)]
plot(dend1, axes=FALSE, main=" ", xlab=" ")

## ------------------------------------------------------------------------
dend2 <- as.dendrogram(myhclust)
labels(dend2) <- rep(" ", nrow(data1))
dend2 <- color_branches(dend2, k = 3, col=c("red", "blue", "gray47"))
plot(dend2, axes=FALSE, main=" ", xlab=" ")

## ------------------------------------------------------------------------
est_clusters <- cutree(myhclust, k=3)
est_clusters

## ---- eval=FALSE---------------------------------------------------------
## est_clusters <- factor(est_clusters)
## p <- data1 %>%
##   mutate(est_clusters=est_clusters) %>%
##   ggplot()
## p + geom_point(aes(x=x, y=y, color=est_clusters))

## ---- echo=FALSE---------------------------------------------------------
rm(est_clusters)
p <- data1 %>% 
  mutate(est_clusters=factor(cutree(myhclust, k=3))) %>% 
  ggplot()
p + geom_point(aes(x=x, y=y, color=est_clusters))

## ------------------------------------------------------------------------
(data1 %>% 
   mutate(est_clusters=factor(cutree(myhclust, k=2))) %>% 
   ggplot()) + geom_point(aes(x=x, y=y, color=est_clusters))

## ------------------------------------------------------------------------
(data1 %>% 
   mutate(est_clusters=factor(cutree(myhclust, k=4))) %>% 
   ggplot()) + geom_point(aes(x=x, y=y, color=est_clusters))

## ------------------------------------------------------------------------
(data1 %>% 
   mutate(est_clusters=factor(cutree(myhclust, k=6))) %>% 
   ggplot()) + geom_point(aes(x=x, y=y, color=est_clusters))

## ------------------------------------------------------------------------
data1 %>% dist() %>% hclust(method="complete") %>% 
  as.dendrogram() %>% plot(axes=FALSE)

## ------------------------------------------------------------------------
data1 %>% dist() %>% hclust(method="average") %>% 
  as.dendrogram() %>% plot(axes=FALSE)

## ------------------------------------------------------------------------
data1 %>% dist() %>% hclust(method="single") %>% 
  as.dendrogram() %>% plot(axes=FALSE)

## ------------------------------------------------------------------------
data1 %>% dist() %>% hclust(method="ward.D") %>% 
  as.dendrogram() %>% plot(axes=FALSE)

## ---- echo=FALSE---------------------------------------------------------
ggplot(data2) + geom_point(aes(x=x,y=y), size=2)

## ------------------------------------------------------------------------
mydist <- dist(data2, method = "euclidean")
myhclust <- hclust(mydist, method="complete")
plot(as.dendrogram(myhclust))

## ---- message=FALSE, warning=FALSE---------------------------------------
library(dendextend)
dend1 <- as.dendrogram(myhclust)
labels(dend1) <- data2$true_clusters
labels_colors(dend1) <- 
  c("red", "blue")[as.numeric(data2$true_clusters)]
plot(dend1, axes=FALSE, main=" ", xlab=" ")

## ------------------------------------------------------------------------
dend2 <- as.dendrogram(myhclust)
labels(dend2) <- rep(" ", nrow(data2))
dend2 <- color_branches(dend2, k = 2, col=c("red", "blue"))
plot(dend2, axes=FALSE, main=" ", xlab=" ")

## ------------------------------------------------------------------------
(data2 %>% 
   mutate(est_clusters=factor(cutree(myhclust, k=2))) %>% 
   ggplot()) + geom_point(aes(x=x, y=y, color=est_clusters))

## ------------------------------------------------------------------------
(data2 %>% 
   mutate(est_clusters=factor(cutree(myhclust, k=3))) %>% 
   ggplot()) + geom_point(aes(x=x, y=y, color=est_clusters))

## ------------------------------------------------------------------------
(data2 %>% 
   mutate(est_clusters=factor(cutree(myhclust, k=4))) %>% 
   ggplot()) + geom_point(aes(x=x, y=y, color=est_clusters))

## ------------------------------------------------------------------------
(data2 %>% 
   mutate(est_clusters=factor(cutree(myhclust, k=6))) %>% 
   ggplot()) + geom_point(aes(x=x, y=y, color=est_clusters))

## ------------------------------------------------------------------------
str(kmeans)

## ------------------------------------------------------------------------
km1 <- kmeans(x=data1[,-3], centers=3, iter.max=100, nstart=5)
est_clusters <- fitted(km1, method="classes")
est_clusters

## ------------------------------------------------------------------------
centroids1 <- fitted(km1, method="centers") %>% unique()
centroids1

## ------------------------------------------------------------------------
est_clusters <- fitted(km1, method="classes")
data1 %>% mutate(est_clusters = factor(est_clusters)) %>% 
  group_by(est_clusters) %>% summarize(mean(x), mean(y))

## ------------------------------------------------------------------------
est_clusters <- factor(est_clusters)
ggplot(data1) + geom_point(aes(x=x, y=y, color=est_clusters))

## ---- echo=FALSE---------------------------------------------------------
rm(est_clusters)
est_clusters <- data1 %>% 
  kmeans(centers=2, iter.max=100, nstart=5) %>%
  fitted(method="classes") %>%
  factor()
ggplot(data1) + geom_point(aes(x=x, y=y, color=est_clusters))

## ---- echo=FALSE---------------------------------------------------------
rm(est_clusters)
est_clusters <- data1 %>% 
  kmeans(centers=6, iter.max=100, nstart=5) %>%
  fitted(method="classes") %>%
  factor()
ggplot(data1) + geom_point(aes(x=x, y=y, color=est_clusters))

## ------------------------------------------------------------------------
km2 <- kmeans(x=data2[,-3], centers=2, iter.max=100, nstart=5)
est_clusters <- fitted(km2, method="classes")
est_clusters

## ------------------------------------------------------------------------
est_clusters <- factor(est_clusters)
ggplot(data2) + geom_point(aes(x=x, y=y, color=est_clusters))

## ---- echo=FALSE---------------------------------------------------------
rm(est_clusters)
est_clusters <- data2 %>% 
  kmeans(centers=3, iter.max=100, nstart=5) %>%
  fitted(method="classes") %>%
  factor()
ggplot(data2) + geom_point(aes(x=x, y=y, color=est_clusters))

## ---- echo=FALSE---------------------------------------------------------
rm(est_clusters)
est_clusters <- data2 %>% 
  kmeans(centers=5, iter.max=100, nstart=5) %>%
  fitted(method="classes") %>%
  factor()
ggplot(data2) + geom_point(aes(x=x, y=y, color=est_clusters))

## ------------------------------------------------------------------------
sessionInfo()

## ----converttonotes, include=FALSE, cache=FALSE--------------------------
source("../customization/make_notes.R")

