## ----my_opts, cache=FALSE, include=FALSE---------------------------------
library(knitr) 
knit_hooks$set(small.mar = function(before, options, envir) {
    if (before) par(mar = c(4, 4, .1, .1))  # smaller margin on top and right
})
opts_chunk$set(fig.align="center", fig.height=5.5, fig.width=6.75, collapse=TRUE, comment="", prompt=TRUE, small.mar=TRUE)
options(width=63)
library(ggplot2)
theme_set(theme_bw())
set.seed(201)

## ---- echo=FALSE---------------------------------------------------------
x <- 0:10
z <- data.frame(x=x, y=dbinom(x, prob=0.4, size=10))
ggplot(z, aes(x=x, y=y)) +
  geom_segment(aes(xend = x, yend = 0), size = 1.5, lineend = "round",
               color="lightgray") + 
  geom_point(size=2) + 
  scale_x_continuous(breaks=0:10) +
  labs(x="x", y="f(x)")

## ---- echo=FALSE---------------------------------------------------------
df <- data.frame(x=0:9, y=pbinom(0:9, prob=0.4, size=10), 
                 xo=1:10, yo=pbinom(0:9, prob=0.4, size=10))
ggplot(data=df) +
  geom_segment(aes(x=x, y=y, xend = xo, yend = yo), size=1.5) +
  geom_point(aes(x=xo, y=yo), color="white", size=2) +
  geom_point(aes(x=xo, y=yo), shape=1, size=2) +
  geom_point(aes(x=x, y=y), size=2) +
  scale_x_continuous(breaks=0:10) +
  labs(x="y", y="F(y)")

## ---- echo=FALSE---------------------------------------------------------
x <- seq(-3.5,3.5,0.01)
z <- data.frame(x=x, y=dnorm(x))
ggplot(z) + geom_line(aes(x=x,y=y), size=1.5) + labs(x="x", y="f(x)")

## ---- echo=FALSE---------------------------------------------------------
x <- seq(-3.5,3.5,0.01)
z <- data.frame(x=x, y=pnorm(x))
ggplot(z) + geom_line(aes(x=x,y=y), size=1.5) + labs(x="y", y="F(y)")

## ---- echo=FALSE---------------------------------------------------------
x <- seq(-3.5,3.5,0.01)
x1 <- seq(-1,2,length.out=length(x))
z <- data.frame(x=x, y=dnorm(x), x1=x1, y1=dnorm(x1))
ggplot(z) + 
  geom_area(aes(x=x1, y=y1), fill="tomato") +
  geom_line(aes(x=x,y=y), size=1.5) + 
  scale_x_continuous(breaks=-3:3) + 
  labs(x="x", y="f(x)", title=expression(Pr(-1<=~X<=~2)))

## ---- echo=FALSE---------------------------------------------------------
x <- 1:10
z <- data.frame(x=x, y=rep(1/length(x), length(x)))
ggplot(z, aes(x=x, y=y)) +
  geom_segment(aes(xend = x, yend = 0), size = 1.5, lineend = "round",
               color="lightgray") + 
  geom_point(size=2) + 
  scale_x_continuous(breaks=0:10) +
  labs(x="x", y="f(x)")

## ------------------------------------------------------------------------
n <- 20L
sample(x=1:n, size=10, replace=TRUE)

x <- sample(x=1:n, size=1e6, replace=TRUE)
mean(x) - (n+1)/2
var(x) - (n^2-1)/12

## ---- echo=FALSE---------------------------------------------------------
x <- 0:10
z <- data.frame(x=x, y=dbinom(x, prob=0.4, size=10))
ggplot(z, aes(x=x, y=y)) +
  geom_segment(aes(xend = x, yend = 0), size = 1.5, lineend = "round",
               color="lightgray") + 
  geom_point(size=2) + 
  scale_x_continuous(breaks=0:10) +
  labs(x="x", y="f(x)", title=expression(paste(italic(n) == 10, ", ", italic(p) == 0.4)))

## ------------------------------------------------------------------------
str(dbinom)

## ------------------------------------------------------------------------
str(pbinom)

## ------------------------------------------------------------------------
str(qbinom)

## ------------------------------------------------------------------------
str(rbinom)

## ---- echo=FALSE---------------------------------------------------------
x <- 0:20
z <- data.frame(x=x, y=dpois(x, lambda=6))
ggplot(z, aes(x=x, y=y)) +
  geom_segment(aes(xend = x, yend = 0), size = 1.5, lineend = "round",
               color="lightgray") + 
  geom_point(size=2) + 
  scale_x_continuous(breaks=seq(0,20,4)) +
  labs(x="x", y="f(x)", title=expression(lambda == 6))

## ------------------------------------------------------------------------
str(dpois)

## ------------------------------------------------------------------------
str(ppois)

## ------------------------------------------------------------------------
str(qpois)

## ------------------------------------------------------------------------
str(rpois)

## ---- echo=FALSE---------------------------------------------------------
x <- seq(0,1,0.01)
z <- data.frame(x=x, y=dunif(x))
ggplot(data=z) +
  geom_line(aes(x=x, y=y), size=1.5) + 
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  labs(x="x", y="f(x)")

## ------------------------------------------------------------------------
str(dunif)

## ------------------------------------------------------------------------
str(punif)

## ------------------------------------------------------------------------
str(qunif)

## ------------------------------------------------------------------------
str(runif)

## ---- echo=FALSE---------------------------------------------------------
x <- seq(0,3,0.01)
z <- data.frame(x=x, y=dexp(x, rate=2))
ggplot(data=z) +
  geom_line(aes(x=x, y=y), size=1.5) + 
  labs(x="x", y="f(x)", title=expression(lambda == 2))

## ------------------------------------------------------------------------
str(dexp)

## ------------------------------------------------------------------------
str(pexp)

## ------------------------------------------------------------------------
str(qexp)

## ------------------------------------------------------------------------
str(rexp)

## ---- echo=FALSE---------------------------------------------------------
x <- seq(-3.5, 3.5, 0.01)
z <- data.frame(x=x, y=dnorm(x))
ggplot(data=z) +
  geom_line(aes(x=x, y=y), size=1.5) + 
  labs(x="x", y="f(x)", title=expression(paste(mu == 0, ", ", sigma^2 == 1)))

## ------------------------------------------------------------------------
str(dnorm) #notice it requires the STANDARD DEVIATION, not the variance

## ------------------------------------------------------------------------
str(pnorm)

## ------------------------------------------------------------------------
str(qnorm)

## ------------------------------------------------------------------------
str(rnorm)

## ------------------------------------------------------------------------
x <- replicate(n=1e4, expr=rpois(n=40, lambda=6), 
               simplify="matrix")
x_bar <- apply(x, 2, mean)
clt <- sqrt(40)*(x_bar - 6)

df <- data.frame(clt=clt, x = seq(-18,18,length.out=1e4), 
                 y = dnorm(seq(-18,18,length.out=1e4), 
                           sd=sqrt(6)))

## ------------------------------------------------------------------------
ggplot(data=df) +
  geom_histogram(aes(x=clt, y=..density..), color="blue", 
                 fill="lightgray", binwidth=0.75) +
  geom_line(aes(x=x, y=y), size=1.5)

## ------------------------------------------------------------------------
x <- replicate(n=1e4, expr=rbinom(1, size=20, prob=0.5))
sim_p_hat <- x/20
my_p_hat <- 16/20

## ---- echo=FALSE---------------------------------------------------------
df <- data.frame(x=sim_p_hat)
ggplot(data=df, aes(x=x)) + 
  geom_histogram(color="blue", fill="lightgray", binwidth=0.05) + 
  geom_vline(xintercept=my_p_hat, size=1.5) +
  labs(title="Histogram of Sampling Distribution") +
  geom_text(aes(my_p_hat, 1500, label="p_hat"), nudge_x=0.05)

## ------------------------------------------------------------------------
sessionInfo()

## ----converttonotes, include=FALSE, cache=FALSE--------------------------
source("../customization/make_notes.R")

