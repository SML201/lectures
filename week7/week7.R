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

## ----clt_demo, cache=TRUE------------------------------------------------
x <- replicate(n=1e4, expr=rpois(n=40, lambda=6), 
               simplify="matrix")
x_bar <- apply(x, 2, mean)
clt <- sqrt(40)*(x_bar - 6)

df <- data.frame(clt=clt, x = seq(-18,18,length.out=1e4), 
                 y = dnorm(seq(-18,18,length.out=1e4), 
                           sd=sqrt(6)))

## ----clt_plot, cache=TRUE, dependson="clt_demo"--------------------------
ggplot(data=df) +
  geom_histogram(aes(x=clt, y=..density..), color="blue", 
                 fill="lightgray", binwidth=0.75) +
  geom_line(aes(x=x, y=y), size=1.5)

## ----sampling_demo, cache=TRUE-------------------------------------------
x <- replicate(n=1e4, expr=rbinom(1, size=20, prob=0.5))
sim_p_hat <- x/20
my_p_hat <- 16/20

## ----sampling_plot, cache=TRUE, echo=FALSE-------------------------------
df <- data.frame(x=sim_p_hat)
ggplot(data=df, aes(x=x)) + 
  geom_histogram(color="blue", fill="lightgray", binwidth=0.05) + 
  geom_vline(xintercept=my_p_hat, size=1.5) +
  labs(title="Histogram of Sampling Distribution") +
  geom_text(aes(my_p_hat, 1500, label="p_hat"), nudge_x=0.05)

## ------------------------------------------------------------------------
mu <- 5
n <- 20
x <- replicate(10000, rnorm(n=n, mean=mu)) # 10000 studies
m <- apply(x, 2, mean) # the estimate for each study
ci <- cbind(m - 1.96/sqrt(n), m + 1.96/sqrt(n))
head(ci)

## ------------------------------------------------------------------------
cover <- (mu > ci[,1]) & (mu < ci[,2])
mean(cover)

## ------------------------------------------------------------------------
qnorm(0.025)
qnorm(0.975)

## ----sampling_demo_2, cache=TRUE-----------------------------------------
x <- replicate(n=1e4, expr=rbinom(1, size=20, prob=0.5))
sim_p_hat <- x/20
my_p_hat <- 16/20

## ----sampling_plot_2, cache=TRUE, echo=FALSE, dependson="sampling_demo_2"----
df <- data.frame(sim_p_hat=sim_p_hat)
ggplot(data=df, aes(x=sim_p_hat)) + 
  geom_histogram(color="blue", fill="lightgray", binwidth=0.05) + 
  geom_vline(xintercept=my_p_hat, size=1.5) +
  labs(title="Histogram of Null Distribution Draws") +
  geom_text(aes(my_p_hat, 1500, label="obs. p_hat"), nudge_x=0.075)

## ---- cache=TRUE, dependson="sampling_demo_2"----------------------------
sum(abs(sim_p_hat-0.5) >= abs(my_p_hat-0.5))/1e4

## ------------------------------------------------------------------------
sessionInfo()

## ----converttonotes, include=FALSE, cache=FALSE--------------------------
source("../customization/make_notes.R")

