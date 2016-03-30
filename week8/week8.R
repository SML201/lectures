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

## ----clt_std_demo, cache=TRUE--------------------------------------------
x <- replicate(n=1e4, expr=rpois(n=40, lambda=6), 
               simplify="matrix")
x_bar <- apply(x, 2, mean)
clt_std <- (x_bar - 6)/(sqrt(6)/sqrt(40))

df <- data.frame(z=clt_std, x = seq(-3.5,3.5,length.out=1e4), 
                 y = dnorm(seq(-3.5,3.5,length.out=1e4)))
# note that df$y are Normal(0,1) pdf values

## ----clt_std_plot, cache=TRUE, dependson="clt_std_demo"------------------
ggplot(data=df) +
  geom_histogram(aes(x=z, y=..density..), color="blue", 
                 fill="lightgray", binwidth=0.3) +
  geom_line(aes(x=x, y=y), size=1.5)

## ---- eval=FALSE---------------------------------------------------------
## install.packages("BSDA")

## ---- message=FALSE------------------------------------------------------
library(BSDA)
str(z.test)

## ---- display=FALSE------------------------------------------------------
set.seed(210)

## ------------------------------------------------------------------------
n <- 40
lam <- 14
x <- rpois(n=n, lambda=lam)
lam.hat <- mean(x)
stddev <- sqrt(lam.hat)
z.test(x=x, sigma.x=stddev, mu=lam)

## ------------------------------------------------------------------------
lam.hat <- mean(x)
lam.hat
stderr <- sqrt(lam.hat)/sqrt(n)
lam.hat - abs(qnorm(0.025)) * stderr # lower bound
lam.hat + abs(qnorm(0.025)) * stderr # upper bound

## ------------------------------------------------------------------------
z <- (lam.hat - lam)/stderr
z # test statistic
2 * pnorm(-abs(z)) # two-sided p-value

## ---- echo=FALSE, fig.height=6.5, fig.width=9----------------------------
x <- seq(-3,3,0.01)
k <- length(x)
y <- c(dt(x, df=4), dt(x, df=30), dnorm(x))
df <- data.frame(x=c(x, x, x), y=y, distribution=c(rep("t, df=4", k), rep("t, df=30", k), rep("Normal", k)))
ggplot(data=df) + 
  geom_line(aes(x=x, y=y, color=distribution), size=1.2) +
  labs(x="x", y="f(x)")

## ------------------------------------------------------------------------
qt(0.025, df=4) # alpha = 0.025
qt(0.05, df=4)
qt(0.95, df=4)
qt(0.975, df=4)

## ---- message=FALSE------------------------------------------------------
library("dplyr")
library("ggplot2")
theme_set(theme_bw())
library("broom")

## ------------------------------------------------------------------------
library("car")
data("Davis")

## ------------------------------------------------------------------------
htwt <- tbl_df(Davis)
htwt

## ------------------------------------------------------------------------
ggplot(htwt) + 
geom_point(aes(x=height, y=weight, color=sex), size=2, alpha=0.5) +
scale_colour_manual(values=c("red", "blue"))

## ------------------------------------------------------------------------
which(htwt$height < 100)
htwt[12,]

## ------------------------------------------------------------------------
htwt[12,c(2,3)] <- htwt[12,c(3,2)]

## ------------------------------------------------------------------------
ggplot(htwt) + 
  geom_point(aes(x=height, y=weight, color=sex), size=2, alpha=0.5) +
  scale_color_manual(values=c("red", "blue"))

## ------------------------------------------------------------------------
ggplot(htwt) + 
  geom_density(aes(x=height, color=sex), size=1.5) + 
  scale_color_manual(values=c("red", "blue"))

## ------------------------------------------------------------------------
ggplot(htwt) + 
  geom_density(aes(x=weight, color=sex), size=1.5) + 
  scale_color_manual(values=c("red", "blue"))

## ------------------------------------------------------------------------
m_ht <- htwt %>% filter(sex=="M") %>% select(height)
testresult <- t.test(x = m_ht$height, mu=177)

## ------------------------------------------------------------------------
class(testresult)
is.list(testresult)

## ------------------------------------------------------------------------
names(testresult)
testresult

## ------------------------------------------------------------------------
library(broom)
tidy(testresult)

## ------------------------------------------------------------------------
f_ht <- htwt %>% filter(sex=="F") %>% select(height)
t.test(x = f_ht$height, mu = 164)

## ------------------------------------------------------------------------
t.test(x = m_ht$height, y = f_ht$height)

## ------------------------------------------------------------------------
htwt %>% group_by(sex) %>% summarize(sd(height))
t.test(x = m_ht$height, y = f_ht$height, var.equal = TRUE)

## ------------------------------------------------------------------------
htwt <- htwt %>% mutate(diffwt = (weight - repwt), 
                        diffht = (height - repht))
t.test(x = htwt$diffwt) %>% tidy()
t.test(x = htwt$diffht) %>% tidy()

## ------------------------------------------------------------------------
t.test(x=htwt$weight, y=htwt$repwt, paired=TRUE) %>% tidy()
t.test(x=htwt$height, y=htwt$repht, paired=TRUE) %>% tidy()
htwt %>% select(height, repht) %>% na.omit() %>% 
  summarize(mean(height), mean(repht))

## ------------------------------------------------------------------------
str(binom.test)
binom.test(x=16, n=20, p = 0.5)

## ------------------------------------------------------------------------
binom.test(x=16, n=20, p = 0.5, alternative="greater")

## ------------------------------------------------------------------------
binom.test(x=16, n=20, p = 0.5, alternative="less")

## ------------------------------------------------------------------------
str(prop.test)
prop.test(x=16, n=20, p=0.5)

## ------------------------------------------------------------------------
p <- binom.test(x=16, n=20, p = 0.5)$p.value
binom.test(x=16, n=20, p = 0.5, conf.level=(1-p))

## ------------------------------------------------------------------------
x <- round(c(0.47*771, 0.34*732))
n <- round(c(771*0.97, 732*0.97))
prop.test(x=x, n=n, conf.level=0.90)

## ------------------------------------------------------------------------
p1.hat <- 0.47
n1 <- 771
p2.hat <- 0.34
n2 <- 732
stderr <- sqrt(p1.hat*(1-p1.hat)/n1 + p2.hat*(1-p2.hat)/n2)

# the 90% CI
(p1.hat - p2.hat) + c(-1,1)*abs(qnorm(0.05))*stderr

## ------------------------------------------------------------------------
str(poisson.test)

## ------------------------------------------------------------------------
healthy <- c(82, 64, 66, 88, 65, 81, 85, 87, 60, 79, 80, 72)
cancer <- c(59, 50, 60, 60, 78, 69, 70, 67, 72, 66, 66, 68, 
            54, 62)

## ------------------------------------------------------------------------
poisson.test(x=c(sum(healthy), sum(cancer)), T=c(12,14), 
             conf.level=0.99)

## ------------------------------------------------------------------------
poisson.test(x=c(sum(healthy), sum(cancer)), T=c(12,14), 
             alternative="less", conf.level=0.99)

## ------------------------------------------------------------------------
poisson.test(x=c(sum(healthy), sum(cancer)), T=c(12,14), 
             alternative="greater", conf.level=0.99)

## ------------------------------------------------------------------------
sessionInfo()

## ----converttonotes, include=FALSE, cache=FALSE--------------------------
source("../customization/make_notes.R")

