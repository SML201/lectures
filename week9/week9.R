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

## ---- message=FALSE------------------------------------------------------
library("MASS")
data("survey", package="MASS")
survey <- tbl_df(survey)
head(survey)

## ------------------------------------------------------------------------
tbl = table(survey$Sex, survey$W.Hnd)
tbl

## ---- echo=FALSE---------------------------------------------------------
df <- survey %>% dplyr::select(Sex, W.Hnd) %>% na.omit()
ggplot(data=df) + geom_bar(aes(x=Sex, fill=W.Hnd), position="stack") +
  scale_fill_manual(values=c("red", "blue"))

## ------------------------------------------------------------------------
str(chisq.test)

## ------------------------------------------------------------------------
chisq.test(tbl)

## ---- echo=FALSE---------------------------------------------------------
x <- seq(0.001, 15, 0.01)

df <- data.frame(x=c(x,x), y=c(dchisq(x, df=2), dchisq(x, df=3)), 
                 Distribution = c(rep("Chi^2 df=2", length(x)), 
                           rep("Chi^2 df=3", length(x))))

ggplot(data=df) + geom_line(aes(x=x, y=y, color=Distribution)) + 
  scale_color_manual(values=c("red", "blue")) + 
  labs(x="x", y="f(x)")

## ------------------------------------------------------------------------
tbl

## ------------------------------------------------------------------------
n <- sum(tbl)
p <- sum(tbl[1,])/n # freq Female 
q <- sum(tbl[,1])/n # freq Left
expected <- n * matrix(c(p*q, (1-p)*q, p*(1-q), (1-p)*(1-q)), 
                       nrow=2)
expected

## ------------------------------------------------------------------------
X2 <- sum((tbl - expected)^2 / expected)
X2

chisq.test(tbl, correct=FALSE)$statistic # equals X2
chisq.test(tbl)$statistic # with continuity correction

## ------------------------------------------------------------------------
1-pchisq(X2, df=1)

chisq.test(tbl, correct=FALSE)$p.value

## ------------------------------------------------------------------------
tbl = table(survey$Clap, survey$W.Hnd)
tbl

## ------------------------------------------------------------------------
chisq.test(tbl)

## ------------------------------------------------------------------------
chisq.test(tbl, simulate.p.value = TRUE, B=10000)

## ------------------------------------------------------------------------
tbl = table(survey$Exer, survey$W.Hnd)
tbl

chisq.test(tbl, simulate.p.value = TRUE, B=10000)

## ------------------------------------------------------------------------
tbl = table(survey$Smoke, survey$Exer)
tbl

chisq.test(tbl, simulate.p.value = TRUE, B=10000)

## ---- echo=FALSE---------------------------------------------------------
set.seed(201)
die <- sample(1:6, size=100, replace=TRUE)
die <- table(die)

## ------------------------------------------------------------------------
die
chisq.test(x=die, p=rep(1/6, 6))

## ---- warning=FALSE------------------------------------------------------
ggplot(data = survey, mapping=aes(x=Wr.Hnd, y=Height)) +
  geom_point() + geom_vline(xintercept=mean(survey$Wr.Hnd, na.rm=TRUE)) +
  geom_hline(yintercept=mean(survey$Height, na.rm=TRUE))

## ------------------------------------------------------------------------
str(cor)

cor(survey$Wr.Hnd, survey$Height, 
    use="pairwise.complete.obs")

## ------------------------------------------------------------------------
DF <- survey %>% dplyr::select(Wr.Hnd, Height) %>% na.omit()
sum((DF$Wr.Hnd - mean(DF$Wr.Hnd)) *
      (DF$Height - mean(DF$Height))) /
  ((nrow(DF)-1) * sd(DF$Wr.Hnd) * sd(DF$Height))

## ------------------------------------------------------------------------
str(cor.test)

## ------------------------------------------------------------------------
cor.test(x=survey$Wr.Hnd, y=survey$Height)

## ------------------------------------------------------------------------
r <- cor(survey$Wr.Hnd, survey$Height, 
    use="pairwise.complete.obs")
df <- sum(complete.cases(survey[,c("Wr.Hnd", "Height")]))-2
# dplyr way to get df:
# df <- (survey %>% select(Wr.Hnd, Height) %>% 
#        na.omit() %>% nrow())-2 

tstat <- r/sqrt((1 - r^2)/df)
tstat

pvalue <- 2*pt(q=-abs(tstat), df=df)
pvalue

## ---- warning=FALSE------------------------------------------------------
ggplot(data = survey) +
  geom_point(aes(x=Wr.Hnd, y=NW.Hnd))

## ------------------------------------------------------------------------
cor.test(x=survey$Wr.Hnd, y=survey$NW.Hnd)

## ---- message=FALSE------------------------------------------------------
library("car")
data("Davis", package="car")

## ------------------------------------------------------------------------
htwt <- tbl_df(Davis)
htwt[12,c(2,3)] <- htwt[12,c(3,2)]
head(htwt)

## ------------------------------------------------------------------------
ggplot(htwt) + 
  geom_point(aes(x=height, y=weight, color=sex), size=2, alpha=0.5) +
  scale_color_manual(values=c("red", "blue"))

## ------------------------------------------------------------------------
cor.test(x=htwt$height, y=htwt$weight)

## ------------------------------------------------------------------------
cor.test(x=Davis$height, y=Davis$weight)

## ------------------------------------------------------------------------
cor.test(x=Davis$height, y=Davis$weight, method="spearman")

## ------------------------------------------------------------------------
htwt %>% filter(sex=="F") %>%  
  cor.test(~ height + weight, data = .)

## ------------------------------------------------------------------------
htwt %>% filter(sex=="M") %>%  
  cor.test(~ height + weight, data = .)

## ---- echo=FALSE---------------------------------------------------------
set.seed(201)
x <- rnorm(50) + 20
y <- 10 + 2*x + rnorm(50)
f <- lm(y ~ x)

df <- data.frame(x=x, y=y, f=f$fitted.values, 
                 lower=pmin(y, f$fitted.values), 
                 upper=pmax(y, f$fitted.values))

ggplot(df) + 
  geom_line(aes(x, f), color="blue") + 
  geom_linerange(aes(x=x, ymin=lower, ymax=upper), color="red") +
  geom_point(aes(x,y)) +
  labs(x="x", y="y")

## ------------------------------------------------------------------------
ggplot(data=htwt, mapping=aes(x=height, y=weight)) + 
  geom_point(size=2, alpha=0.5) +
  geom_smooth(method="lm", se=FALSE, formula=y~x)

## ------------------------------------------------------------------------
beta1 <- cor(htwt$height, htwt$weight) * 
               sd(htwt$weight) / sd(htwt$height)
beta1

beta0 <- mean(htwt$weight) - beta1 * mean(htwt$height)
beta0

yhat <- beta0 + beta1 * htwt$height

## ------------------------------------------------------------------------
df <- data.frame(htwt, yhat=yhat)
ggplot(data=df) + geom_point(aes(x=height, y=weight), size=2, alpha=0.5) +
  geom_line(aes(x=height, y=yhat), color="blue", size=1.2)

## ------------------------------------------------------------------------
myfit <- lm(weight ~ height, data=htwt)
myfit

## ---- eval=FALSE, echo=FALSE---------------------------------------------
## # Least Squares Regression
## 
## 
## # Regression with Several Variables
## 
## 
## # Regression with Mixed Variable Types

## ------------------------------------------------------------------------
sessionInfo()

## ----converttonotes, include=FALSE, cache=FALSE--------------------------
source("../customization/make_notes.R")

