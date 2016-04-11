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

## ---- echo="FALSE"-------------------------------------------------------
library("MASS")
data("survey", package="MASS")

## ---- warning=FALSE------------------------------------------------------
ggplot(data = survey, mapping=aes(x=Wr.Hnd, y=Height)) +
  geom_point() + geom_vline(xintercept=mean(survey$Wr.Hnd, na.rm=TRUE)) +
  geom_hline(yintercept=mean(survey$Height, na.rm=TRUE))

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

## ------------------------------------------------------------------------
class(myfit)
is.list(myfit)
names(myfit)

## ------------------------------------------------------------------------
summary(myfit)

## ------------------------------------------------------------------------
mysummary <- summary(myfit)
names(mysummary)

## ------------------------------------------------------------------------
library(broom)
tidy(myfit)

## ---- echo=FALSE, fig.width=10, fig.height=4-----------------------------
par(mfrow=c(1,3))
set.seed(777)
n <- 60
x <- (1:n) * (20/n) + rnorm(n, sd=0.1)
y1 <- rnorm(n); y1 <- y1 - mean(y1)
y2 <- rnorm(n) + 0.07*(x-10.5)^2; y2 <- y2 - mean(y2); y2 <- y2/sd(y2)
y3 <- rnorm(n); y3 <- y3 - mean(y3)
y3 <- x^(1.5)*y3; y3 <- y3/sd(y3)
plot(x, y1, xlab=" ", ylab="Residuals", pch=20, cex=2,
     ylim=c(-2,2), cex.lab=1.2); abline(h=0)
plot(x, y2, xlab="Fitted Values", ylab=" ", pch=20, cex=2, 
     ylim=c(-2,2), cex.lab=1.5); abline(h=0)
plot(x, y3, xlab=" ", ylab=" ", pch=20, cex=2, 
     ylim=c(-2,2)); abline(h=0)

## ------------------------------------------------------------------------
plot(myfit, which=1)

## ------------------------------------------------------------------------
plot(myfit, which=2)

## ------------------------------------------------------------------------
summary(myfit)$r.squared

var(myfit$fitted.values)/var(htwt$weight)

## ------------------------------------------------------------------------
data("chickwts", package="datasets")
head(chickwts)
summary(chickwts$feed)

## ------------------------------------------------------------------------
chick_fit <- lm(weight ~ feed, data=chickwts)
summary(chick_fit)

## ------------------------------------------------------------------------
plot(chickwts$feed, chickwts$weight, xlab="Feed", ylab="Weight")
points(chickwts$feed, chick_fit$fitted.values, col="blue", pch=20, cex=2)

## ------------------------------------------------------------------------
anova(chick_fit)

## ------------------------------------------------------------------------
n <- length(chick_fit$residuals) # n <- 71
(n-1)*var(chick_fit$fitted.values)
(n-1)*var(chick_fit$residuals)
(n-1)*var(chickwts$weight) # sum of above two quantities
(231129/5)/(195556/65) # F-statistic

## ------------------------------------------------------------------------
levels(chickwts$feed)
head(chickwts, n=3)
tail(chickwts, n=3)
x <- model.matrix(weight ~ feed, data=chickwts)
dim(x)

## ------------------------------------------------------------------------
head(x)

## ------------------------------------------------------------------------
tail(x)

## ------------------------------------------------------------------------
chick_fit$fitted.values %>% round(digits=4) %>% unique()

## ------------------------------------------------------------------------
chickwts %>% group_by(feed) %>% summarize(mean(weight))

## ------------------------------------------------------------------------
summary(lm(weight ~ height + sex, data=htwt))

## ------------------------------------------------------------------------
lm(weight ~ height + sex, data=htwt)

## ------------------------------------------------------------------------
htwt <- htwt %>% mutate(height2 = height^2)
summary(lm(weight ~ height + height2, data=htwt))

## ------------------------------------------------------------------------
summary(lm(weight ~ height + sex + height:sex, data=htwt))

## ---- echo=FALSE---------------------------------------------------------
f1 <- lm(weight ~ height, data=htwt)
f2 <- lm(weight ~ height + sex, data=htwt)
f3 <- lm(weight ~ height + sex + height:sex, data=htwt)

fits <- data.frame(height=htwt$height, weight=htwt$weight, 
                   f1=f1$fitted.values, f2=f2$fitted.values,
                   f3=f3$fitted.values, sex=htwt$sex)

ggplot(data = fits) +
  geom_line(aes(x=height, y=f1), color="black", size=1.5, alpha=0.5) + 
  geom_line(aes(x=height, y=f2, color=sex), linetype=2, size=1.5, alpha=0.5) + 
  geom_line(aes(x=height, y=f3, color=sex), size=1.5, alpha=0.5) + 
  geom_point(aes(x=height, y=weight, color=sex)) +
  scale_color_manual(values = c("red", "blue")) + 
  labs(x="height", y="weight")

## ------------------------------------------------------------------------
f1 <- lm(weight ~ height, data=htwt)
f2 <- lm(weight ~ height + sex, data=htwt)
f3 <- lm(weight ~ height + sex + height:sex, data=htwt)

## ------------------------------------------------------------------------
anova(f1, f2)

## ------------------------------------------------------------------------
tidy(f2)

## ------------------------------------------------------------------------
anova(f1, f2)

## ------------------------------------------------------------------------
n <- nrow(htwt)
ss1 <- (n-1)*var(f1$residuals)
ss1
ss2 <- (n-1)*var(f2$residuals)
ss2
((ss1 - ss2)/anova(f1, f2)$Df[2])/(ss2/f2$df.residual)

## ------------------------------------------------------------------------
anova(f1, f3)

## ------------------------------------------------------------------------
anova(f1, f2, f3)

## ---- cache=TRUE---------------------------------------------------------
data("diamonds", package="ggplot2")
head(diamonds)

## ---- cache=TRUE---------------------------------------------------------
ggplot(data = diamonds) +
  geom_point(mapping=aes(x=carat, y=price, color=clarity), alpha=0.3)

## ---- cache=TRUE---------------------------------------------------------
diam_fit <- lm(price ~ carat + clarity, data=diamonds)
anova(diam_fit)

## ---- cache=TRUE---------------------------------------------------------
plot(diam_fit, which=1)

## ---- cache=TRUE---------------------------------------------------------
plot(diam_fit, which=2)

## ---- cache=TRUE---------------------------------------------------------
ggplot(data = diamonds) +
  geom_point(aes(x=carat, y=price, color=clarity), alpha=0.3) +
  scale_y_log10(breaks=c(1000,5000,10000)) + 
  scale_x_log10(breaks=1:5)

## ---- cache=TRUE---------------------------------------------------------
diamonds <- mutate(diamonds, log_price = log(price, base=10), 
                   log_carat = log(carat, base=10))
ldiam_fit <- lm(log_price ~ log_carat + clarity, data=diamonds)
anova(ldiam_fit)

## ---- cache=TRUE---------------------------------------------------------
plot(ldiam_fit, which=1)

## ---- cache=TRUE---------------------------------------------------------
plot(ldiam_fit, which=2)

## ------------------------------------------------------------------------
sessionInfo()

## ----converttonotes, include=FALSE, cache=FALSE--------------------------
source("../customization/make_notes.R")

