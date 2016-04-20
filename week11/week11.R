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
mydata <- 
  read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
dim(mydata)
head(mydata)

## ------------------------------------------------------------------------
apply(mydata, 2, mean)
apply(mydata, 2, sd)

table(mydata$admit, mydata$rank)

## ------------------------------------------------------------------------
ggplot(data=mydata) +
  geom_boxplot(aes(x=as.factor(admit), y=gre))

## ------------------------------------------------------------------------
ggplot(data=mydata) +
  geom_boxplot(aes(x=as.factor(admit), y=gpa))

## ------------------------------------------------------------------------
mydata$rank <- factor(mydata$rank, levels=c(1, 2, 3, 4))
myfit <- glm(admit ~ gre + gpa + rank, 
             data = mydata, family = "binomial")
myfit

## ------------------------------------------------------------------------
summary(myfit)

## ------------------------------------------------------------------------
anova(myfit, test="Chisq")

## ------------------------------------------------------------------------
cuse <- 
  read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", 
             header=TRUE)
dim(cuse)
head(cuse)

## ------------------------------------------------------------------------
head(cuse)

## ------------------------------------------------------------------------
myfit <- glm(cbind(using, notUsing) ~ age + education + 
               wantsMore, data=cuse, family = binomial)
myfit

## ------------------------------------------------------------------------
summary(myfit)

## ------------------------------------------------------------------------
anova(myfit, test="Chisq")

## ---- message=FALSE------------------------------------------------------
library("dplyr")
library("kernlab")
library("broom")
data("spam")
spam <- tbl_df(spam)
names(spam)

## ------------------------------------------------------------------------
dim(spam)
head(spam)

## ------------------------------------------------------------------------
spam <- spam %>% 
        mutate(response=as.numeric(type=="spam")) %>% 
        select(-type)
mean(spam$response)

## ------------------------------------------------------------------------
myfit <- glm(response ~ edu, family=binomial, data=spam)
myfit

## ------------------------------------------------------------------------
summary(myfit)

## ------------------------------------------------------------------------
logit <- function(p, tol=1e-10) {
  p <- pmin(pmax(tol, p), 1-tol)
  log(p/(1-p))
}

## ------------------------------------------------------------------------
plot(spam$edu, myfit$fitted.values)

## ------------------------------------------------------------------------
plot(spam$edu, logit(myfit$fitted.values))

## ------------------------------------------------------------------------
myfit <- glm(response ~ ., family=binomial, data=spam)
x <- tidy(myfit)
head(x)

## ------------------------------------------------------------------------
pred_response <- as.numeric(myfit$fitted.values >= 0.5)
mean(pred_response == spam$response)

## ------------------------------------------------------------------------
boxplot(myfit$fitted.values[spam$response==0], 
        myfit$fitted.values[spam$response==1])

## ------------------------------------------------------------------------
set.seed(210)
v <- sample(nrow(spam), size=round(2*nrow(spam)/3))
spam0 <- spam[v,]  ## training data
spam1 <- spam[-v,]  ## test data
fit0 <- glm(response ~ ., family=binomial, data=spam0)
pred_prob <- predict(fit0, newdata=spam1[,-ncol(spam1)], 
                     type="response")

## ------------------------------------------------------------------------
boxplot(pred_prob[spam1$response==0], 
        pred_prob[spam1$response==1])

## ------------------------------------------------------------------------
pred_response <- as.numeric(pred_prob >= 0.5)
mean(pred_response == spam1$response)

## ---- message=FALSE, warning=FALSE, cache=TRUE---------------------------
library("caret")
## remove and reload data to undo my earlier changes
rm(spam) 
data("spam", package="kernlab")

## ---- message=FALSE, warning=FALSE, cache=TRUE---------------------------
set.seed(201)
inTrain <- createDataPartition(y=spam$type, p=0.6, 
                               list=FALSE)
training <-spam[inTrain,]
testing <- spam[-inTrain,]

## ---- message=FALSE, warning=FALSE, cache=TRUE---------------------------
modelFit_glm <- train(type ~., data=training, method="glm")
modelFit_glm

## ---- message=FALSE, warning=FALSE, cache=TRUE---------------------------
predictions <- predict(modelFit_glm, newdata=testing)
confusionMatrix(predictions, testing$type)

## ---- message=FALSE, warning=FALSE, cache=TRUE---------------------------
modelFit_svm <- train(type ~., data=training, method="svmLinear")
modelFit_svm

## ---- message=FALSE, warning=FALSE, cache=TRUE---------------------------
predictions <- predict(modelFit_svm, newdata=testing)
confusionMatrix(predictions, testing$type)

## ---- message=FALSE, warning=FALSE, cache=TRUE---------------------------
modelFit_rpart <- train(type ~., data=training, method="rpart")
modelFit_rpart

## ---- message=FALSE, warning=FALSE, cache=TRUE---------------------------
predictions <- predict(modelFit_rpart, newdata=testing)
confusionMatrix(predictions, testing$type)

## ------------------------------------------------------------------------
sessionInfo()

## ----converttonotes, include=FALSE, cache=FALSE--------------------------
source("../customization/make_notes.R")

