## ----my_opts, cache=FALSE, include=FALSE---------------------------------
library(knitr)
knit_hooks$set(small.mar = function(before, options, envir) {
    if (before) par(mar = c(4, 4, .1, .1))  # smaller margin on top and right
})
opts_chunk$set(fig.align="center", fig.height=5.5, fig.width=6.75, collapse=TRUE, comment="", prompt=TRUE, small.mar=TRUE)
options(width=63)
library("rvest")
library("dplyr")
library("datasets")

## ---- eval=FALSE, prompt=FALSE-------------------------------------------
## pkgs <- c("dplyr", "babynames", "readr", "tidyr", "reshape2",
##           "hexbin", "ggplot2", "ggthemes", "broom",
##           "devtools", "RColorBrewer", "rvest", "xtable")
## install.packages(pkgs)

## ---- include=FALSE------------------------------------------------------
rm(list=ls())

## ------------------------------------------------------------------------
library("dplyr", verbose=FALSE)
library("babynames")
ls()
babynames <- babynames::babynames
ls()

## ------------------------------------------------------------------------
class(babynames)
dim(babynames)

## ------------------------------------------------------------------------
babynames

## ------------------------------------------------------------------------
set.seed(201)
sample_n(babynames, 10) 
# try also sample_frac(babynames, 6e-6)

## ------------------------------------------------------------------------
x <- 1:10

x %>% log(base=10) %>% sum

sum(log(x,base=10))

## ------------------------------------------------------------------------
babynames %>% sample_n(5)

## ------------------------------------------------------------------------
filter(babynames, year==1880, sex=="F")
# same as filter(babynames, year==1880 & sex=="F")

## ------------------------------------------------------------------------
filter(babynames, year==1880, sex=="F", n > 5000)

## ------------------------------------------------------------------------
arrange(babynames, name, year, sex)

## ------------------------------------------------------------------------
arrange(babynames, desc(name), desc(year), sex)

## ------------------------------------------------------------------------
rename(babynames, number=n)

## ------------------------------------------------------------------------
select(babynames, sex, name, n)
# same as select(babynames, sex:n)

## ------------------------------------------------------------------------
select(babynames, sex, name, number=n)

## ------------------------------------------------------------------------
mutate(babynames, total_by_year=round(n/prop))
# see also transmutate

## ------------------------------------------------------------------------
babynames %>% mutate(total_by_year=round(n/prop)) %>% 
  select(sex, year, total_by_year) %>% distinct()

## ------------------------------------------------------------------------
summarize(babynames, mean_n = mean(n), median_n = median(n), 
          number_sex = n_distinct(sex), 
          distinct_names = n_distinct(name))

## ------------------------------------------------------------------------
babynames %>% group_by(year, sex)

## ------------------------------------------------------------------------
babynames %>% group_by(year, sex) %>% 
  summarize(total_by_year=sum(n))

## ------------------------------------------------------------------------
babynames %>% group_by(sex) %>% 
  summarize(mean_n = mean(n), 
            distinct_names_sex = n_distinct(name))

## ------------------------------------------------------------------------
top_names <- babynames %>% group_by(year, sex) %>% 
  summarize(top_name = name[which.max(n)])

head(top_names)

## ------------------------------------------------------------------------
tail(top_names, n=10)

## ------------------------------------------------------------------------
top_names %>% filter(year >= 1990 & year < 2000, sex=="F")

## ------------------------------------------------------------------------
top_names %>% filter(year >= 1990 & year < 2000, sex=="M")

## ---- small.mar=TRUE-----------------------------------------------------
# Analyzing the name 'John'
john <- babynames %>% filter(sex=="M", name=="John")
plot(john$year, john$prop, type="l")

## ---- small.mar=TRUE-----------------------------------------------------
# Analyzing the name 'Bella'
bella <- babynames %>% filter(sex=="F", name=="Bella") 
plot(bella$year, bella$prop, type="l")

## ---- eval=FALSE---------------------------------------------------------
## ?read.table
## ?write.table

## ------------------------------------------------------------------------
data("airquality", package="datasets")
head(airquality, n=8)

write.table(airquality, file="../data/airquality.txt", 
            sep="\t", row.names=FALSE)
write.csv(airquality, file="../data/airquality.csv", 
          row.names=FALSE)

## ------------------------------------------------------------------------
readLines(con="../data/airquality.txt", n=2)
aq1 <- read.table(file="../data/airquality.txt", header=TRUE, 
                  sep="\t")

readLines(con="../data/airquality.csv", n=2)
aq2 <- read.csv(file="../data/airquality.csv", header=TRUE)

dim(aq1) == dim(aq2)
sum(aq1 != aq2, na.rm=TRUE)

## ------------------------------------------------------------------------
library("rvest")
schedule <- read_html("http://sml201.github.io/schedule/")
first_table <- html_table(schedule)[[1]]
names(first_table) <- c("week", "topics", "reading")
first_table[4,"week"]
first_table[4,"topics"] %>% strsplit(split="  ")
first_table[4,"reading"] %>% strsplit(split="  ")
grep("EDAR", first_table$reading)

## ------------------------------------------------------------------------
usg_url <- "http://princetonusg.com/meet-your-usg-officers/"
usg <- read_html(usg_url)
officers <- html_nodes(usg, ".team-member-name") %>% 
            html_text
head(officers, n=20)

## ------------------------------------------------------------------------
data("mtcars", package="datasets")
head(mtcars)

## ------------------------------------------------------------------------
mean(mtcars$mpg)
median(mtcars$mpg)

sample_mode <- function(x) {
  as.numeric(names(which(table(x) == max(table(x)))))
}

sample_mode(round(mtcars$mpg))

## ------------------------------------------------------------------------
fivenum(mtcars$mpg)
summary(mtcars$mpg)

quantile(mtcars$mpg, prob=seq(0, 1, 0.25))

## ------------------------------------------------------------------------
var(mtcars$mpg)

## ------------------------------------------------------------------------
sd(mtcars$mpg)

## ------------------------------------------------------------------------
IQR(mtcars$mpg)
diff(fivenum(mtcars$mpg)[c(2,4)])

## ------------------------------------------------------------------------
sd_units <- abs(mtcars$wt - mean(mtcars$wt))/sd(mtcars$wt)
sum(sd_units > 3)
max(sd_units)

iqr_outlier_cuts <- fivenum(mtcars$wt)[c(2,4)] + 
      c(-1.5, 1.5)*diff(fivenum(mtcars$wt)[c(2,4)])
sum(mtcars$wt < iqr_outlier_cuts[1] | 
    mtcars$wt > iqr_outlier_cuts[2])

## ---- eval=FALSE---------------------------------------------------------
## ?barplot
## ?boxplot
## ?hist
## ?density
## ?plot
## ?legend

## ---- eval=FALSE---------------------------------------------------------
## cyl_tbl <- table(mtcars$cyl)
## barplot(cyl_tbl, xlab="Cylinders", ylab="Count")

## ---- echo=FALSE---------------------------------------------------------
cyl_tbl <- table(mtcars$cyl)
barplot(cyl_tbl, xlab="Cylinders", ylab="Count", ylim=c(0,16))

## ------------------------------------------------------------------------
boxplot(mtcars$mpg, ylab="MPG", col="lightgray")

## ------------------------------------------------------------------------
boxplot(mtcars$wt, ylab="Weight (1000 lbs)", 
        col="lightgray")

## ------------------------------------------------------------------------
hist(mtcars$mpg, xlab="MPG", main="", col="lightgray")

## ------------------------------------------------------------------------
hist(mtcars$mpg, breaks=12, xlab="MPG", main="", col="lightgray")

## ------------------------------------------------------------------------
plot(density(mtcars$mpg), xlab="MPG", main="")
polygon(density(mtcars$mpg), col="lightgray", border="black")

## ------------------------------------------------------------------------
boxplot(mpg ~ cyl, data=mtcars, xlab="Cylinders", 
        ylab="MPG", col="lightgray")

## ------------------------------------------------------------------------
counts <- table(mtcars$cyl, mtcars$gear)
counts

## ---- eval=FALSE---------------------------------------------------------
## barplot(counts, main="Number of Gears and Cylinders",
##   xlab="Gears", col=c("blue","red", "lightgray"))
## legend(x="topright", title="Cyl",
##        legend = rownames(counts),
##        fill = c("blue","red", "lightgray"))

## ---- echo=FALSE, fig.height=7, small.mar=FALSE--------------------------
par(mar = c(4, 4, 1, 1))
counts <- table(mtcars$cyl, mtcars$gear)
barplot(counts, main="Number of Gears and Cylinders",
  xlab="Gears", col=c("blue","red", "lightgray"), ylim=c(0,16))
legend(x="topright", title="Cyl",
       legend = rownames(counts), 
       fill = c("blue","red", "lightgray"))

## ------------------------------------------------------------------------
plot(mtcars$wt, mtcars$mpg, xlab="Weight (1000 lbs)", 
     ylab="MPG")

## ------------------------------------------------------------------------
sessionInfo()

## ----converttonotes, include=FALSE, cache=FALSE--------------------------
source("../customization/make_notes.R")

