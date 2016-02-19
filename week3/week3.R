## ----my_opts, cache=FALSE, include=FALSE---------------------------------
library(knitr)
opts_chunk$set(fig.align="center", fig.height=6, fig.width=7.5, collapse=TRUE, comment="", prompt=TRUE)
options(width=63)
knit_hooks$set(small.mar = function(before, options, envir) {
    if (before) par(mar = c(4, 4, .1, .1))  # smaller margin on top and right
})

## ------------------------------------------------------------------------
my_square <- function(x) {
  x*x  # can also do return(x*x)
}

my_square(x=2)

my_fun2 <- my_square
my_fun2(x=3)

## ------------------------------------------------------------------------
my_square_ext <- function(x) {
  y <- x*x
  return(list(x_original=x, x_squared=y))
}

my_square_ext(x=2)

z <- my_square_ext(x=2)

## ------------------------------------------------------------------------
my_power <- function(x, e, say_hello) {
  if(say_hello) {
    cat("Hello World!")
  }
  x^e
}

my_power(x=2, e=3, say_hello=TRUE)

z <- my_power(x=2, e=3, say_hello=TRUE)
z

## ------------------------------------------------------------------------
str(matrix)

## ------------------------------------------------------------------------
double_log <- function(x, ...) {
  log((2*x), ...)
}

double_log(x=1, base=2)
double_log(x=1, base=10)

## ---- include=FALSE, cache=FALSE-----------------------------------------
rm(list=ls())
load("../data/project_1_R_basics.RData")

## ---- eval=FALSE---------------------------------------------------------
## # assumes file in working directory
## load(file="project_1_R_basics.RData")

## ---- eval=FALSE---------------------------------------------------------
## # loads from our GitHub repository
## load(file=url("https://github.com/SML201/project1/raw/
##          master/project_1_R_basics.RData"))

## ------------------------------------------------------------------------
ls()

ls(name=globalenv())

# see help file for other options
?ls

## ------------------------------------------------------------------------
rm("some_ORFE_profs") # removes variable some_ORFE_profs

rm(list=ls()) # Removes all variables from environment

## ------------------------------------------------------------------------
x <- 1:8

x[1]           # extract the first element
x[2]           # extract the second element

x[1:4]         # extract the first 4 elements

x[c(1, 3, 4)]  # extract elements 1, 3, and 4
x[-c(1, 3, 4)] # extract all elements EXCEPT 1, 3, and 4

## ------------------------------------------------------------------------
names(x) <- letters[1:8]
x

x[c("a", "b", "f")]

s <- x > 3
s
x[s]

## ------------------------------------------------------------------------
x <- matrix(1:6, nrow=2, ncol=3, byrow=TRUE)
x

x[1,2]
x[1, ]
x[ ,2]

## ------------------------------------------------------------------------
colnames(x) <- c("A", "B", "C")

x[ , c("B", "C")]

x[c(FALSE, TRUE), c("B", "C")]

x[2, c("B", "C")]

## ------------------------------------------------------------------------
s <- (x %% 2) == 0
s

x[s]

x[c(2, 3, 6)]

## ------------------------------------------------------------------------
x <- list(my=1:3, favorite=c("a", "b", "c"), 
          course=c(FALSE, TRUE, NA))

x[[1]]
x[["my"]]
x$my

## ------------------------------------------------------------------------
x[[c(3,1)]]
x[[3]][1]

## ------------------------------------------------------------------------
x[c(3,1)]

## ------------------------------------------------------------------------
x <- data.frame(my=1:3, favorite=c("a", "b", "c"), 
          course=c(FALSE, TRUE, NA))

x[[1]]
x[["my"]]
x$my

## ------------------------------------------------------------------------
x[[c(3,1)]]
x[[3]][1]

## ------------------------------------------------------------------------
x[c(3,1)]

## ------------------------------------------------------------------------
x <- data.frame(my=1:3, favorite=c("a", "b", "c"), 
          course=c(FALSE, TRUE, NA))

x[1, ]
x[ ,3]
x[ ,"favorite"]

## ------------------------------------------------------------------------
x[1:2, ]
x[ ,2:3]

## ------------------------------------------------------------------------
x <- data.frame(my=1:3, favorite=c("a", "b", "c"), 
                course=c(FALSE, TRUE, NA), 
                stringsAsFactors=FALSE)

x[ ,"favorite"]
class(x[ ,"favorite"])

## ------------------------------------------------------------------------
data("airquality", package="datasets")
head(airquality)
dim(airquality)

## ------------------------------------------------------------------------
which(is.na(airquality$Ozone))
sum(is.na(airquality$Ozone))

## ------------------------------------------------------------------------
letters
vowels <- c("a", "e", "i", "o", "u")

letters %in% vowels
which(letters %in% vowels)

letters[which(letters %in% vowels)]

## ------------------------------------------------------------------------
library("reshape2")
library("datasets")
data(airquality, package="datasets")
names(airquality)
dim(airquality)

## ---- include=FALSE------------------------------------------------------
library("dplyr")
#names(airquality) <- tolower(names(airquality))
airquality <- tbl_df(airquality)

## ------------------------------------------------------------------------
head(airquality)

## ------------------------------------------------------------------------
tail(airquality)

## ------------------------------------------------------------------------
aql <- melt(airquality)
head(aql)

## ------------------------------------------------------------------------
tail(aql)

## ------------------------------------------------------------------------
aql <- melt(airquality, id.vars = c("Month", "Day"))
head(aql)

## ------------------------------------------------------------------------
tail(aql)

## ------------------------------------------------------------------------
aqw <- dcast(aql, Month + Day ~ variable)
head(aqw)

## ------------------------------------------------------------------------
tail(aqw)

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

## ------------------------------------------------------------------------
sessionInfo()

