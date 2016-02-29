## ----my_opts, cache=FALSE, include=FALSE---------------------------------
library(knitr)

knit_hooks$set(
  small.mar = function(before, options, envir) {
  if (before)
  par(mar = c(4, 4, .1, .1))  # smaller margin on top and right
  }
  )

opts_chunk$set(fig.align="center", fig.height=5.5, fig.width=6.75, 
               collapse=TRUE, comment="", prompt=TRUE, 
               small.mar=TRUE, cache=TRUE)

options(width=63)

library("ggplot2")

## ---- cache=FALSE--------------------------------------------------------
library(ggplot2)

## ---- cache=FALSE--------------------------------------------------------
theme_set(theme_bw())

## ---- eval=FALSE---------------------------------------------------------
## ggplot(data = mpg) +
##   geom_point(mapping = aes(x = displ, y = hwy, color=drv)) +
##   geom_smooth(mapping = aes(x = displ, y = hwy, color=drv)) +
##   scale_color_brewer(palette = "Set1", name = "Drivetrain") +
##   labs(title = "Highway MPG By Drivetrain and Displacement",
##        x = "Displacement", y = "Highway MPG")

## ---- eval=FALSE---------------------------------------------------------
## ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=drv)) +
##   geom_point() +
##   geom_smooth() +
##   scale_color_brewer(palette = "Set1", name = "Drivetrain") +
##   labs(title = "Highway MPG By Drivetrain and Displacement",
##        x = "Displacement", y = "Highway MPG")

## ---- cache=FALSE, message=FALSE-----------------------------------------
library("dplyr") # why load dplyr?
data("mpg", package="ggplot2")
head(mpg)

## ---- cache=FALSE, message=FALSE-----------------------------------------
data("diamonds", package="ggplot2")
head(diamonds)

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut), fill = "tomato")

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping= aes(x = cut, fill = clarity), position = "dodge")

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping=aes(x = cut, fill = clarity), position = "fill") +
  labs(x = "cut", y = "relative proporition within cut")

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = 1, y = hwy))

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = 1, y = hwy), fill="lightblue") +
  labs(x=NULL)

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = factor(cyl), y = hwy))

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = factor(cyl), y = hwy, 
                             fill = factor(cyl)))

## ------------------------------------------------------------------------
ggplot(data = mpg, mapping = aes(x=factor(cyl), y=hwy)) + 
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2)

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_violin(mapping = aes(x = drv, y = hwy))

## ------------------------------------------------------------------------
ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) + 
  geom_violin(adjust=1.2) +
  geom_jitter(width=0.2, alpha=0.5)

## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_boxplot(mapping = aes(x=color, y=price))

## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_violin(mapping = aes(x=color, y=price))

## ---- message=FALSE------------------------------------------------------
ggplot(diamonds) +
  geom_histogram(mapping = aes(x=price))

## ------------------------------------------------------------------------
ggplot(diamonds) +
  geom_histogram(mapping = aes(x=price), binwidth = 1000)

## ------------------------------------------------------------------------
ggplot(diamonds) +
  geom_histogram(mapping = aes(x=price, y=..density..), binwidth=1000)

## ------------------------------------------------------------------------
ggplot(diamonds) +
  geom_histogram(mapping = aes(x=price, fill = cut), binwidth = 1000)

## ------------------------------------------------------------------------
ggplot(diamonds) +
  geom_density(mapping = aes(x=price))

## ------------------------------------------------------------------------
ggplot(diamonds) +
  geom_density(mapping = aes(x=price), color="blue", fill="lightblue")

## ------------------------------------------------------------------------
ggplot(diamonds) +
  geom_density(mapping = aes(x=price, color=clarity))

## ---- message=FALSE------------------------------------------------------
ggplot(diamonds, mapping = aes(x=price)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(fill="lightblue", alpha=.5)

## ------------------------------------------------------------------------
library("babynames")
john <- babynames %>% filter(sex=="M", name=="John")
head(john)

## ------------------------------------------------------------------------
ggplot(data = john) + 
  geom_line(mapping = aes(x=year, y=prop), size=1.5)

## ------------------------------------------------------------------------
kelly <- babynames %>% filter(name=="Kelly")
ggplot(data = kelly) + 
  geom_line(mapping = aes(x=year, y=prop, color=sex), size=1.5)

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
  

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class), size=2)

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

## ---- warning=FALSE------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

## ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_point(mapping = aes(x=carat, y=price, color=cut), alpha=0.7)

## ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_point(mapping=aes(x=carat, y=price, color=clarity), alpha=0.3)

## ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_point(mapping=aes(x=carat, y=price, color=clarity), alpha=0.3) + 
  guides(color = guide_legend(override.aes = list(alpha = 1)))

## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_boxplot(aes(x=color, y=price)) 

## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_boxplot(aes(x=color, y=price)) + 
  scale_y_log10()

## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_violin(aes(x=color, y=price)) + 
  scale_y_log10()

## ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_point(mapping=aes(x=carat, y=price, color=clarity), alpha=0.3)

## ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_point(aes(x=carat, y=price, color=clarity), alpha=0.3) +
  scale_y_sqrt()

## ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_point(aes(x=carat, y=price, color=clarity), alpha=0.3) +
  scale_y_log10(breaks=c(1000,5000,10000)) + 
  scale_x_log10(breaks=1:5)

## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_violin(aes(x=clarity, y=price, fill=clarity), adjust=1.5) +  
  scale_y_log10()

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

## ------------------------------------------------------------------------
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(se=FALSE)

## ------------------------------------------------------------------------
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(aes(colour = "loess"), method = "loess", se = FALSE) + 
  geom_smooth(aes(colour = "lm"), method = "lm", se = FALSE)

## ------------------------------------------------------------------------
ggplot(data=mpg, mapping = aes(x = displ, y = hwy, linetype = drv)) + 
  geom_point() + 
  geom_smooth(se=FALSE)

## ------------------------------------------------------------------------
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=drv)) + 
  geom_point() +
  geom_smooth(se=FALSE)

## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(x=carat, y=price)) +
  geom_point()

## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(x=carat, y=price)) +
  geom_point(alpha=0.1)

## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(x=carat, y=price)) +
  geom_point(alpha=0.01)

## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(x=carat, y=price)) +
  geom_hex()

## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(x=carat, y=price)) +
  geom_hex() + 
  scale_fill_gradient2(low="lightblue", mid="purple", high="black", 
                       midpoint=3000)

## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(x=carat, y=price)) +
  geom_hex(bins=20) + scale_fill_gradient() +
  scale_x_log10(breaks=1:5) + scale_y_log10(breaks=c(1000,5000,10000)) 

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = factor(cyl), y = hwy)) +
  labs(title="Highway MPG by Cylinders",x="Cylinders",y="Highway MPG")

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut)) +
  theme(legend.position="none")

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut)) +
  theme(legend.position="bottom")

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut)) +
  theme(legend.position=c(0.15,0.75))

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut)) +
  scale_fill_discrete(name="Diamond\nCut")

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut)) +
  scale_fill_discrete(labels=c("F", "G", "VG", "P", "I"))

## ------------------------------------------------------------------------
ggplot(mpg) + geom_histogram(mapping=aes(x=displ), binwidth=0.25)

## ------------------------------------------------------------------------
ggplot(mpg) + 
  geom_histogram(mapping=aes(x=displ), binwidth=0.25) + 
  facet_wrap(~ cyl)

## ------------------------------------------------------------------------
ggplot(mpg) + 
  geom_histogram(mapping=aes(x=displ), binwidth=0.25) + 
  facet_wrap(~ drv)

## ------------------------------------------------------------------------
ggplot(mpg) + 
  geom_histogram(mapping=aes(x=displ), binwidth=0.25) + 
  facet_grid(drv ~ cyl)

## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_histogram(mapping=aes(x=price), binwidth=500)

## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_histogram(mapping=aes(x=price), binwidth=500) + 
  facet_wrap(~ clarity)

## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_histogram(mapping=aes(x=price), binwidth=500) + 
  facet_wrap(~ clarity, scale="free_y")

## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_histogram(mapping=aes(x=price), binwidth=500) + 
  facet_grid(cut ~ clarity) +
  scale_x_continuous(breaks=9000)

## ------------------------------------------------------------------------
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")

## ------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut)) +
  scale_fill_manual(values=cbPalette)

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class), size=2) +
  scale_color_manual(values=cbPalette)

## ---- message=FALSE------------------------------------------------------
ggplot(data = mpg) + 
  geom_histogram(aes(x=hwy, fill=..count..)) + 
  scale_fill_gradient(low="blue", high="red")

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(aes(x=hwy, y=cty, color=displ), size=2) + 
  scale_color_gradient(low="blue", high="red")

## ------------------------------------------------------------------------
ggplot(diamonds) +
  geom_density(mapping = aes(x=price, color=clarity)) +
  scale_color_brewer(palette = "Set1")

## ------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  scale_color_brewer(palette = "Set1")

## ---- eval=FALSE---------------------------------------------------------
## ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=drv)) +
##   geom_point() +
##   geom_smooth(se=FALSE)

## ---- eval=FALSE---------------------------------------------------------
## p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=drv)) +
##   geom_point()
## p + geom_smooth(se=FALSE)

## ---- eval=FALSE---------------------------------------------------------
## p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=drv))
## p +  geom_point() + geom_smooth(se=FALSE)

## ---- eval=FALSE---------------------------------------------------------
## p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=drv)) +
##   geom_point() +
##   geom_smooth(se=FALSE)
## ggsave(filename="my_plot.pdf", plot=p) # saves PDF file
## ggsave(filename="my_plot.png", plot=p) # saves PNG file

## ------------------------------------------------------------------------
str(ggsave)

## ---- eval=FALSE---------------------------------------------------------
## theme_set(theme_minimal())

## ---- eval=FALSE---------------------------------------------------------
## ggplot(data = diamonds) +
##   geom_bar(mapping = aes(x = cut)) +
##   theme_minimal()

## ------------------------------------------------------------------------
sessionInfo()

## ----converttonotes, include=FALSE, cache=FALSE--------------------------
source("../customization/make_notes.R")

