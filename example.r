library(broom)
library(MKinfer)
library(betareg)

kill <- function(d) (d$dead / (d$dead + d$live)) - (d$sdea / (d$sdea + d$sliv))

dat <- read.csv("dat/4c.csv")
dat$type <- as.factor(dat$type)
dat$id <- as.factor(dat$id)
dat$kill <- kill(dat)

hig <- dat[dat$type == "high",]
low <- dat[dat$type == "low",]

tst <- t.test(hig$kill, low$kill, paired=T)
bot <- boot.t.test(hig$kill, low$kill, paired=T)

lin <- lm(kill ~ type + id, dat)
bet <- betareg(kill ~ type + id, dat)
sqr <- lm(sqrt(kill) ~ type + id, dat)

data.frame(rbind(glance(lin), glance(sqr)))
data.frame(glance(bet))
