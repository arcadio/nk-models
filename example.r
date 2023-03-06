library(broom)
library(MKinfer)
library(betareg)
source("data.r")

dat <- read("4c")
hig <- dat[dat$type == "high",]
low <- dat[dat$type == "low",]

tst <- t.test(hig$kill, low$kill, paired=T)
bot <- boot.t.test(hig$kill, low$kill, paired=T, R=1e8)
non <- wilcox.test(hig$kill, low$kill, paired=T)

lin <- lm(kill ~ type + id, dat)
bet <- betareg(kill ~ type + id, dat)
sqr <- lm(sqrt(kill) ~ type + id, dat)

data.frame(rbind(glance(lin), glance(sqr)))
data.frame(glance(bet))
