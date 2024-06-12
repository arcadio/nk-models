# Maximum likelihood beta regression. Only for demonstration purposes,
# not used in the manuscript.

library(betareg)
source("data.r")

meap <- function(p, d, b, t) mean(p[d[[t]] == b]) - mean(p[d[[t]] != b])

gsam <- function(d, u) d[d$id %in% sample(u, replace=T),]

isam <- function(d, u) {
    l <- levels(d$day)
    a <- d[d$day == l[1],]
    b <- d[d$day != l[1],]
    rbind(a[sample(1:nrow(a), replace=T),], b[sample(1:nrow(b), replace=T),])
}

boot <- function(dat, sam, bas, type, fml, b, q) {
    p <- numeric(b)
    u <- unique(dat$id)
    for (i in 1:b) {
        d <- sam(dat, u)
        m <- betareg(fml, data=d)
        r <- predict(m)
        p[i] <- meap(r, d, bas, type)
    }
    quantile(p, c((1-q)/2, 1-(1-q)/2))
}

run <- function(dat, bas, type="type", fml=kill~type+id, sam=gsam, b=1000, q=0.95) {
    mod <- betareg(fml, data=dat)
    sry <- summary(mod)
    prd <- predict(mod)
    mea <- meap(prd, dat, bas, type)
    bot <- boot(dat, sam, bas, type, fml, b, q)
    list(sry=sry, mea=mea, bot=bot)
}

d2c <- read("2c")
tcv <- d2c[d2c$type == "tcnv",]
trg <- d2c[d2c$type == "treg",]

run(read("2b_a"), "tcnv")
run(read("2b_b"), "tcnv")

run(d2c, "tcnv")
run(tcv, 2.5, type="day", fml=kill~day, sam=isam)
run(trg, 2.5, type="day", fml=kill~day, sam=isam)

run(read("4c"), "high")
run(read("4d"), "high")
run(read("4e"), "high")
run(read("4f"), "high")
