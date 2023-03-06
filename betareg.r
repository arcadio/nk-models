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

run(read("2b_a"), "tcnv") # 4.56e-8 0.048 (0.026, 0.069)
run(read("2b_b"), "tcnv") #   0.058 0.019 (-0.002, 0.041)

run(d2c, "tcnv") # 0.439 0.007 (-0.012, 0.027)
run(tcv, 2.5, type="day", fml=kill~day, sam=isam) # 3.6e-10 -0.094 (-0.114, -0.071)
run(trg, 2.5, type="day", fml=kill~day, sam=isam) # 0.375   -0.012 (-0.039, 0.013)

run(read("4c"), "high") # <2e-16 0.080 (0.066, 0.095)
run(read("4d"), "high") # <2e-16 0.071 (0.048, 0.092)
run(read("4e"), "high") #  0.011 0.028 (0.005, 0.052)
run(read("4f"), "high") #  0.053 0.015 (-0.004, 0.034)
