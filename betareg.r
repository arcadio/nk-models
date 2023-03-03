library(betareg)

kill <- function(d) (d$dead / (d$dead + d$live)) - (d$sdea / (d$sdea + d$sliv))

boot <- function(dat, lvl, b, q) {
    p <- numeric(b)
    u <- unique(dat$id)
    for (i in 1:b) {
        d <- dat[dat$id %in% sample(u, replace=T),]
        m <- betareg(kill ~ type + id, data=d)
        r <- predict(m)
        p[i] <- mean(r[d$type == lvl[1]]) - mean(r[d$type == lvl[2]])
    }
    quantile(p, c((1-q)/2, 1-(1-q)/2))
}

run <- function(fil, lvl, b=1000, q=0.95) {
    dat <- na.omit(read.csv(paste0("dat/", fil, ".csv")))
    dat$kill <- kill(dat)
    mod <- betareg(kill ~ type + id, data=dat)
    sry <- summary(mod)
    prd <- predict(mod)
    mea <- mean(prd[dat$type == lvl[1]]) - mean(prd[dat$type == lvl[2]])
    bot <- boot(dat, lvl, b, q)
    list(sry=sry, mea=mea, bot=bot)
}

run("2b_a", c("tcnv", "treg")) # 4.56e-8 0.048 (0.026, 0.069)
run("2b_b", c("tcnv", "treg")) #   0.058 0.019 (-0.002, 0.041)

run("4c", c("high", "low")) # <2e-16 0.080 (0.066, 0.095)
run("4d", c("high", "low")) # <2e-16 0.071 (0.048, 0.092)
run("4e", c("high", "low")) #  0.011 0.028 (0.005, 0.052)
run("4f", c("high", "low")) #  0.053 0.015 (-0.004, 0.034)
