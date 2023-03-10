library(rstanarm)
source("data.r")

options(mc.cores=parallel::detectCores())

run <- function(dat, bas, fml=kill~type+id) {
    fit <- stan_betareg(fml, dat, link="logit", link.phi="log", iter=5e3, seed=42)
    pri <- prior_summary(fit)
    pst <- posterior_epred(fit)
    gm1 <- rowMeans(pst[,dat$type==bas])
    gm2 <- rowMeans(pst[,dat$type!=bas])
    dif <- rowMeans(pst[,dat$type==bas] - pst[,dat$type!=bas])
    fld <- rowMeans(pst[,dat$type==bas] / pst[,dat$type!=bas])
    dry <- summary(dif)
    fry <- summary(fld)
    fsr <- ifelse(dry[3] > 0, mean(dif < 0), mean(dif > 0))
    list(fit=fit, dif=dif, gm1=gm1, gm2=gm2, fld=fld, pri=pri, fry=fry, fsr=fsr)
}

dba <- run(read("2b_a"), "tcnv")
dbb <- run(read("2b_b"), "tcnv")

d4c <- run(read("4c"), "high")
d4d <- run(read("4d"), "high")
d4e <- run(read("4e"), "high")
d4f <- run(read("4f"), "high")
