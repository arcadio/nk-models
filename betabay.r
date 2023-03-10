library(rstanarm)
source("data.r")

options(mc.cores=parallel::detectCores())

run <- function(dat, bas, fml=kill~type+id) {
    fit <- stan_betareg(fml, dat, link="logit", link.phi="log", iter=5e3)
    pri <- prior_summary(fit)
    pst <- posterior_epred(fit)
    dif <- rowMeans(pst[,dat$type==bas] - pst[,dat$type!=bas])
    fld <- rowMeans(pst[,dat$type==bas] / pst[,dat$type!=bas])
    dry <- summary(dif)
    fry <- summary(fld)
    fsr <- ifelse(dry[3] > 0, mean(dif < 0), mean(dif > 0))
    list(fit=fit, dif=dif, fld=fld, pri=pri, dry=dry, fry=fry, fsr=fsr)
}

dba <- run(read("2b_a"), "tcnv")
dbb <- run(read("2b_b"), "tcnv")

d4c <- run(read("4c"), "high")
d4d <- run(read("4d"), "high")
d4e <- run(read("4e"), "high")
d4f <- run(read("4f"), "high")
