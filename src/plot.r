# Plot posterior distributions.

library(tidyverse)
library(ggdist)

theme_set(theme_classic(base_size=18) + theme(plot.title=element_text(hjust=0.5)))

mft <- function() function(x) format(100*x, digits=2)

fldplot <- function(id, dat, xlab, lab, xlim=NULL, col=c(180, 300)) {
    p <- ggplot(dat, aes(x=fld, y=exp, fill=after_stat(x < 1))) +
        stat_halfeye(alpha=0.5, .width=c(0.025, 0.975), linewidth=4) +
        geom_vline(xintercept=1, linetype="dashed", color="grey") +
        coord_cartesian(xlim=xlim) +
        scale_y_discrete(labels=lab) +
        scale_fill_manual(values=col) +
        labs(x=bquote(paste("Fold change ", "(", .(xlab), ")")), y="") +
        guides(fill="none")
    ggsave(paste0("tmp/fold", id, ".pdf"), p)
}

dotplot <- function(id, err, tit, lab, ylim=NULL, col=c(180, 300)) {
    p <- ggplot(data=read(id), mapping=aes(x=type, y=kill)) +
        geom_line(aes(group=id), alpha=0.1) +
        geom_point(size=3, aes(color=type, shape=day), alpha=0.9) +
        scale_shape_manual(values=c(15, 17)) +
        stat_halfeye(data=err, aes(fill=type), alpha=0.5,
                     .width=c(0.025, 0.975), linewidth=4) +
        scale_x_discrete(labels=lab) +
        scale_y_continuous(labels=mft()) +
        scale_color_manual(values=col) +
        scale_fill_manual(values=col) +
        coord_cartesian(ylim=ylim) +
        labs(x="", y="Specific killing (%)", title=tit) +
        guides(color="none", fill="none", shape=guide_legend(title="Day"))
    ggsave(paste0("tmp/dot", id, ".pdf"), p)
}

rdf <- function(a, b) tibble(fld=map(a, \(x) x$fld), exp=b) |> unnest(cols=c(fld))

rdk <- function(a, b) tibble(type=a, kill=list(b$gm1, b$gm2)) |> unnest(cols=c(kill))

pal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#008080")
pl1 <- pal[1:2]
pl2 <- pal[3:4]
pl3 <- pal[5:6]
lhl <- c(bquote(Prolif^high), bquote(Prolif^low))

fldplot("2b",
        rdf(list(dba, dbb), c("2b(a)", "2b(b)")),
        bquote(Kill[Tconv] / Kill[Treg]),
        c(bquote(CD56^br), bquote(CD56^dim)),
        c(0.25, 2), pl1)

fldplot("2c",
        rdf(list(dca, dcb, dcc), c("tconv", "treg", "all")),
        bquote(Kill[3.5] / Kill[2.5]),
        c("Tconv", "Treg", bquote(atop(Tconv + "", Treg))),
        c(0.25, 4), pl2)

fldplot("4",
        rdf(list(d4c, d4d, d4e, d4f), c("4c", "4d", "4e", "4f")),
        bquote(Kill[Prolif^high] / Kill[Profif^low]),
        c(bquote(atop(CD56^br  + "", Tconv)),
          bquote(atop(CD56^br  + "", Treg)),
          bquote(atop(CD56^dim + "", Tconv)),
          bquote(atop(CD56^dim + "", Treg))),
        c(0.25, 2), pl3)

dotplot("2b_a", rdk(c("tcnv", "treg"), dba),
        bquote(CD56^br), c("Tconv", "Treg"), c(0, 0.5), pl1)

dotplot("2b_b", rdk(c("tcnv", "treg"), dbb),
        bquote(CD56^dim), c("Tconv", "Treg"), c(0, 0.5), pl1)

dotplot("2c", rdk(c("tcnv", "treg"), dcc), "", c("Tconv", "Treg"), col=pl1)

dotplot("4c", rdk(c("high", "low"), d4c), bquote(CD56^br+Tconv), lhl, c(0, 0.5), pl3)

dotplot("4d", rdk(c("high", "low"), d4d), bquote(CD56^br+Treg), lhl, c(0, 0.5), pl3)

dotplot("4e", rdk(c("high", "low"), d4e), bquote(CD56^dim+Tconv), lhl, c(0, 0.5), pl3)

dotplot("4f", rdk(c("high", "low"), d4f), bquote(CD56^dim+Treg), lhl, c(0, 0.5), pl3)
