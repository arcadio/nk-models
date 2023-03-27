library(ggplot2)
library(ggdist)

theme_set(theme_classic(base_size=18) + theme(plot.title=element_text(hjust=0.5)))

mft <- function() function(x) format(100*x, digits=2)

fldplot <- function(id, dat, xlab, lab, xlim=NULL, col=c(180, 300)) {
    p <- ggplot(dat, aes(x=fld, y=exp, fill=after_stat(x < 1))) +
        stat_halfeye(alpha=0.5, .width=c(0.025, 0.975), linewidth=4) +
        geom_vline(xintercept=1, linetype="dashed", color="grey") +
        coord_cartesian(xlim=xlim) +
        scale_y_discrete(labels=lab) +
        scale_fill_hue(h=col) +
        labs(x=bquote(paste("Fold change ", "(", .(xlab), ")")), y="") +
        guides(fill="none")
    ggsave(paste0("tmp/fold", id, ".pdf"), p)
}

dotplot <- function(id, dat, err, tit, lab, ylim=NULL, col=c(180, 300)) {
    p <- ggplot(data=dat, mapping=aes(x=type, y=kill)) +
        geom_line(aes(group=id), alpha=0.1) +
        geom_point(size=3, aes(color=type, shape=day), alpha=0.9) +
        scale_shape_manual(values=c(15, 17)) +
        stat_halfeye(data=err, aes(fill=type), alpha=0.5, .width=c(0.025, 0.975), linewidth=4) +
        scale_x_discrete(labels=lab) +
        scale_y_continuous(labels=mft()) +
        scale_fill_hue(h=col, aesthetics=c("color", "fill")) +
        coord_cartesian(ylim=ylim) +
        labs(x="", y="Specific killing (%)", title=tit) +
        guides(color="none", fill="none", shape=guide_legend(title="Day"))
    ggsave(paste0("tmp/dot", id, ".pdf"), p)
}

pl1 <- c(0, 60)
pl2 <- c(120, 180)
pl3 <- c(240, 300)
lhl <- c(bquote(Prolif^high), bquote(Prolif^low))

fldplot("2b",
        rbind(data.frame(fld=dba$fld, exp="2b(a)"),
              data.frame(fld=dbb$fld, exp="2b(b)")),
        bquote(Kill[Tconv] / Kill[Treg]),
        c(bquote(CD56^br), bquote(CD56^dim)),
        c(0.25, 2),
        pl1)

fldplot("2c",
        rbind(data.frame(fld=dca$fld, exp="tconv"),
              data.frame(fld=dcb$fld, exp="treg"),
              data.frame(fld=dcc$fld, exp="all")),
        bquote(Kill[3.5] / Kill[2.5]),
        c("Tconv", "Treg", bquote(atop(Tconv + "", Treg))),
        c(0.25, 4),
        pl2)

fldplot("4",
        rbind(data.frame(fld=d4c$fld, exp="4c"),
              data.frame(fld=d4d$fld, exp="4d"),
              data.frame(fld=d4e$fld, exp="4e"),
              data.frame(fld=d4f$fld, exp="4f")),
        bquote(Kill[Prolif^high] / Kill[Profif^low]),
        c(bquote(atop(CD56^br  + "", Tconv)),
          bquote(atop(CD56^br  + "", Treg)),
          bquote(atop(CD56^dim + "", Tconv)),
          bquote(atop(CD56^dim + "", Treg))),
        c(0.25, 2),
        pl3)

dotplot("2b_a",
        read("2b_a"),
        rbind(data.frame(type="tcnv", kill=dba$gm1),
              data.frame(type="treg", kill=dba$gm2)),
        bquote(CD56^br),
        c("Tconv", "Treg"),
        c(0, 0.5),
        pl1)

dotplot("2b_b",
        read("2b_b"),
        rbind(data.frame(type="tcnv", kill=dbb$gm1),
              data.frame(type="treg", kill=dbb$gm2)),
        bquote(CD56^dim),
        c("Tconv", "Treg"),
        c(0, 0.5),
        pl1)

dotplot("2c",
        read("2c"),
        rbind(data.frame(type="tcnv", kill=dcc$gm1),
              data.frame(type="treg", kill=dcc$gm2)),
        "",
        c("Tconv", "Treg"),
        col=pl2)

dotplot("4c",
        read("4c"),
        rbind(data.frame(type="high", kill=d4c$gm1),
              data.frame(type="low", kill=d4c$gm2)),
        bquote(CD56^br+Tconv),
        lhl,
        c(0, 0.5),
        pl3)

dotplot("4d",
        read("4d"),
        rbind(data.frame(type="high", kill=d4d$gm1),
              data.frame(type="low", kill=d4d$gm2)),
        bquote(CD56^br+Treg),
        lhl,
        c(0, 0.5),
        pl3)

dotplot("4e",
        read("4e"),
        rbind(data.frame(type="high", kill=d4e$gm1),
              data.frame(type="low", kill=d4e$gm2)),
        bquote(CD56^dim+Tconv),
        lhl,
        c(0, 0.5),
        pl3)

dotplot("4f",
        read("4f"),
        rbind(data.frame(type="high", kill=d4f$gm1),
              data.frame(type="low", kill=d4f$gm2)),
        bquote(CD56^dim+Treg),
        lhl,
        c(0, 0.5),
        pl3)
