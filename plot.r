library(ggplot2)
library(ggridges)

pstplot <- function(id, dat, xlim=NULL) {
    p <- ggplot(dat, aes(x=fld, y=exp, fill=after_stat(x))) +
        geom_density_ridges_gradient(alpha=0.8, color="grey") +
        scale_fill_viridis_c() +
        geom_vline(xintercept=1, linetype="dashed", color="red") +
        coord_cartesian(xlim=xlim) +
        labs(x="Fold change", y="") +
        theme_ridges(center=T) +
        guides(fill="none")
    ggsave(paste0("tmp/fold", id, ".pdf"), p)
}

dotplot <- function(id, dat, err, ylim=NULL) {
    p <- ggplot(data=dat, mapping=aes(x=type, y=kill)) +
        geom_violin(data=err, draw_quantiles=c(0.5), color="darkgrey") +
        geom_line(aes(group=id), alpha=0.1) +
        geom_point(size=2, aes(color=type, shape=day)) +
        coord_cartesian(ylim=ylim) +
        labs(x="", y="Kill rate") +
        theme_classic() +
        guides(color="none")
    ggsave(paste0("tmp/dot", id, ".pdf"), p)
}

pstplot("2b", rbind(data.frame(fld=dba$fld, exp="2b(a)"),
                    data.frame(fld=dbb$fld, exp="2b(b)")), c(0.25, 2))

pstplot("2c", rbind(data.frame(fld=dca$fld, exp="tconv"),
                    data.frame(fld=dcb$fld, exp="treg"),
                    data.frame(fld=dcc$fld, exp="all")), c(0.25, 2))

pstplot("4",  rbind(data.frame(fld=d4c$fld, exp="4c"),
                    data.frame(fld=d4d$fld, exp="4d"),
                    data.frame(fld=d4e$fld, exp="4e"),
                    data.frame(fld=d4f$fld, exp="4f")), c(0.25, 2))

dotplot("2b_a", read("2b_a"), rbind(data.frame(type="tcnv", kill=dba$gm1),
                                    data.frame(type="treg", kill=dba$gm2)),
        c(0, 0.5))

dotplot("2b_b", read("2b_b"), rbind(data.frame(type="tcnv", kill=dbb$gm1),
                                    data.frame(type="treg", kill=dbb$gm2)),
        c(0, 0.5))

dotplot("2c", read("2c"), rbind(data.frame(type="tcnv", kill=dcc$gm1),
                                data.frame(type="treg", kill=dcc$gm2)))

dotplot("4c", read("4c"), rbind(data.frame(type="high", kill=d4c$gm1),
                                data.frame(type="low", kill=d4c$gm2)),
        c(0, 0.5))

dotplot("4d", read("4d"), rbind(data.frame(type="high", kill=d4d$gm1),
                                data.frame(type="low", kill=d4d$gm2)),
        c(0, 0.5))

dotplot("4e", read("4e"), rbind(data.frame(type="high", kill=d4e$gm1),
                                data.frame(type="low", kill=d4e$gm2)),
        c(0, 0.5))

dotplot("4f", read("4f"), rbind(data.frame(type="high", kill=d4f$gm1),
                                data.frame(type="low", kill=d4f$gm2)),
        c(0, 0.5))
