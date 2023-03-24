library(ggplot2)
library(ggdist)

theme_set(theme_classic(base_size=18))

mft <- function() function(x) format(100*x, digits=2)

pstplot <- function(id, dat, xlim=NULL) {
    p <- ggplot(dat, aes(x=fld, y=exp, fill=after_stat(x < 1))) +
        stat_halfeye(alpha=0.5, .width=c(0.025, 0.975), linewidth=4) +
        geom_vline(xintercept=1, linetype="dashed", color="grey") +
        coord_cartesian(xlim=xlim) +
        labs(x="Fold change", y="") +
        guides(fill="none")
    ggsave(paste0("tmp/fold", id, ".pdf"), p)
}

dotplot <- function(id, dat, err, lab, ylim=NULL) {
    p <- ggplot(data=dat, mapping=aes(x=type, y=kill)) +
        geom_line(aes(group=id), alpha=0.1) +
        geom_point(size=3, aes(color=type, shape=day), alpha=0.9) +
        scale_shape_manual(values=c(15, 17)) +
        stat_halfeye(data=err, aes(fill=type), alpha=0.5, .width=c(0.025, 0.975), linewidth=4) +
        scale_x_discrete(labels=lab) +
        scale_y_continuous(labels=mft()) +
        coord_cartesian(ylim=ylim) +
        labs(x="", y="Specific kill (%)") +
        guides(color="none", fill="none", shape=guide_legend(title="Day"))
    ggsave(paste0("tmp/dot", id, ".pdf"), p)
}

pstplot("2b",
        rbind(data.frame(fld=dba$fld, exp="2b(a)"),
              data.frame(fld=dbb$fld, exp="2b(b)")),
        c(0.25, 2))

pstplot("2c",
        rbind(data.frame(fld=dca$fld, exp="tconv"),
              data.frame(fld=dcb$fld, exp="treg"),
              data.frame(fld=dcc$fld, exp="all")),
        c(0.25, 4))

pstplot("4",
        rbind(data.frame(fld=d4c$fld, exp="4c"),
              data.frame(fld=d4d$fld, exp="4d"),
              data.frame(fld=d4e$fld, exp="4e"),
              data.frame(fld=d4f$fld, exp="4f")),
        c(0.25, 2))

lhl <- c(expression("Prolif"^"high"), expression("Prolif"^"low"))

dotplot("2b_a",
        read("2b_a"),
        rbind(data.frame(type="tcnv", kill=dba$gm1),
              data.frame(type="treg", kill=dba$gm2)),
        c("Tconv", "Treg"),
        c(0, 0.5))

dotplot("2b_b",
        read("2b_b"),
        rbind(data.frame(type="tcnv", kill=dbb$gm1),
              data.frame(type="treg", kill=dbb$gm2)),
        c("Tconv", "Treg"),
        c(0, 0.5))

dotplot("2c",
        read("2c"),
        rbind(data.frame(type="tcnv", kill=dcc$gm1),
              data.frame(type="treg", kill=dcc$gm2)),
        c("Tconv", "Treg"))

dotplot("4c",
        read("4c"),
        rbind(data.frame(type="high", kill=d4c$gm1),
              data.frame(type="low", kill=d4c$gm2)),
        lhl,
        c(0, 0.5))

dotplot("4d",
        read("4d"),
        rbind(data.frame(type="high", kill=d4d$gm1),
              data.frame(type="low", kill=d4d$gm2)),
        lhl,
        c(0, 0.5))

dotplot("4e",
        read("4e"),
        rbind(data.frame(type="high", kill=d4e$gm1),
              data.frame(type="low", kill=d4e$gm2)),
        lhl,
        c(0, 0.5))

dotplot("4f",
        read("4f"),
        rbind(data.frame(type="high", kill=d4f$gm1),
              data.frame(type="low", kill=d4f$gm2)),
        lhl,
        c(0, 0.5))
