library(metafor)
library(tidyverse)

# 降压有效率亚组分析-----------------------------------------------------
outcomes <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 4)
table(outcomes$subgroup)

dat <- outcomes
dat$subgroup <- factor(dat$subgroup, levels = c("TMGT_CCB vs CCB",
                                                "SLXM_CCB vs CCB",
                                                "QLDX_CCB vs CCB",
                                                "XMT_CCB vs CCB",
                                                "QJDH_CCB vs CCB",
                                                "QGJY_CCB vs CCB"))

### calculate log risk ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="RR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat,
              slab=paste(author, year))
dat



### fit random-effects model
res <- rma(yi, vi, data=dat, method="EE")
res

### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                    ", df = ", .(res$k - res$p),
                    ", p ", .(metafor:::.pval(res$QEp, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}

pdf("返修/subCPM_rate.pdf", width = 8, height = 8)

forest(res, xlim=c(-5.5,2), at=log(c(0.2,1,2)), atransf=exp,
       ilab=cbind(ai, n1i, ci, n2i), ilab.xpos=c(-3.9,-3.1,-2.3,-1.5),
       cex=0.58, ylim=c(-1, 52), order=subgroup, rows=c(47:37,32:28,23:20,15:13,8:7,2),
       mlab=mlabfun("EE Model for All Studies", res),
       header="Author(s) and Year")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.55, font=2)

text(c(-3.9,-3.1,-2.3,-1.5), 50, pos=3, c("Events","Total","Events","Total"))
text(c(-3.5,-1.9), 51.5, pos=3, c("--- CPM + CCB ---","--- CCB ---"))

### switch to bold italic font
par(font=4)

text(-5.5, c(48.3,33.3,24.3,16.3,9.3,3.3), pos=4, c("TMGT_CCB vs CCB",
                                                    "SLXM_CCB vs CCB",
                                                    "QLDX_CCB vs CCB",
                                                    "XMT_CCB vs CCB",
                                                    "QJDH_CCB vs CCB",
                                                    "QGJY_CCB vs CCB"))


### set par back to the original settings
par(op)

### fit fixed-effects model in the 4 subgroups
res.1 <- rma(yi, vi, subset=(subgroup=="TMGT_CCB vs CCB"), data=dat, method = "EE")
res.2 <- rma(yi, vi, subset=(subgroup=="SLXM_CCB vs CCB"), data=dat, method="EE")
res.3 <- rma(yi, vi, subset=(subgroup=="QLDX_CCB vs CCB"), data=dat, method="EE")
res.4 <- rma(yi, vi, subset=(subgroup=="XMT_CCB vs CCB"), data=dat, method="EE")
res.5 <- rma(yi, vi, subset=(subgroup=="QJDH_CCB vs CCB"), data=dat, method="EE")
res.6 <- rma(yi, vi, subset=(subgroup=="QGJY_CCB vs CCB"), data=dat, method="EE")

### add summary polygons for the 4 subgroups
addpoly(res.1, row=35.5, mlab=mlabfun("EE Model for Subgroup", res.1))
addpoly(res.2, row= 26.5, mlab=mlabfun("EE Model for Subgroup", res.2))
addpoly(res.3, row= 18.5, mlab=mlabfun("EE Model for Subgroup", res.3))
addpoly(res.4, row= 11.5, mlab=mlabfun("EE Model for Subgroup", res.4))
addpoly(res.5, row= 5.5, mlab=mlabfun("EE Model for Subgroup", res.5))
addpoly(res.6, row= 0.5, mlab=mlabfun("EE Model for Subgroup", res.6))

### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ subgroup, data=dat, method="EE")

### add text for the test of subgroup differences
text(-5.5, -2.5, pos=4, cex=0.55, bquote(paste("Test for Subgroup Differences: ",
                                               Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                               ", p = ", .(formatC(res$QMp, digits=2, format="f")))))

dev.off()

# SBP亚组分析----------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 5)
table(outcomes$subgroup)

dat <- outcomes
dat$subgroup <- factor(dat$subgroup, levels = c("TMGT_CCB vs CCB",
                                                "SLXM_CCB vs CCB",
                                                "QLDX_CCB vs CCB",
                                                "XMT_CCB vs CCB",
                                                "QJDH_CCB vs CCB",
                                                "QGJY_CCB vs CCB"))

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res

### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                    ", df = ", .(res$k - res$p),
                    ", p ", .(metafor:::.pval(res$QEp, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}

pdf("返修/subCPM_SBP.pdf", width = 8, height = 7)

forest(res, xlim=c(-120,30), at=c(-30,-20,-10,0,5),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-95,-85,-75,-60,-50,-40),
       cex=0.55, ylim=c(-1, 44), order=subgroup, rows=c(39:35,30:26,21:19,14:13,8:7,2),
       mlab=mlabfun("DL Model for All Studies", res),
       header="Author(s) and Year")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.55, font=2)

text(c(-91,-82,-71,-56,-47,-37), 43, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-76,-41), 44.5, pos=2, c("--- CPM + CCB ---","--- CCB ---"))

### switch to bold italic font
par(font=4)

text(-120, c(40.3,31.3,22.3,15.3,9.3,3.3), pos=4, c("TMGT_CCB vs CCB",
                                                    "SLXM_CCB vs CCB",
                                                    "QLDX_CCB vs CCB",
                                                    "XMT_CCB vs CCB",
                                                    "QJDH_CCB vs CCB",
                                                    "QGJY_CCB vs CCB"))


### set par back to the original settings
par(op)

### fit random-effects model in the 6 subgroups
res.1 <- rma(yi, vi, subset=(subgroup=="TMGT_CCB vs CCB"), data=dat, method="DL")
res.2 <- rma(yi, vi, subset=(subgroup=="SLXM_CCB vs CCB"), data=dat, method="DL")
res.3 <- rma(yi, vi, subset=(subgroup=="QLDX_CCB vs CCB"), data=dat, method="DL")
res.4 <- rma(yi, vi, subset=(subgroup=="XMT_CCB vs CCB"), data=dat, method="DL")
res.5 <- rma(yi, vi, subset=(subgroup=="QJDH_CCB vs CCB"), data=dat, method="DL")
res.6 <- rma(yi, vi, subset=(subgroup=="QGJY_CCB vs CCB"), data=dat, method="DL")

### add summary polygons for the 6 subgroups
addpoly(res.1, row=33.5, mlab=mlabfun("DL Model for Subgroup", res.1))
addpoly(res.2, row= 24.5, mlab=mlabfun("DL Model for Subgroup", res.2))
addpoly(res.3, row= 17.5, mlab=mlabfun("DL Model for Subgroup", res.3))
addpoly(res.4, row= 11.5, mlab=mlabfun("DL Model for Subgroup", res.4))
addpoly(res.5, row= 5.5, mlab=mlabfun("DL Model for Subgroup", res.5))
addpoly(res.6, row= 0.5, mlab=mlabfun("DL Model for Subgroup", res.6))

### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ subgroup, data=dat)

### add text for the test of subgroup differences
text(-120, -2.5, pos=4, cex=0.55, bquote(paste("Test for Subgroup Differences: ",
                                               Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                               ", p = ", .(formatC(res$QMp, digits=2, format="f")))))

dev.off()

# DBP亚组分析-----------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 6)
table(outcomes$subgroup)

dat <- outcomes
dat$subgroup <- factor(dat$subgroup, levels = c("TMGT_CCB vs CCB",
                                                "SLXM_CCB vs CCB",
                                                "QLDX_CCB vs CCB",
                                                "XMT_CCB vs CCB",
                                                "QJDH_CCB vs CCB",
                                                "QGJY_CCB vs CCB"))

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res

### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                    ", df = ", .(res$k - res$p),
                    ", p ", .(metafor:::.pval(res$QEp, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}

pdf("返修/subCPM_DBP.pdf", width = 8, height = 7)

forest(res, xlim=c(-105,25), at=c(-15,-10,-5,0,5),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-80,-70,-60,-45,-35,-25),
       cex=0.55, ylim=c(-1, 44), order=subgroup, rows=c(39:35,30:26,21:19,14:13,8:7,2),
       mlab=mlabfun("DL Model for All Studies", res),
       header="Author(s) and Year")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.55, font=2)

text(c(-77,-68,-58,-42,-33,-23), 43, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-64,-29), 44.5, pos=2, c("--- CPM + CCB ---","--- CCB ---"))

### switch to bold italic font
par(font=4)

text(-105, c(40.3,31.3,22.3,15.3,9.3,3.3), pos=4, c("TMGT_CCB vs CCB",
                                                    "SLXM_CCB vs CCB",
                                                    "QLDX_CCB vs CCB",
                                                    "XMT_CCB vs CCB",
                                                    "QJDH_CCB vs CCB",
                                                    "QGJY_CCB vs CCB"))


### set par back to the original settings
par(op)

### fit random-effects model in the 6 subgroups
res.1 <- rma(yi, vi, subset=(subgroup=="TMGT_CCB vs CCB"), data=dat, method="DL")
res.2 <- rma(yi, vi, subset=(subgroup=="SLXM_CCB vs CCB"), data=dat, method="DL")
res.3 <- rma(yi, vi, subset=(subgroup=="QLDX_CCB vs CCB"), data=dat, method="DL")
res.4 <- rma(yi, vi, subset=(subgroup=="XMT_CCB vs CCB"), data=dat, method="DL")
res.5 <- rma(yi, vi, subset=(subgroup=="QJDH_CCB vs CCB"), data=dat, method="DL")
res.6 <- rma(yi, vi, subset=(subgroup=="QGJY_CCB vs CCB"), data=dat, method="DL")

### add summary polygons for the 6 subgroups
addpoly(res.1, row=33.5, mlab=mlabfun("DL Model for Subgroup", res.1))
addpoly(res.2, row= 24.5, mlab=mlabfun("DL Model for Subgroup", res.2))
addpoly(res.3, row= 17.5, mlab=mlabfun("DL Model for Subgroup", res.3))
addpoly(res.4, row= 11.5, mlab=mlabfun("DL Model for Subgroup", res.4))
addpoly(res.5, row= 5.5, mlab=mlabfun("DL Model for Subgroup", res.5))
addpoly(res.6, row= 0.5, mlab=mlabfun("DL Model for Subgroup", res.6))

### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ subgroup, data=dat)

### add text for the test of subgroup differences
text(-105, -2.5, pos=4, cex=0.55, bquote(paste("Test for Subgroup Differences: ",
                                               Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                               ", p = ", .(formatC(res$QMp, digits=2, format="f")))))

dev.off()

# 不良反应发生率meta分析-------------------------------------------

dat <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 7)

dat <- escalc(measure="RR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat,
              slab=paste(" ", author, year))
dat

### fit fixed-effects model
res <- rma(yi, vi, data=dat)
res

pdf("返修/adverse.pdf", width = 9, height = 4, family = "GB1")

forest(res, atransf=exp, xlim=c(-7,3),at=log(c(.1, 1, 4)),
       ilab=cbind(ai, n1i, ci, n2i), ilab.xpos=c(-5.8,-5,-4.2,-3.4),
       cex=.6, header="Author and Year", mlab="", showweights = TRUE, order = yi)

op <- par(cex=.6, font=2)
text(c(-5.8,-5,-4.2,-3.4,1.6), res$k+2,
     c("Event", "Total", "Event", "Total", "Weight"))
text(c(-5.5,-3.9), res$k+3, c("—— CPM + CCB ——", "—— CCB ——"))
par(op)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-7, -1, pos=4, cex=0.7, bquote(paste("RE Model (Q = ",
                                          .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                          ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                          .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

# Contour-Enhanced Funnel Plot--------------------------------------------------
dat1 <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 4)
dat2 <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 5)
dat3 <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 6)

dat1 <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat1,
               slab=paste(" ", author, year))

dat2 <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat2, slab=paste(author, year))

dat3 <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
               sd1i = sd1i, sd2i = sd2i, data=dat3, slab=paste(author, year))



### fit random-effects model
res1 <- rma(yi, vi, data=dat1, method="FE")
res2 <- rma(yi, vi, data=dat2, method="DL")
res3 <- rma(yi, vi, data=dat3, method="DL")

par(mfrow=c(1,3))
### create contour enhanced funnel plot (with funnel centered at 0)
funnel(res1, 
       level=c(90, 95, 99), 
       shade=c("white", "gray55", "gray75"), 
       refline=0, 
       legend="topleft",
       main = "Rate")

funnel(res2, 
       level=c(90, 95, 99), 
       shade=c("white", "gray55", "gray75"), 
       refline=0, 
       legend="topleft",
       main = "SBP")

funnel(res3, 
       level=c(90, 95, 99), 
       shade=c("white", "gray55", "gray75"), 
       refline=0, 
       legend="topleft",
       main = "DBP")
# Plot of Influence Diagnostics ---------------------------

### calculate influence diagnostics
inf1 <- influence(res1)
### plot the influence diagnostics
plot(inf1, layout=c(4,2))

inf2 <- influence(res2)
plot(inf2, layout=c(4,2))

inf3 <- influence(res3)
plot(inf3, layout=c(4,2))









