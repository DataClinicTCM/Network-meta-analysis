library(metafor)
library(tidyverse)

# SBP subgroup ------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 1)

table(outcomes$CPM)

dat <- outcomes
dat$CPM <- factor(dat$CPM, levels = c("SLXM","QLDX","TMGT","XMT","QGJY","QJDH"))
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

pdf("output/subCPM_SBP.pdf", width = 10, height = 11, family = "GB1")

forest(res, xlim=c(-140,30), at=c(-50,-40,-30,-20,-10,0,10),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-110,-100,-90,-75,-65,-55),
       cex=0.55, ylim=c(-1, 77), order=CPM, rows=c(73:54,49:45,40:30,25:16,11:10,5:2),
       mlab=mlabfun("DL Model for All Studies", res),
       header="Author(s) and Year")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.55, font=2)

text(c(-106,-96,-86,-71,-61,-51), 76, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-94,-59), 77.5, pos=2, c("---试验组---","---对照组---"))

### switch to bold italic font
par(font=4)

text(-140, c(74.3,50.3,41.3,26.3,12.3,6.3), pos=4, c("松龄血脉康胶囊+西药vs西药",
                                                     "强力定眩片+西药vs西药",
                                                     "天麻钩藤颗粒+西药vs西药",
                                                     "心脉通胶囊+西药vs西药",
                                                     "清肝降压胶囊+西药vs西药",
                                                     "杞菊地黄丸+西药vs西药"))


### set par back to the original settings
par(op)

### fit random-effects model in the 6 subgroups
res.1 <- rma(yi, vi, subset=(CPM=="SLXM"), data=dat, method="DL")
res.2 <- rma(yi, vi, subset=(CPM=="QLDX"), data=dat, method="DL")
res.3 <- rma(yi, vi, subset=(CPM=="TMGT"), data=dat, method="DL")
res.4 <- rma(yi, vi, subset=(CPM=="XMT"), data=dat, method="DL")
res.5 <- rma(yi, vi, subset=(CPM=="QGJY"), data=dat, method="DL")
res.6 <- rma(yi, vi, subset=(CPM=="QJDH"), data=dat, method="DL")

### add summary polygons for the 6 subgroups
addpoly(res.1, row=52.5, mlab=mlabfun("DL Model for Subgroup", res.1))
addpoly(res.2, row= 43.5, mlab=mlabfun("DL Model for Subgroup", res.2))
addpoly(res.3, row= 28.5, mlab=mlabfun("DL Model for Subgroup", res.3))
addpoly(res.4, row= 14.5, mlab=mlabfun("DL Model for Subgroup", res.4))
addpoly(res.5, row= 8.5, mlab=mlabfun("DL Model for Subgroup", res.5))
addpoly(res.6, row= 0.5, mlab=mlabfun("DL Model for Subgroup", res.6))

### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ CPM, data=dat)

### add text for the test of subgroup differences
text(-140, -2.5, pos=4, cex=0.55, bquote(paste("Test for Subgroup Differences: ",
                                               Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                               ", p = ", .(formatC(res$QMp, digits=2, format="f")))))

dev.off()

# DBP subgroup ------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 2)

table(outcomes$CPM)

dat <- outcomes
dat$CPM <- factor(dat$CPM, levels = c("SLXM","QLDX","TMGT","XMT","QGJY","QJDH"))
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

pdf("output/subCPM_DBP.pdf", width = 10, height = 11, family = "GB1")

forest(res, xlim=c(-110,25), at=c(-30,-20,-10,0,10),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-83,-73,-63,-53,-43,-35),
       cex=0.55, ylim=c(-1, 77), order=CPM, rows=c(73:54,49:45,40:30,25:16,11:10,5:2),
       mlab=mlabfun("DL Model for All Studies", res),
       header="Author(s) and Year")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.55, font=2)

text(c(-79,-69,-59,-49,-39,-31), 76, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-67,-37), 77.5, pos=2, c("---试验组---","---对照组---"))

### switch to bold italic font
par(font=4)

text(-110, c(74.3,50.3,41.3,26.3,12.3,6.3), pos=4, c("松龄血脉康胶囊+西药vs西药",
                                                     "强力定眩片+西药vs西药",
                                                     "天麻钩藤颗粒+西药vs西药",
                                                     "心脉通胶囊+西药vs西药",
                                                     "清肝降压胶囊+西药vs西药",
                                                     "杞菊地黄丸+西药vs西药"))


### set par back to the original settings
par(op)

### fit random-effects model in the 4 subgroups
res.1 <- rma(yi, vi, subset=(CPM=="SLXM"), data=dat)
res.2 <- rma(yi, vi, subset=(CPM=="QLDX"), data=dat)
res.3 <- rma(yi, vi, subset=(CPM=="TMGT"), data=dat)
res.4 <- rma(yi, vi, subset=(CPM=="XMT"), data=dat)
res.5 <- rma(yi, vi, subset=(CPM=="QGJY"), data=dat)
res.6 <- rma(yi, vi, subset=(CPM=="QJDH"), data=dat)

### add summary polygons for the 4 subgroups
addpoly(res.1, row=52.5, mlab=mlabfun("DL Model for Subgroup", res.1))
addpoly(res.2, row= 43.5, mlab=mlabfun("DL Model for Subgroup", res.2))
addpoly(res.3, row= 28.5, mlab=mlabfun("DL Model for Subgroup", res.3))
addpoly(res.4, row= 14.5, mlab=mlabfun("DL Model for Subgroup", res.4))
addpoly(res.5, row= 8.5, mlab=mlabfun("DL Model for Subgroup", res.5))
addpoly(res.6, row= 0.5, mlab=mlabfun("DL Model for Subgroup", res.6))

### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ CPM, data=dat)

### add text for the test of subgroup differences
text(-110, -2.5, pos=4, cex=0.55, bquote(paste("Test for Subgroup Differences: ",
                                               Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                               ", p = ", .(formatC(res$QMp, digits=2, format="f")))))

dev.off()

# rate --------------------------------------------------------------------
library(metafor)

outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 3)
table(outcomes$CPM)
dat <- outcomes

dat$CPM <- factor(dat$CPM, levels = c("SLXM","QLDX","TMGT","XMT","QGJY","QJDH"))

dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat,
              slab=paste(author, year))
dat

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

pdf("output/subCPM_rate.pdf", width = 7, height = 9, family = "GB1")

forest(res, xlim=c(-5.5,2), at=log(c(.3, 1, 3)), atransf=exp,
       ilab=cbind(x1i, n1i, x2i, n2i), ilab.xpos=c(-3.9,-3.1,-2.3,-1.5),
       cex=0.55, ylim=c(-1, 78), order=CPM, rows=c(74:57,52:46,41:26,21:13,8,3:2),
       mlab=mlabfun("EE Model for All Studies", res),
       header="Author(s) and Year")


### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.55, font=2)

text(c(-3.9,-3.1,-2.3,-1.5), 75, pos=3, c("Events","Total","Events","Total"))
text(c(-3.5,-1.9), 77.5, pos=3, c("---试验组---","---对照组---"))

### switch to bold italic font
par(font=4)

text(-5.5, c(75.3,53.3,42.3,22.3,9.3,4.3), pos=4, c("松龄血脉康胶囊+西药vs西药",
                                                     "强力定眩片+西药vs西药",
                                                     "天麻钩藤颗粒+西药vs西药",
                                                     "心脉通胶囊+西药vs西药",
                                                     "清肝降压胶囊+西药vs西药",
                                                     "杞菊地黄丸+西药vs西药"))


### set par back to the original settings
par(op)

### fit fixed-effects model in the 4 subgroups
res.1 <- rma(yi, vi, subset=(CPM=="SLXM"), data=dat, method = "EE")
res.2 <- rma(yi, vi, subset=(CPM=="QLDX"), data=dat, method="EE")
res.3 <- rma(yi, vi, subset=(CPM=="TMGT"), data=dat, method="EE")
res.4 <- rma(yi, vi, subset=(CPM=="XMT"), data=dat, method="EE")
res.5 <- rma(yi, vi, subset=(CPM=="QGJY"), data=dat, method="EE")
res.6 <- rma(yi, vi, subset=(CPM=="QJDH"), data=dat, method="EE")

### add summary polygons for the 4 subgroups
addpoly(res.1, row=55.5, mlab=mlabfun("EE Model for Subgroup", res.1))
addpoly(res.2, row= 44.5, mlab=mlabfun("EE Model for Subgroup", res.2))
addpoly(res.3, row= 24.5, mlab=mlabfun("EE Model for Subgroup", res.3))
addpoly(res.4, row= 11.5, mlab=mlabfun("EE Model for Subgroup", res.4))
addpoly(res.5, row= 7, mlab=mlabfun("EE Model for Subgroup", res.5))
addpoly(res.6, row= 0.5, mlab=mlabfun("EE Model for Subgroup", res.6))

### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ CPM, data=dat, method="EE")

### add text for the test of subgroup differences
text(-5.5, -2.5, pos=4, cex=0.55, bquote(paste("Test for Subgroup Differences: ",
                                               Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                               ", p = ", .(formatC(res$QMp, digits=2, format="f")))))

dev.off()


# adverse -----------------------------------------------------------------
library(metafor)

outcomes <- readxl::read_xlsx(path = 'data/meta subgroup.xlsx', sheet = 4)
dat <- outcomes

### calculate log risk ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat,
              slab=paste(author, year), drop00 = TRUE)
dat

### fit random-effects model
res <- rma(yi, vi, data=dat, method="EE")
res

options(na.action = "na.pass")

pdf("output/adverse.pdf", width = 7, height = 5.5, family = "GB1")
### forest plot with extra annotations
forest(res, atransf=exp, at=log(c(.01,.1, 1, 4)), xlim=c(-10.5,3.5),
       ilab=cbind(x1i, n1i, x2i, n2i), ilab.xpos=c(-8,-7,-6,-5),
       cex=.55, header="Author(s) and Year", mlab="")

op <- par(cex=.55, font=2)
text(3.3, c(8,26), "Not estimable", pos=2)



text(c(-8,-7,-6,-5), 28, pos=3, c("Events","Total","Events","Total"))
text(c(-7.5,-5.5), 29, pos=3, c("---试验组---","---对照组---"))

par(op)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-10.5, -1, pos=4, cex=0.55, bquote(paste("EE Model (Q = ",
                                              .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                              ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                              .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

