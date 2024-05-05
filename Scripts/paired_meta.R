library(metafor)

# Forest Plot in BMJ Style ------------------------------------------------

dat <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 3)
### create dataset (from Goldenberg et al., 2021; http://dx.doi.org/10.1136/bmj.m4743)
# dat <- data.frame(author = c("Dyson", "Jönsson", "Morris", "Saslow", "Saslow", "Sato", "Tay", "Yamada"),
#                   year   = c(2010, 2009, 2019, 2014, 2017, 2017, 2014, 2014),
#                   ai     = c(3, 6, 11, 8, 6, 4, 36, 2),
#                   n1i    = c(6, 6, 21, 9, 11, 22, 46, 12),
#                   ci     = c(1, 3, 0, 5, 0, 0, 30, 2),
#                   n2i    = c(6, 6, 12, 13, 8, 27, 47, 12))

### calculate RD and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="RD", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat,
              slab=paste(" ", author, year))
dat

### fit fixed-effects model
res <- rma(yi, vi, data=dat, method="FE")
res

############################################################################

### colors to be used in the plot
colp <- "#5C8286"
coll <- "#a7a9ac"
    
### total number of studies
k <- nrow(dat)
  
### generate point sizes
psize <- weights(res)
psize <- 1.2 + (psize - min(psize)) / (max(psize) - min(psize))
  
### get the weights and format them as will be used in the forest plot
weights <- formatC(weights(res), format="f", digits=1)
  
### adjust the margins
par(mar=c(2.7,3.2,2.3,1.3), mgp=c(3,0,0), tcl=0.15)
  
### forest plot with extra annotations
sav <- forest(dat$yi, dat$vi, xlim=c(-3.4,2.1), ylim=c(-0.5,k+3), alim=c(-1,1), cex=0.88,
                pch=18, psize=psize, efac=0, refline=NA, lty=c(1,0), xlab="",
                ilab=cbind(paste(dat$x1i, "/", dat$n1i), paste(dat$x2i, "/", dat$n2i), weights),
                ilab.xpos=c(-1.9,-1.3,1.2), annosym=c(" (", " to ", ")"),
                rowadj=-.07)
  
### add vertical reference line at 0
segments(0, -1, 0, k+1.6, col=coll)
  
### add vertical reference line at the pooled estimate
segments(coef(res), 0, coef(res), k, col=colp, lty="33", lwd=0.8)
  
### redraw the CI lines and points in the chosen color
segments(summary(dat)$ci.lb, k:1, summary(dat)$ci.ub, k:1, col=colp, lwd=1.5)
points(dat$yi, k:1, pch=18, cex=psize*1.15, col="white")
points(dat$yi, k:1, pch=18, cex=psize, col=colp)
  
### add the summary polygon
addpoly(res, row=0, mlab="Total (95% CI)", efac=2, col=colp, border=colp)
  
### add horizontal line at the top
abline(h=k+1.6, col=coll)
  
### redraw the x-axis in the chosen color
axis(side=1, at=seq(-1,1,by=0.5), col=coll, labels=FALSE)
  
### now we add a bunch of text; since some of the text falls outside of the
### plot region, we set xpd=NA so nothing gets clipped
par(xpd=NA)
  
### adjust cex as used in the forest plot and use a bold font
par(cex=sav$cex, font=2)
  
### add headings
text(sav$xlim[1], k+2.5, pos=4, "   Study")
text(sav$ilab.xpos[1:2], k+2.3, c("Experimental","Control"))
text(mean(sav$ilab.xpos[1:2]), k+3.4, "No of events / total")
text(0, k+2.7, "Risk difference,\nfixed (95% CI)")
segments(sav$ilab.xpos[1]-0.22, k+2.8, sav$ilab.xpos[2]+0.13, k+2.8)
text(c(sav$ilab.xpos[3],sav$xlim[2]-0.35), k+2.7, c("Weight\n(%)","Risk difference,\nfixed (95% CI)"))
  
### add 'Favours caffeine'/'Favours decaf' text below the x-axis
text(c(-1,1), -3, c("Control","Experimental"), pos=c(4,2), offset=-0.3)
  
### use a non-bold font for the rest of the text
par(cex=sav$cex, font=1)
  
### add the 100.0 for the sum of the weights
text(sav$ilab.xpos[3], 0, "100.0")
  
### add the column totals for the counts and sample sizes
text(sav$ilab.xpos[1:2], 0, c(paste(sum(dat$x1i), "/", sum(dat$n1i)), paste(sum(dat$x2i), "/", sum(dat$n2i))))
  
### add text with heterogeneity statistics
text(sav$xlim[1], -1, pos=4, bquote(paste("Test for heterogeneity: ", tau^2, "=",
                                            .(formatC(res$tau2, digits=2, format="f")), "; ", chi^2, "=",
                                            .(formatC(res$QE, digits=2, format="f")), ", df=", .(res$k - res$p),
                                            ", P=", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, "=",
                                            .(formatC(res$I2, digits=0, format="f")), "%")))
  
### add text for test of overall effect
text(sav$xlim[1], -2, pos=4, bquote(paste("Test for overall effect: Z=",
                                            .(formatC(res$zval, digits=2, format="f")),
                                            ", P", .(ifelse(res$pval<.001, "<0.001",
                                                            paste0("=",formatC(res$pval, digits=2, format="f")))))))

# funnel plot -------------------------------------------------------------

library(metafor)

dat1 <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 1)
dat2 <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 2)
dat3 <- readxl::read_xlsx(path = 'data/paired_en.xlsx', sheet = 3)

dat1 <- escalc(measure="OR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat1,
              slab=paste(" ", author, year))

dat2 <- escalc(measure="OR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat2,
               slab=paste(" ", author, year))

dat3 <- escalc(measure="OR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat3,
               slab=paste(" ", author, year))

res1 <- rma(yi, vi, data=dat1, method="FE")
res2 <- rma(yi, vi, data=dat2, method="FE")
res3 <- rma(yi, vi, data=dat3, method="FE")

par(mfrow=c(1,3))

funnel(res1, main="ACEI")
funnel(res2, main="ARB")
funnel(res3, main="CCB")


# Galbraith plot ----------------------------------------------------------

### adjust margins so the space is better used
par(mar=c(5,4,0,3))

radial(res1, main="ACEI")
radial(res2, main="ARB")
radial(res3, main="CCB")


# -------------------------------------------------------------------------

### fit random-effects model
res <- rma(ai=x1i, bi=n1i-x1i, ci=x2i, di=n2i-x2i, data=dat3, measure="RR")

par(mar=c(5,4,2,3))
### draw L'Abbé plot
labbe(res)
