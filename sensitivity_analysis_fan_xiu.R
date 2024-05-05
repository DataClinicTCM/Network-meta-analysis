library(tidyverse)
library(metafor)
library(forplo)

# SBP ---------------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = '返修240123/paired_en.xlsx', sheet = 5)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat, method="DL")
res
mydat <- leave1out(res)
mydat <- as.data.frame(mydat)

# 添加一列样本量
outcomes$SampleSize <- outcomes$n1i + outcomes$n2i
SampleSize <- outcomes$SampleSize
mydat <- cbind(mydat, SampleSize)

tiff(file = "返修240123/SBP_sensitivity.tiff", compression = 'none', width = 800, height = 500)

forplo(mydat[,c(1,5,6)],
       # add.columns = mydat$SampleSize,
       # add.colnames = c("SampleSize"),
       scaledot.by = SampleSize,
       xlim = c(-14,-9),
       em = "MD",
       ci.edge = TRUE,
       sort = TRUE,
       horiz.bar = TRUE,
       col = 8,
       margin.right = 12,
       left.align = TRUE, #标签左对齐
       char = 15, #点的形状
       # size = 2
       )

dev.off()

# DBP ---------------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = "返修240123/paired_en.xlsx", sheet = 6)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat, method="DL")
res
mydat <- leave1out(res)
mydat <- as.data.frame(mydat)

# 添加一列样本量
outcomes$SampleSize <- outcomes$n1i + outcomes$n2i
SampleSize <- outcomes$SampleSize
mydat <- cbind(mydat, SampleSize)

tiff(file = "返修240123/DBP_sensitivity.tiff", compression = 'none', width = 800, height = 500)

forplo(mydat[,c(1,5,6)],
       scaledot.by = SampleSize,
       xlim = c(-10,-5),
       em = "MD",
       ci.edge = TRUE,
       sort = TRUE,
       horiz.bar = TRUE,
       col = 8,
       margin.right = 12,
       char = 15, #点的形状
       left.align = TRUE
       # size = 3.5
       )

dev.off()

# rate --------------------------------------------------------------------
outcomes <- readxl::read_xlsx(path = "返修240123/paired_en.xlsx", sheet = 4)

dat <- escalc(measure="RR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat, method="EE")
res
mydat <- leave1out(res, transf = exp) #必须要加exp转换，因为RR默认log
mydat <- as.data.frame(mydat)

# 添加一列样本量
outcomes$SampleSize <- outcomes$n1i + outcomes$n2i
SampleSize <- outcomes$SampleSize
mydat <- cbind(mydat, SampleSize)


tiff(file = "返修240123/rate_sensitivity.tiff", compression = 'none', width = 800, height = 620)

forplo(mydat[,c(1,4,5)],
       scaledot.by = SampleSize,
       xlim = c(1.1,1.24),
       em = "RR",
       ci.edge = TRUE,
       sort = TRUE,
       horiz.bar = TRUE,
       col = 8,
       margin.right = 11,
       char = 15,
       left.align = TRUE
       )

dev.off()

# adverse -----------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = "返修240123/paired_en.xlsx", sheet = 7)

dat <- escalc(measure="RR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat, method="EE")
res
mydat <- leave1out(res, transf = exp) #必须要加exp转换，因为RR默认log
mydat <- as.data.frame(mydat)

# 添加一列样本量
outcomes$SampleSize <- outcomes$n1i + outcomes$n2i
SampleSize <- outcomes$SampleSize
mydat <- cbind(mydat, SampleSize)


tiff(file = "返修240123/adverse_sensitivity.tiff", compression = 'none', width = 800, height = 350)

forplo(mydat[,c(1,4,5)],
       scaledot.by = SampleSize,
       xlim = c(0.2,1),
       em = "RR",
       ci.edge = TRUE,
       sort = TRUE,
       horiz.bar = TRUE,
       col = 8,
       char = 15,
       left.align = TRUE,
       margin.right = 12)

dev.off()
