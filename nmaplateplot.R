
# Quick example -----------------------------------------------------------

library(nmaplateplot)
library(svglite)
data("ad12.eff.acc")
plateplot(ad12.eff.acc, design_method = c("circle", "circle"),
          upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

# option design_method controls whether you want upper and lower diagonal parts to display "text" or "circle" respectively.
plateplot(ad12.eff.acc, design_method = c("text", "text"),
          upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

# Input dataset -----------------------------------------------------------

# The input dataset must be a list containing at least 5 data frames named: 
# Point_estimates, Interval_estimates_LB, Interval_estimates_UB, Pvalues, 
# and Treatment_specific_values.

nma_res_minimum <- ad12.eff.acc
nma_res_minimum$Treatment_specific_values$Value_Upper <- NULL
nma_res_minimum$Treatment_specific_values$Value_Lower <- NULL
# This is the minimum input for the plateplot function
nma_res_minimum$Treatment_specific_values

round(nma_res_minimum$Point_estimates,2)
round(nma_res_minimum$Interval_estimates_LB,2)
round(nma_res_minimum$Interval_estimates_UB,2)
round(nma_res_minimum$Pvalues,3)
# The text output based on minimum input
plateplot(nma_res_minimum, design_method = c("text", "text"),
          upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

# Two types of layouts: row-column and upper-left lower-right -------------

plateplot(nma_res_minimum, design_method = c("text", "text"),
          transform_rc_ullr_boolean = FALSE,
          upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

plateplot(nma_res_minimum, design_method = c("text", "text"),
          transform_rc_ullr_boolean = TRUE,
          upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

# SUCRA values ------------------------------------------------------------

# only show SUCRA for upper diagonal part
nma_res_SUCRA_LD <- ad12.eff.acc
nma_res_SUCRA_LD$Treatment_specific_values$Value_Lower <- NULL
plateplot(nma_res_SUCRA_LD, design_method = c("circle", "circle"),
          transform_rc_ullr_boolean = TRUE,
          upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")
# only show SUCRA for lower diagonal part
nma_res_SUCRA_UD <- ad12.eff.acc
nma_res_SUCRA_UD$Treatment_specific_values$Value_Upper <- NULL
plateplot(nma_res_SUCRA_UD, design_method = c("circle", "circle"),
          transform_rc_ullr_boolean = TRUE,
          upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")
# show both, but they are different
nma_res_SUCRA_Bothd <- ad12.eff.acc
plateplot(nma_res_SUCRA_Bothd, design_method = c("circle", "circle"),
          transform_rc_ullr_boolean = TRUE,
          upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")
# show both and they are same
nma_res_SUCRA_Boths <- ad12.eff.acc
nma_res_SUCRA_Boths$Treatment_specific_values$Value_Lower <- 
  nma_res_SUCRA_Boths$Treatment_specific_values$Value_Upper
plateplot(nma_res_SUCRA_Boths, design_method = c("circle", "circle"),
          transform_rc_ullr_boolean = TRUE,
          upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

# saving plot -------------------------------------------------------------

nma_res_minimum <- plateplot(nma_res_minimum, design_method = c("text", "text"),
                             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")
ggsave("nma_res_minimum.pdf", plot = nma_res_minimum, device = "pdf",
       width = 10, height = 6, dpi = 600)
ggsave("nma_res_minimum.tiff", plot = nma_res_minimum, device = "tiff", compression = "lzw",
       width = 10, height = 6, dpi = 600)
ggsave("nma_res_minimum.svg", plot = nma_res_minimum, device = "svg",
       width = 10, height = 6, dpi = 600)

# -------------------------------------------------------------------------

library(gemtc)
library(tidyverse)
library(readxl)

mydata_SBP <- readxl::read_xlsx(path = 'data/Outcome_indicators.xlsx', sheet = 1)
string <- unique(mydata_SBP$treatment) #看一下有多少不一样的干预
length(unique(mydata_SBP$treatment))

pattern <- "[A-Z]{0,}[_]?ACEI" #ACEI的干预
intervene <- str_subset(string, pattern)
mydata_SBP[mydata_SBP$treatment %in% intervene,] #提取























