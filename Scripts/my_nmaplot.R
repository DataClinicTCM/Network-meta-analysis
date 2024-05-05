library(nmaplateplot)
library(ggplot2)


Point_estimates <- readxl::read_xlsx("output/nmaplot/有效率_不良反应_ARB.xlsx", sheet = 1)
Interval_estimates_LB <- readxl::read_xlsx("output/nmaplot/有效率_不良反应_ARB.xlsx", sheet = 2)
Interval_estimates_UB <- readxl::read_xlsx("output/nmaplot/有效率_不良反应_ARB.xlsx", sheet = 3)
Pvalues <- readxl::read_xlsx("output/nmaplot/有效率_不良反应_ARB.xlsx", sheet = 4)
Treatment_specific_values <- readxl::read_xlsx("output/nmaplot/有效率_不良反应_ARB.xlsx", sheet = 5)

Point_estimates$v3 <- as.numeric(Point_estimates$v3)

Point_estimates <- as.data.frame(Point_estimates)
Interval_estimates_LB <- as.data.frame(Interval_estimates_LB)
Interval_estimates_UB <- as.data.frame(Interval_estimates_UB)
Pvalues <- as.data.frame(Pvalues)
Treatment_specific_values <- as.data.frame(Treatment_specific_values)

nma_res_minimum <- list()

nma_res_minimum$Point_estimates <- Point_estimates
nma_res_minimum$Interval_estimates_LB <- Interval_estimates_LB
nma_res_minimum$Interval_estimates_UB <- Interval_estimates_UB
nma_res_minimum$Pvalues <- Pvalues
nma_res_minimum$Treatment_specific_values <- Treatment_specific_values

# The text output based on minimum input

plateplot(nma_res_minimum, design_method = c("text", "text"),
          null_value_zero = c(FALSE, FALSE), lower_better = c(TRUE, FALSE),
          transform_rc_ullr_boolean = F, max_substring = 10,
          text_size = 3, bold = FALSE,
          upper_diagonal_name = "ad_event", lower_diagonal_name = "JY_rate",
          diagonal_color = c("#EAFFD0", "#f2f4f6"), offdiagonal_color = c("#fae3d9", "#bbded6"))

ggsave(filename = "有效率不良反应CCB.tiff", width = 12, height = 6, dpi = 300)

# function-------------------------------------------------------------------------

my_nmaplot <- function(filename){
  Point_estimates <- readxl::read_xlsx(paste0("output/nmaplot/", filename, ".xlsx"), sheet = 1)
  Interval_estimates_LB <- readxl::read_xlsx(paste0("output/nmaplot/", filename, ".xlsx"), sheet = 2)
  Interval_estimates_UB <- readxl::read_xlsx(paste0("output/nmaplot/", filename, ".xlsx"), sheet = 3)
  Pvalues <- readxl::read_xlsx(paste0("output/nmaplot/", filename, ".xlsx"), sheet = 4)
  Treatment_specific_values <- readxl::read_xlsx(paste0("output/nmaplot/", filename, ".xlsx"), sheet = 5)
  
  Point_estimates$v3 <- as.numeric(Point_estimates$v3)
  
  Point_estimates <- as.data.frame(Point_estimates)
  Interval_estimates_LB <- as.data.frame(Interval_estimates_LB)
  Interval_estimates_UB <- as.data.frame(Interval_estimates_UB)
  Pvalues <- as.data.frame(Pvalues)
  Treatment_specific_values <- as.data.frame(Treatment_specific_values)
  
  nma_res_minimum <- list()
  
  nma_res_minimum$Point_estimates <- Point_estimates
  nma_res_minimum$Interval_estimates_LB <- Interval_estimates_LB
  nma_res_minimum$Interval_estimates_UB <- Interval_estimates_UB
  nma_res_minimum$Pvalues <- Pvalues
  nma_res_minimum$Treatment_specific_values <- Treatment_specific_values
  
  # The text output based on minimum input
  
  plateplot(nma_res_minimum, design_method = c("text", "text"),
            # null_value_zero = c(FALSE, FALSE), lower_better = c(TRUE, FALSE),
            transform_rc_ullr_boolean = F, max_substring = 10,
            text_size = 3.5, bold = FALSE,
            upper_diagonal_name = "ad", lower_diagonal_name = "JY_rate", #注意修改名称
            diagonal_color = c("#EAFFD0", "#f2f4f6"), offdiagonal_color = c("#fae3d9", "#bbded6"))
  
  ggsave(filename = paste0("output/nmaplot/", filename, ".pdf"), width = 10, height = 6)
}

# -------------------------------------------------------------------------

my_nmaplot("SBP_DBP_ACEI")
my_nmaplot("SBP_DBP_ARB")
my_nmaplot("SBP_DBP_CCB")

my_nmaplot("有效率_不良反应_ACEI")
my_nmaplot("有效率_不良反应_ARB")
my_nmaplot("有效率_不良反应_CCB")


