library(tidyverse)

# pheatmap ----------------------------------------------------------------

library(pheatmap)
## 颜色包 viridisLite
library(viridisLite)
library(RColorBrewer)

data <- readxl::read_xlsx(path = 'data/all_sucra_new.xlsx', sheet = 2)
data <- column_to_rownames(data, var = 'outcome')

group <- c(rep('ACEI',4), rep('ARB',5), rep('CCB',7)) 
group <- data.frame(group)
rownames(group) <- colnames(data)

for (i in 1:length(colnames(data))) {
  data[[i]] <- round(data[[i]], 4)
}

for (i in 1:length(colnames(data))) {
  data[[i]] <- ifelse(data[[i]] == 0, NA, data[[i]])
}

pheatmap(data, #热图的数据
         cluster_rows = FALSE,#行聚类
         cluster_cols = FALSE,#列聚类，可以看出样本之间的区分度
         annotation_col =group, #标注样本分类
         annotation_legend=FALSE, # 显示注释
         show_rownames = TRUE,# 显示行名
         show_colnames = TRUE, 
         scale = "none", #以行来标准化，这个功能很不错
         display_numbers = TRUE,
         number_format = "%s",
         na_col = 'white',
         color = colorRampPalette(brewer.pal(n = 7, name ="RdYlBu"))(100),
         fontsize = 9,
         gaps_col = c(4,9),
         angle_col = 45,
         annotation_colors = list(group = c('#1B6AA5', '#629460', '#FF9933'))
)

# complexheatmap ----------------------------------------------------------

library(ComplexHeatmap)
library(circlize)

data <- readxl::read_xlsx(path = 'data/all_sucra_new.xlsx', sheet = 2) #0作为NA
data <- column_to_rownames(data, var = 'outcome')

group <- c(rep('ACEI',4), rep('ARB',5), rep('CCB',7)) 
group <- data.frame(group)
rownames(group) <- colnames(data)

for (i in 1:length(colnames(data))) {
  data[[i]] <- round(data[[i]], 4)
}
#FF99A3生成热图的颜色范围函数
col_fun = colorRamp2(c( 0,0.5*max(data),max(data)), c( "white",'#c2dde4', '#368cbf'))

topanno=HeatmapAnnotation(df=group,#列注释
                          border = T,
                          show_annotation_name = F,
                          show_legend = F,
                          col = list(group=c('ACEI'='#D87575',
                                             'ARB'='#337AB7',
                                             'CCB'='#FF9933')))

Heatmap(data,
        heatmap_legend_param = list(
          title = "SUCRA", 
          labels = c("0", "0.5", "1"), 
          labels_gp = gpar(fontsize = 8, fontfamily = "serif")),
        col = col_fun,
        cluster_columns = F,
        cluster_rows = F,
        row_names_side =  'left',#行注释置于左侧
        top_annotation = topanno,
        show_heatmap_legend = T,
        show_column_names = T,
        border = T,
        rect_gp = gpar(col = "#F5F7FA", lwd = 1),#热图单元边框颜色和宽度
        row_names_gp = gpar(fontsize = 10, fontfamily = "serif"),
        border_gp = gpar(lwd = 2),
        column_names_gp = gpar(fontsize = 10, fontfamily = "serif"),
        column_split = group,#按照分组来分隔热图
        column_gap = unit(1, "mm"),#分隔间距
        column_names_rot = 45,
        cell_fun = function(j, i, x, y, width, height, fill) {
          if(data[i, j] > 0)
            grid.text(sprintf("%.2f", data[i, j]), x, y, gp = gpar(fontsize = 8))
        }
)

# SUCRA_CCB_complexheatmap -------------------------------------------------------
library(tidyverse)
library(ComplexHeatmap)
library(circlize)

data <- readxl::read_xlsx(path = 'data/all_sucra_new.xlsx', sheet = 2) #0作为NA

data <- data %>% select(c(1,11:17)) %>% filter(outcome != "Q_score")

data <- column_to_rownames(data, var = 'outcome')

group <- c(rep('CCB',7))
group <- data.frame(group)
rownames(group) <- colnames(data)

for (i in 1:length(colnames(data))) {
  data[[i]] <- round(data[[i]], 4)
}

#FF99A3生成热图的颜色范围函数
col_fun = colorRamp2(c( 0,0.5*max(data),max(data)), c( "white",'#c2dde4', '#368cbf'))

topanno=HeatmapAnnotation(df=group,#列注释
                          border = T,
                          show_annotation_name = F,
                          show_legend = F,
                          col = list(group=c('CCB'='#FF9933'))
                          )

Heatmap(data,
        heatmap_legend_param = list(
          title = "SUCRA", 
          labels = c("0", "0.5", "1"), 
          labels_gp = gpar(fontsize = 8, fontfamily = "serif")),
        col = col_fun,
        cluster_columns = F,
        cluster_rows = F,
        row_names_side =  'left',#行注释置于左侧
        top_annotation = topanno,
        show_heatmap_legend = T,
        show_column_names = T,
        border = T,
        rect_gp = gpar(col = "#F5F7FA", lwd = 1),#热图单元边框颜色和宽度
        row_names_gp = gpar(fontsize = 10, fontfamily = "serif"),
        border_gp = gpar(lwd = 2),
        column_names_gp = gpar(fontsize = 10, fontfamily = "serif"),
        column_split = group,#按照分组来分隔热图
        column_gap = unit(1, "mm"),#分隔间距
        column_names_rot = 45,
        cell_fun = function(j, i, x, y, width, height, fill) {
          if(data[i, j] > 0)
            grid.text(sprintf("%.2f", data[i, j]), x, y, gp = gpar(fontsize = 8))
        }
)

