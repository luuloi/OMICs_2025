#### link books : https://jokergoo.github.io/ComplexHeatmap-reference/book/index.html
### 3d plot: https://github.com/jokergoo/ComplexHeatmap
#install.packages("ComplexHeatmap")
library(ComplexHeatmap) 
library(circlize)
library("ellipse")
## loading data: Measles cases in US states 1930-2001\nVaccine introduced 1961
data  = readRDS(system.file("extdata", "measles.rds", package = "ComplexHeatmap"))
head(data)
## check dimenson 
dim(data)
#[1] 59 72
head(data) 
## check type of data 
class(data)
#[1] "matrix" "array"
## draw simple heatmap 
Heatmap(data)

###############################
### edit heatmap 
# create annotation for column 
mat = readRDS(system.file("extdata", "measles.rds", package = "ComplexHeatmap"))
# create annotation for column 
ha1 = HeatmapAnnotation(
    dist1 = anno_barplot(
    colSums(mat), 
    bar_width = 1, 
    gp = gpar(col = "white", fill = "#FFE200"), 
    border = FALSE,
    axis_param = list(at = c(0, 2e5, 4e5, 6e5, 8e5), labels = c("0", "200k", "400k", "600k", "800k")),
    height = unit(2, "cm")), show_annotation_name = FALSE)
# create annotation for row 
ha2 = rowAnnotation(
     dist2 = anno_barplot(
    rowSums(mat), 
    bar_width = 1, 
    gp = gpar(col = "white", fill = "#FFE200"), 
    border = FALSE,
    axis_param = list(at = c(0, 5e5, 1e6, 1.5e6), labels = c("0", "500k", "1m", "1.5m")), width = unit(2, "cm")), 
    show_annotation_name = FALSE)

# deal with column variables 
year_text = as.numeric(colnames(mat))
year_text[year_text %% 10 != 0] = ""
ha_column = HeatmapAnnotation(
  year = anno_text(year_text, rot = 0, location = unit(1, "npc"), just = "top"))

# create annotation color 
col_fun = colorRamp2(c(0, 800, 1000, 127000), c("white", "cornflowerblue", "yellow", "red"))

# draw plot 
ht_list = Heatmap(mat, name = "cases", col = col_fun,
                  cluster_columns = FALSE, show_row_dend = FALSE, rect_gp = gpar(col= "white"), 
                  show_column_names = FALSE,
                  row_names_side = "left", row_names_gp = gpar(fontsize = 8),
                  column_title = 'Measles cases in US states 1930-2001\nVaccine introduced 1961',
                  top_annotation = ha1, bottom_annotation = ha_column,
                  heatmap_legend_param = list(at = c(0, 5e4, 1e5, 1.5e5), 
                                              labels = c("0", "50k", "100k", "150k"))) + ha2
draw(ht_list, ht_gap = unit(3, "mm"))

# addd the line 
decorate_heatmap_body("cases", {
  i = which(colnames(mat) == "1961")
  x = i/ncol(mat)
  grid.lines(c(x, x), c(0, 1), gp = gpar(lwd = 2, lty = 2))
  grid.text("Vaccine introduced", x, unit(1, "npc") + unit(5, "mm"))
})


##### make a 3d heatmap 
Heatmap3D(mat, name = "cases", col = col_fun,
          cluster_columns = FALSE, show_row_dend = FALSE, 
          show_column_names = FALSE,
          row_names_side = "left", row_names_gp = gpar(fontsize = 8),
          column_title = 'Measles cases in US states 1930-2001\nVaccine introduced 1961',
          bottom_annotation = ha_column,
          heatmap_legend_param = list(at = c(0, 5e4, 1e5, 1.5e5), 
                                      labels = c("0", "50k", "100k", "150k")),
          # new arguments for Heatmap3D()
          bar_rel_width = 1, bar_rel_height = 1, bar_max_length = unit(2, "cm")
)











