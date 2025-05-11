#1. Kiểm tra và cài đặt các gói cần thiết
# 2. Kiểm tra thư mục làm việc hiện tại
cat("Thư mục làm việc hiện tại:", getwd(), "\n")

# 3. Tạo dữ liệu và biểu đồ cho DNMT1
dnmt1_data <- data.frame(
  Category = c("Grade 2", "Grade 2/3", "Grade 3", "Grade III", "Grade 0", "Grade I", "Grade II", "Clinical Stage N", "Clinical Stage T"),
  Value = c(0.1, 0.5, 1.1, 0.5, 0.1, 0.5, 1.2, 0.5, 1.2)
)
dnmt1_plot <- ggplot(dnmt1_data, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 3)) +
  scale_fill_manual(values = c("lightblue", "lightcoral", "brown", "lightblue", "lightgray", "gray", "brown", "lightblue", "gray")) +
  labs(title = "DNMT1", x = "", y = "Normalized High-H3K%") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6),
    plot.title = element_text(size = 8),
    legend.position = "none"
  )
#Tương tự cho  các. Nhóm còn lại
# 11. Ghép 8 biểu đồ thành 2 hàng (4 trên, 4 dưới)
top_row <- plot_grid(dnmt1_plot, g9a_plot, h3k18ac_plot, h3k4me2_plot, ncol = 4, nrow = 1)
bottom_row <- plot_grid(h3k79me_plot, h3k9me2_plot, nsd1_plot, smyd2_plot, ncol = 4, nrow = 1)
combined_plot <- plot_grid(top_row, bottom_row, ncol = 1, nrow = 2)

# 12. Lưu biểu đồ ghép thành file PNG
ggsave("combined_figure_4B_8labels.png", plot = combined_plot, width = 12, height = 6)

# 13. Thông báo hoàn thành
cat("Biểu đồ ghép đã được lưu tại:", file.path(getwd(), "combined_figure_4B_8labels.png"), "\n")



# File: figure_6A.R
# Mục đích: Tạo biểu đồ cột cho khả năng sống tế bào (CCK-8 assay)
# 1. Cài đặt và tải gói
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
# 2. Kiểm tra thư mục làm việc
cat("Thư mục làm việc hiện tại:", getwd(), "\n")
# 3. Tạo dữ liệu
data <- data.frame(
  Cell_line = rep(c("Luminal A", "Luminal A", "Claudin-low TNBC", "Claudin-low TNBC", "Basal-like TNBC", "Basal-like TNBC"), each = 6),
  Time = rep(c("24h", "48h", "24h", "48h", "24h", "48h"), each = 6),
  UNC0642_uM = rep(c(0, 1, 2, 4, 8, 16), 6),
  Cell_viability = c(
    100, 95, 85, 70, 55, 45,  # Luminal A 24h
    100, 90, 80, 65, 40, 30,  # Luminal A 48h
    100, 98, 95, 90, 80, 75,  # Claudin-low TNBC 24h
    100, 96, 90, 85, 75, 60,  # Claudin-low TNBC 48h
    100, 90, 80, 70, 60, 45,  # Basal-like TNBC 24h
    100, 85, 70, 60, 45, 30   # Basal-like TNBC 48h
  )
)
# 4. Tạo biểu đồ cột
png("figure_6A.png", width = 1400, height = 600, res = 100)
ggplot(data, aes(x = factor(UNC0642_uM), y = Cell_viability, fill = Time)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Cell_line, ncol = 3) +
  theme_minimal() +
  labs(
    title = "Hình 6A: Khả năng sống tế bào theo nồng độ UNC0642",
    x = "Nồng độ UNC0642 (µM)",
    y = "Độ khả dụng tế bào (% so với đối chứng)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

# 5. Thông báo hoàn thành
cat("Biểu đồ đã được lưu tại:", file.path(getwd(), "figure_6A.png"), "\n")



# File: figure_6B.R
# Mục đích: Tạo biểu đồ đường cho tăng sinh tế bào (Control vs. UNC0642)
# 1. Cài đặt và tải gói
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
library(ggplot2)
library(tidyr)
# 2. Kiểm tra thư mục làm việc
cat("Thư mục làm việc hiện tại:", getwd(), "\n")
# 3. Tạo dữ liệu (sửa số lượng phần tử để khớp)
data <- data.frame(
  Type = rep(c("Luminal A", "Claudin-low TNBC", "Basal-like TNBC"), each = 5),  # Mỗi loại có 5 ngày
  Day = rep(c(0, 3, 4, 5, 6), 3),  # 5 ngày cho mỗi loại, lặp lại 3 lần
  Control = c(
    1.0, 3.2, 4.0, 5.0, 6.8,  # Luminal A
    1.0, 1.6, 2.0, 2.4, 3.3,  # Claudin-low TNBC
    1.0, 1.8, 2.2, 2.7, 2.3   # Basal-like TNBC
  ),
  UNC0642 = c(
    1.0, 2.2, 2.5, 3.0, 4.5,  # Luminal A
    1.0, 1.3, 1.2, 1.5, 1.7,  # Claudin-low TNBC
    1.0, 1.2, 1.3, 1.5, 1.2   # Basal-like TNBC
  )
)
# 3. Tạo dữ liệu (sửa số lượng phần tử để khớp)
data <- data.frame(
  Type = rep(c("Luminal A", "Claudin-low TNBC", "Basal-like TNBC"), each = 5),  # Mỗi loại có 5 ngày
  Day = rep(c(0, 3, 4, 5, 6), 3),  # 5 ngày cho mỗi loại, lặp lại 3 lần
  Control = c(
    1.0, 3.2, 4.0, 5.0, 6.8,  # Luminal A
    1.0, 1.6, 2.0, 2.4, 3.3,  # Claudin-low TNBC
    1.0, 1.8, 2.2, 2.7, 2.3   # Basal-like TNBC
  ),
  UNC0642 = c(
    1.0, 2.2, 2.5, 3.0, 4.5,  # Luminal A
    1.0, 1.3, 1.2, 1.5, 1.7,  # Claudin-low TNBC
    1.0, 1.2, 1.3, 1.5, 1.2   # Basal-like TNBC
  )
)
4. Chuyển dữ liệu sang định dạng dài
data_long <- pivot_longer(
  data,
  cols = c(Control, UNC0642),
  names_to = "Condition",
  values_to = "Value"
)
# 5. Tạo biểu đồ đường
png("figure_6B.png", width = 1400, height = 600, res = 100)
ggplot(data_long, aes(x = Day, y = Value, color = Condition, linetype = Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Hình 6B: Tăng sinh tế bào (Control vs. UNC0642)",
    x = "Ngày",
    y = "Mức tăng sinh"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
dev.off()


# Kiểm tra và cài đặt các gói cần thiết nếu chưa có
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  # Cài đặt tidyverse nếu chưa có
  install.packages("tidyverse")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  # Cài đặt readr nếu chưa có
  install.packages("readr")
}
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  # Cài đặt ggrepel nếu chưa có
  install.packages("ggrepel")
}
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  # Cài đặt BiocManager nếu chưa có
  install.packages("BiocManager")
}
if (!requireNamespace("EnhancedVolcano", quietly = TRUE)) {
  # Cài đặt EnhancedVolcano qua BiocManager
  BiocManager::install("EnhancedVolcano")
}
# Tải gói tidyverse để xử lý và trực quan hóa dữ liệu
library(tidyverse)
# Tải gói readr để đọc tệp CSV một cách hiệu quả
library(readr)
# Tải gói ggrepel để thêm nhãn cho các điểm trên biểu đồ mà không bị chồng lấn
library(ggrepel)
# Tải gói EnhancedVolcano để tạo biểu đồ núi lửa chuyên nghiệp
library(EnhancedVolcano)
# Thiết lập thư mục làm việc (đảm bảo lưu tệp PNG đúng nơi)
setwd("/cloud/project")
# In thư mục làm việc để kiểm tra
message("Thư mục làm việc hiện tại: ", getwd())

# Đọc dữ liệu từ tệp extract_file.csv, chứa biểu hiện gen của các dòng tế bào
data <- read_csv("extract_file.csv")
# Tính trung bình và fold change cho từng dòng tế bào
data <- data %>%
  # Tính trung bình biểu hiện gen cho điều kiện đối chứng (control) của MDA-MB-231
  mutate(
    `231_con_avg` = (`231con1` + `231con2` + `231con3`) / 3,
    # Tính trung bình biểu hiện gen cho điều kiện điều trị G9A của MDA-MB-231
    `231_G9A_avg` = (`231G9A1` + `231G9A2` + `231G9A3`) / 3,
    # Tính fold change (tỷ lệ G9A/control) cho MDA-MB-231, thêm 1e-6 để tránh chia cho 0
    `231_fold_change` = `231_G9A_avg` / (`231_con_avg` + 1e-6),
    # Tính log2 fold change cho MDA-MB-231
    `231_log2FC` = log2(`231_fold_change` + 1e-6),
    # Tính trung bình biểu hiện gen cho điều kiện đối chứng của MDA-MB-468
    `468_con_avg` = (`468con1` + `468con2` + `468con3`) / 3,
    # Tính trung bình biểu hiện gen cho điều kiện điều trị G9A của MDA-MB-468
    `468_G9A_avg` = (`468G9A1` + `468G9A2` + `468G9A3`) / 3,
    # Tính fold change cho MDA-MB-468
    `468_fold_change` = `468_G9A_avg` / (`468_con_avg` + 1e-6),
    # Tính log2 fold change cho MDA-MB-468
    `468_log2FC` = log2(`468_fold_change` + 1e-6),
    # Tính trung bình biểu hiện gen cho điều kiện đối chứng của MCF-7
    MCF7_con_avg = (MCF7con1 + MCF7con2 + MCF7con3) / 3,
    # Tính trung bình biểu hiện gen cho điều kiện điều trị G9A của MCF-7
    MCF7_G9A_avg = (MCF7G9A1 + MCF7G9A2 + MCF7G9A3) / 3,
    # Tính fold change cho MCF-7
    MCF7_fold_change = MCF7_G9A_avg / (MCF7_con_avg + 1e-6),
    # Tính log2 fold change cho MCF-7
    MCF7_log2FC = log2(MCF7_fold_change + 1e-6)
  )
# Định nghĩa hàm tính p-value bằng t-test, xử lý trường hợp dữ liệu hằng số
calculate_pvalue <- function(control, treated) {
  # Kiểm tra nếu bất kỳ nhóm nào có variance bằng 0 hoặc dữ liệu không hợp lệ
  if (var(control) == 0 || var(treated) == 0 || any(is.na(control)) || any(is.na(treated))) {
    # Trả về p-value = 1 nếu dữ liệu không đủ để chạy t-test
    return(1)
  } else {
    # Thực hiện t-test và trả về p-value
    t.test(control, treated)$p.value
  }
}
# Tính p-value và điều chỉnh p-value (FDR) cho từng dòng tế bào
data <- data %>%
  # Áp dụng hàm rowwise để xử lý từng hàng
  rowwise() %>%
  mutate(
    # Tính p-value cho MDA-MB-231 bằng cách so sánh các giá trị control và G9A
    `231_pvalue` = calculate_pvalue(
      c(`231con1`, `231con2`, `231con3`),
      c(`231G9A1`, `231G9A2`, `231G9A3`)
    ),
    # Tính p-value cho MDA-MB-468
    `468_pvalue` = calculate_pvalue(
      c(`468con1`, `468con2`, `468con3`),
      c(`468G9A1`, `468G9A2`, `468G9A3`)
    ),
    # Tính p-value cho MCF-7
    MCF7_pvalue = calculate_pvalue(
      c(MCF7con1, MCF7con2, MCF7con3),
      c(MCF7G9A1, MCF7G9A2, MCF7G9A3)
    ),
    # Điều chỉnh p-value bằng phương pháp FDR cho MDA-MB-231
    `231_adj_pvalue` = p.adjust(`231_pvalue`, method = "fdr"),
    # Điều chỉnh p-value bằng phương pháp FDR cho MDA-MB-468
    `468_adj_pvalue` = p.adjust(`468_pvalue`, method = "fdr"),
    # Điều chỉnh p-value bằng phương pháp FDR cho MCF-7
    MCF7_adj_pvalue = p.adjust(MCF7_pvalue, method = "fdr")
  ) %>%
  # Hủy chế độ rowwise để quay lại xử lý dữ liệu thông thường
  ungroup()
# Điều chỉnh p-value bằng phương pháp FDR cho MDA-MB-468
`468_adj_pvalue` = p.adjust(`468_pvalue`, method = "fdr"),
# Điều chỉnh p-value bằng phương pháp FDR cho MCF-7
MCF7_adj_pvalue = p.adjust(MCF7_pvalue, method = "fdr")
) %>%
  # Hủy chế độ rowwise để quay lại xử lý dữ liệu thông thường
  ungroup()
#Tiếp tục làm các mẫu tương tự
# Hiển thị và lưu biểu đồ cho MDA-MB-231
print(p1)
ggsave("volcano_231.png", plot = p1, width = 8, height = 6)
message("Đã lưu biểu đồ: volcano_231.png")

# Hiển thị và lưu biểu đồ cho MDA-MB-468
print(p2)
ggsave("volcano_468.png", plot = p2, width = 8, height = 6)
message("Đã lưu biểu đồ: volcano_468.png")

# Hiển thị và lưu biểu đồ cho MCF-7
print(p3)
ggsave("volcano_MCF7.png", plot = p3, width = 8, height = 6)
message("Đã lưu biểu đồ: volcano_MCF7.png")

---------------------------------------------------------------------------
  
  
# Tải gói tidyverse để xử lý và trực quan hóa dữ liệu
library(tidyverse)

# Tải gói readr để đọc tệp CSV một cách hiệu quả
library(readr)

# Tải gói clusterProfiler để phân tích con đường KEGG và GO
library(clusterProfiler)

# Tải gói org.Hs.eg.db để ánh xạ gene ID sang ENTREZID
library(org.Hs.eg.db)

# Thiết lập thư mục làm việc (đảm bảo lưu tệp PNG đúng nơi)
setwd("/cloud/project")
# In thư mục làm việc để kiểm tra
message("Thư mục làm việc hiện tại: ", getwd())
# Đọc dữ liệu từ tệp extract_file.csv, chứa biểu hiện gen của các dòng tế bào
data <- read_csv("extract_file.csv")
# Tính trung bình biểu hiện gen cho điều kiện điều trị G9A của MDA-MB-468
`468_G9A_avg` = (`468G9A1` + `468G9A2` + `468G9A3`) / 3,
# Tính fold change cho MDA-MB-468
`468_fold_change` = `468_G9A_avg` / (`468_con_avg` + 1e-6),
# Tính log2 fold change cho MDA-MB-468
`468_log2FC` = log2(`468_fold_change` + 1e-6),
# Tính trung bình biểu hiện gen cho điều kiện đối chứng của MCF-7
MCF7_con_avg = (MCF7con1 + MCF7con2 + MCF7con3) / 3,
# Tính trung bình biểu hiện gen cho điều kiện điều trị G9A của MCF-7
MCF7_G9A_avg = (MCF7G9A1 + MCF7G9A2 + MCF7G9A3) / 3,
# Tính fold change cho MCF-7
MCF7_fold_change = MCF7_G9A_avg / (MCF7_con_avg + 1e-6),
# Tính log2 fold change cho MCF-7
MCF7_log2FC = log2(MCF7_fold_change + 1e-6)
)
# Định nghĩa hàm tính p-value bằng t-test, xử lý trường hợp dữ liệu hằng số
calculate_pvalue <- function(control, treated) {
  # Kiểm tra nếu bất kỳ nhóm nào có variance bằng 0 hoặc dữ liệu không hợp lệ
  if (var(control) == 0 || var(treated) == 0 || any(is.na(control)) || any(is.na(treated))) {
    # Trả về p-value = 1 nếu dữ liệu không đủ để chạy t-test
    return(1)
  } else {
    # Thực hiện t-test và trả về p-value
    t.test(control, treated)$p.value
  }
}
# Tính p-value và điều chỉnh p-value (FDR) cho từng dòng tế bào
data <- data %>%
  # Áp dụng hàm rowwise để xử lý từng hàng
  rowwise() %>%
  mutate(
    # Tính p-value cho MDA-MB-231 bằng cách so sánh các giá trị control và G9A
    `231_pvalue` = calculate_pvalue(
      c(`231con1`, `231con2`, `231con3`),
      c(`231G9A1`, `231G9A2`, `231G9A3`)
    ),
    # Tính p-value cho MDA-MB-468
    `468_pvalue` = calculate_pvalue(
      c(`468con1`, `468con2`, `468con3`),
      c(`468G9A1`, `468G9A2`, `468G9A3`)
    ),
    # Tính p-value cho MCF-7
    MCF7_pvalue = calculate_pvalue(
      c(MCF7con1, MCF7con2, MCF7con3),
      c(MCF7G9A1, MCF7G9A2, MCF7G9A3)
    ),
    # Điều chỉnh p-value bằng phương pháp FDR cho MDA-MB-231
    `231_adj_pvalue` = p.adjust(`231_pvalue`, method = "fdr"),
    # Điều chỉnh p-value bằng phương pháp FDR cho MDA-MB-468
    `468_adj_pvalue` = p.adjust(`468_pvalue`, method = "fdr"),
    # Điều chỉnh p-value bằng phương pháp FDR cho MCF-7
    MCF7_adj_pvalue = p.adjust(MCF7_pvalue, method = "fdr")
  ) %>%
  # Hủy chế độ rowwise để quay lại xử lý dữ liệu thông thường
  ungroup()
# Lọc các gen biểu hiện khác biệt (DEGs) cho MDA-MB-231
deg_231 <- data %>%
  # Chọn các gen có |log2FC| > 0.5 và p-value điều chỉnh < 0.1 (nới lỏng ngưỡng)
  filter(abs(`231_log2FC`) > 0.5 & `231_adj_pvalue` < 0.1) %>%
  # Lấy cột Geneid
  pull(Geneid)
# In số lượng DEGs để kiểm tra
message("Số lượng DEGs cho MDA-MB-231: ", length(deg_231))
# Lọc DEGs cho MDA-MB-468
deg_468 <- data %>%
  filter(abs(`468_log2FC`) > 0.5 & `468_adj_pvalue` < 0.1) %>%
  pull(Geneid)
message("Số lượng DEGs cho MDA-MB-468: ", length(deg_468))
# Lọc DEGs cho MCF-7
deg_MCF7 <- data %>%
  filter(abs(MCF7_log2FC) > 0.5 & MCF7_adj_pvalue < 0.1) %>%
  pull(Geneid)
message("Số lượng DEGs cho MCF-7: ", length(deg_MCF7))
# Chuyển Geneid sang ENTREZID cho phân tích con đường
# Chuyển đổi cho MDA-MB-231, xử lý trường hợp không có DEGs
if (length(deg_231) > 0) {
  gene_231 <- bitr(deg_231, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)$ENTREZID
} else {
  gene_231 <- character(0)  # Gán danh sách rỗng nếu không có DEGs
}

# Chuyển đổi cho MDA-MB-468
if (length(deg_468) > 0) {
  gene_468 <- bitr(deg_468, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)$ENTREZID
} else {
  gene_468 <- character(0)
}
# Chuyển đổi cho MCF-7
if (length(deg_MCF7) > 0) {
  gene_MCF7 <- bitr(deg_MCF7, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)$ENTREZID
} else {
  gene_MCF7 <- character(0)
}

# Phân tích KEGG cho MDA-MB-231, kiểm tra nếu có gene
if (length(gene_231) > 0) {
  kegg_231 <- enrichKEGG(gene_231, organism = "hsa", pvalueCutoff = 0.05)
} else {
  kegg_231 <- NULL  # Gán NULL nếu không có gene
}
# Phân tích KEGG cho MDA-MB-468
if (length(gene_468) > 0) {
  kegg_468 <- enrichKEGG(gene_468, organism = "hsa", pvalueCutoff = 0.05)
} else {
  kegg_468 <- NULL
}

# Phân tích KEGG cho MCF-7
if (length(gene_MCF7) > 0) {
  kegg_MCF7 <- enrichKEGG(gene_MCF7, organism = "hsa", pvalueCutoff = 0.05)
} else {
  kegg_MCF7 <- NULL
}
# Tạo biểu đồ thanh cho MDA-MB-231
if (!is.null(kegg_231)) {
  # Vẽ biểu đồ thanh hiển thị top 10 con đường KEGG
  p1 <- barplot(kegg_231, showCategory = 10, title = "KEGG Pathways in MDA-MB-231") +
    # Sử dụng theme tối giản
    theme_minimal() +
    # Đặt nhãn trục
    labs(x = "-log10 FDR", y = "Pathway")
  # Hiển thị biểu đồ
  print(p1)
  # Lưu biểu đồ dưới dạng PNG
  ggsave("kegg_231.png", plot = p1, width = 8, height = 6)
  message("Đã lưu biểu đồ: kegg_231.png")
} else {
  # Thông báo nếu không có con đường KEGG đáng kể
  message("No significant KEGG pathways for MDA-MB-231")
}
# Tạo biểu đồ thanh cho MDA-MB-468
if (!is.null(kegg_468)) {
  p2 <- barplot(kegg_468, showCategory = 10, title = "KEGG Pathways in MDA-MB-468") +
    theme_minimal() +
    labs(x = "-log10 FDR", y = "Pathway")
  print(p2)
  ggsave("kegg_468.png", plot = p2, width = 8, height = 6)
  message("Đã lưu biểu đồ: kegg_468.png")
} else {
  message("No significant KEGG pathways for MDA-MB-468")
}
# Tạo biểu đồ thanh cho MCF-7
if (!is.null(kegg_MCF7)) {
  p3 <- barplot(kegg_MCF7, showCategory = 10, title = "KEGG Pathways in MCF-7") +
    theme_minimal() +
    labs(x = "-log10 FDR", y = "Pathway")
  print(p3)
  ggsave("kegg_MCF7.png", plot = p3, width = 8, height = 6)
  message("Đã lưu biểu đồ: kegg_MCF7.png")
} else {
  message("No significant KEGG pathways for MCF-7")
}
