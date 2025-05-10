
# Bioconductor packages
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("DESeq2", "DEGreport", "clusterProfiler", "org.Hs.eg.db", "enrichplot"))
BiocManager::install("pathview")
a

# CRAN packages
install.packages(c("pheatmap", "ashr", "ggrepel"))
install.packages("readxl")     
install.packages("VennDiagram")
install.packages("ggVennDiagram")
install.packages("org.Mm.eg.db")
install.packages("openxlsx")


# Load required packages
library(DESeq2)
library(ggplot2)
library(RColorBrewer)
library(pheatmap)
library(ggrepel)
library(DEGreport)
library(clusterProfiler)
library(org.Hs.eg.db)
library(enrichplot)
library(tidyverse)
library(dplyr)
library(readxl)
library(VennDiagram)
library(ggVennDiagram)
library(grid)
library(org.Mm.eg.db)
library(openxlsx)
library(pathview)

#1. Load in data #####
data <- read.table("D:/NCS 2024/Mon hoc/OMICS/Buoi 5/File run/bai bao/Bai 2/GSE202165_raw_counts_GRCh38.p13_NCBI.txt", header=T, row.names=1)

meta <- read.table("D:/NCS 2024/Mon hoc/OMICS/Buoi 5/File run/bai bao/Bai 2/Bai2_Meta_data.txt", header=T, row.names=1)
# Convert to factor
meta[, c("sampletype", "HPexpr")] <- lapply(meta[, c("sampletype", "HPexpr")], as.factor)

class(data)
head(data)

class(meta)
head(meta)


mean_counts <- apply(data[, c("Hpwt_6h_1", "Hpwt_6h_2", "Hpwt_6h_3", "Hpwt_6h_4")], 1, mean)
variance_counts <- apply(data[, c("Hpwt_6h_1", "Hpwt_6h_2", "Hpwt_6h_3", "Hpwt_6h_4")], 1, var)
df <- data.frame(mean_counts, variance_counts)
head(df)

ggplot(df) +
  geom_point(aes(x=mean_counts, y=variance_counts)) +
  geom_line(aes(x=mean_counts, y=mean_counts, color="red"), show.legend = FALSE) +
  scale_y_log10() +
  scale_x_log10() +
  theme_classic()
  

#2: DGE count normalization ####
# Create DESeqDataSet object
dds <- DESeqDataSetFromMatrix(
  countData = data,
  colData = meta,
  design = ~sampletype
)
  
class(dds)  
# Get the original count matrix
head(counts(dds))


dds <- estimateSizeFactors(dds)



# Get the normalization factor applied to each sample:
sizeFactors(dds)


normalized_counts <- counts(dds, normalized = TRUE)


write.table(normalized_counts, file = "normalized_counts.txt", sep = "\t", quote = FALSE, col.names = NA)


#3: DGE QC analysis ####
# Transform coutns for data visualization
rld <- rlog(dds, blind = TRUE)


#3.1 Plot PCA ####
plotPCA(rld, intgroup = "sampletype")


pcaData <- plotPCA(rld, intgroup = c("sampletype", "HPexpr"), returnData = TRUE)
pcaData


# Get the percentage variation (~PC)
percentVar <- round(100 * attr(pcaData, "percentVar"))


# Plot PCA using ggplot2
ggplot(
  pcaData,
  aes(
    x = PC1, y = PC2,
    color = sampletype, shape = HPexpr
  )
) +
  geom_point(size = 5) +
  labs(
    x = paste0("PC1 (", percentVar[1], "%)"),
    y = paste0("PC2 (", percentVar[2], "%)")
  ) +
  theme_classic()


# Gộp sampletype và Hpexpr để ánh xạ màu và hình
pcaData$combined <- pcaData$sampletype  # Dùng sampletype cho cả color & shape

# Vẽ biểu đồ
ggplot(pcaData, aes(x = PC1, y = PC2, color = combined, shape = combined)) +
  geom_point(size = 5) +
  labs(
    x = paste0("PC1 (", percentVar[1], "%)"),
    y = paste0("PC2 (", percentVar[2], "%)"),
    color = "Sample type", shape = "Sample type"
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#e6f2ff", color = NA),  # Nền xanh nhạt
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_blank()
  )


# Input is a matrix of log transformed values
rld_mat <- assay(rld)
pca <- prcomp(t(rld_mat))

# Create data frame with metadata and PC3 and PC4 values for input to ggplot
pca_w_meta_df <- merge(as.data.frame(pca$x), meta, by = 0 , all.y = TRUE)
pca_w_meta_df %>%
  rename("sample" = "Row.names") %>%
  ggplot(
    aes(
      x = PC3, y = PC4,
      color = sampletype
    )
  ) +
  geom_point(size = 4) +
  theme_classic()

#3.2 Heat map ######
# Extract rlog matrix from the object
rld_mat <- assay(rld)


# Compute pairwise correlation values
rld_cor <- cor(rld_mat)
head(rld_cor)


#PLot heatmap 
pheatmap(rld_cor)

#Vẽ kiểu khác
# Ma trận tương quan giữa các mẫu
rld_cor <- cor(assay(rld))


ann_colors <- list(
  sampletype = c(
    "Ctrl" = "gray",
    "Hpwt_2h" = "orange",
    "Hpwt_6h" = "skyblue",
    "Hpmut_2h" = "red",
    "Hpmut_6h" = "deepskyblue"
  )
)

# Vẽ heatmap
pheatmap(rld_cor,
         annotation_col = meta,
         annotation_row = meta,
         annotation_colors = ann_colors,
         show_rownames = FALSE,
         show_colnames = FALSE,
         annotation_names_col = FALSE,
         annotation_names_row = FALSE,
         main = "Sample Correlation Heatmap")

# Example on changing the color scale using RColorBrewer package:
heat.colors <- brewer.pal(6, "Blues")
pheatmap(rld_cor, color = heat.colors, border_color=NA, fontsize = 10,
         fontsize_row = 10, height=20)


# Select top 20 most expressed genes
select <- order(rowMeans(normalized_counts), decreasing = TRUE)[1:20]

# log2(n+1)
ntd <- normTransform(dds)
pheatmap(
  assay(ntd)[select, ],
  cluster_cols = TRUE, cluster_rows = TRUE,
  fontsize_row = 10,
  annotation_col = meta
)

# Using rlog transformation
pheatmap(
  rld_mat[select, ],
  cluster_cols = TRUE, cluster_rows = TRUE,
  fontsize_row = 10,
  annotation_col = meta
)


# Top 20 genes with most variance
top20_variance_genes <- order(rowVars(normalized_counts), decreasing = TRUE)[1:20]
pheatmap(
  rld_mat[top20_variance_genes, ],
  cluster_cols = TRUE, cluster_rows = TRUE,
  fontsize_row = 10,
  annotation_col = meta
)


#4: Differential expression analysis with DESeq2 ######
# Create DESeqDataSet object
dds <- DESeqDataSetFromMatrix(
  countData = data,
  colData = meta,
  design = ~ sampletype
)


# Run the DE analysis
dds <- DESeq(dds)


# Check the size factors
sizeFactors(dds)


## Total number of raw counts per sample
colSums(counts(dds))


# Total number of normalized counts per sample
colSums(counts(dds, normalized = TRUE))


# Plot dispersion estimates
plotDispEsts(dds)

#4.1 Hpwt_2h #####
# Define contrats, extract results table and shrink the log2 fold changes
contrast_wt_2h <- c("sampletype", "Hpwt_2h", "Ctrl")
res_table_wt_2h_unshrunken <- results(dds, contrast = contrast_wt_2h, alpha = 0.05)
res_table_wt_2h <- lfcShrink(dds, contrast = contrast_wt_2h, res = res_table_wt_2h_unshrunken, type = "ashr")


# Unshrunken MA plot
plotMA(res_table_wt_2h_unshrunken)


# Shrunken MA(Mean-Average) plot
plotMA(res_table_wt_2h)

class(res_table_wt_2h)

mcols(res_table_wt_2h, use.names = TRUE)

res_table_wt_2h

# Summarize results
summary(res_table_wt_2h)


# Set thresholds
padj.cutoff <- 0.05
lfc.cutoff <- log2(1.5)

#ctrl vs wt_2h
res_table_wt_2h_df <- res_table_wt_2h %>%
  as.data.frame() %>%
  rownames_to_column("gene")

sig_wt_2h <- res_table_wt_2h_df %>%
  filter(padj < padj.cutoff & abs(log2FoldChange) > lfc.cutoff)

sig_wt_2h
write.xlsx(sig_wt_2h, file = "sig_wt_2h_raW.xlsx", rowNames = FALSE)


#4.2 Hpmut_2h #####
# Define contrats, extract results table and shrink the log2 fold changes
contrast_mut_2h <- c("sampletype", "Hpmut_2h", "Ctrl")
res_table_mut_2h_unshrunken <- results(dds, contrast = contrast_mut_2h, alpha = 0.05)
res_table_mut_2h <- lfcShrink(dds, contrast = contrast_mut_2h, res = res_table_mut_2h_unshrunken, type = "ashr")


# Unshrunken MA plot
plotMA(res_table_wt_2h_unshrunken)


# Shrunken MA(Mean-Average) plot
plotMA(res_table_mut_2h)

class(res_table_mut_2h)

mcols(res_table_mut_2h, use.names = TRUE)

res_table_mut_2h


# Summarize results
summary(res_table_mut_2h)


# Set thresholds
padj.cutoff <- 0.05
lfc.cutoff <- log2(1.5)

#ctrl vs mut_2h
res_table_mut_2h_df <- res_table_mut_2h %>%
  as.data.frame() %>%
  rownames_to_column("gene")


sig_mut_2h <- res_table_mut_2h_df %>%
  filter(padj < padj.cutoff & abs(log2FoldChange) > lfc.cutoff)


sig_mut_2h
write.xlsx(sig_mut_2h, file = "sig_mut_2h_raW.xlsx", rowNames = FALSE)



#4.3 Hpwt_6h #####
# Define contrats, extract results table and shrink the log2 fold changes
contrast_wt_6h <- c("sampletype", "Hpwt_6h", "Ctrl")
res_table_wt_6h_unshrunken <- results(dds, contrast = contrast_wt_6h, alpha = 0.05)
res_table_wt_6h <- lfcShrink(dds, contrast = contrast_wt_6h, res = res_table_wt_6h_unshrunken, type = "ashr")


# Unshrunken MA plot
plotMA(res_table_wt_6h_unshrunken)


# Shrunken MA(Mean-Average) plot
plotMA(res_table_wt_6h)


class(res_table_wt_6h)


mcols(res_table_wt_6h, use.names = TRUE)


res_table_wt_6h


# Xuất file Excel từ kết quả
library(writexl)
write_xlsx(
  cbind(
    gen = rownames(res_table_wt_6h)[which(!is.na(res_table_wt_6h$padj) & res_table_wt_6h$padj < 0.05)],
    as.data.frame(res_table_wt_6h)[which(!is.na(res_table_wt_6h$padj) & res_table_wt_6h$padj < 0.05), ]
  ),"res_table_wt_6h_sig_genes_raw.xlsx")

# Summarize results
summary(res_table_wt_6h)


# Set thresholds
padj.cutoff <- 0.05
lfc.cutoff <- log2(1.5)

#ctrl vs wt_6h
res_table_wt_6h_df <- res_table_wt_6h %>%
  as.data.frame() %>%
  rownames_to_column("gene")


sig_wt_6h <- res_table_wt_6h_df %>%
  filter(padj < padj.cutoff & abs(log2FoldChange) > lfc.cutoff)


sig_wt_6h
write.xlsx(sig_wt_6h, file = "sig_wt_6h_raW.xlsx", rowNames = FALSE)


#4.4 Hpmut_6h #####
# Define contrats, extract results table and shrink the log2 fold changes
contrast_mut_6h <- c("sampletype", "Hpmut_6h", "Ctrl")
res_table_mut_6h_unshrunken <- results(dds, contrast = contrast_mut_6h, alpha = 0.05)
res_table_mut_6h <- lfcShrink(dds, contrast = contrast_mut_6h, res = res_table_mut_6h_unshrunken, type = "ashr")


# Unshrunken MA plot
plotMA(res_table_wt_6h_unshrunken)


# Shrunken MA(Mean-Average) plot
plotMA(res_table_mut_6h)


class(res_table_mut_6h)


mcols(res_table_mut_6h, use.names = TRUE)


res_table_mut_6h


# Summarize results
summary(res_table_mut_6h)


# Set thresholds
padj.cutoff <- 0.05
lfc.cutoff <- log2(1.5)

#ctrl vs wt_6h
res_table_mut_6h_df <- res_table_mut_6h %>%
  as.data.frame() %>%
  rownames_to_column("gene")


sig_mut_6h <- res_table_mut_6h_df %>%
  filter(padj < padj.cutoff & abs(log2FoldChange) > lfc.cutoff)


sig_mut_6h
write.xlsx(sig_mut_6h, file = "sig_mut_6h_raW.xlsx", rowNames = FALSE)


#5: Visualizing the results #####
# Create df including row names
Meta_data_HP <- meta %>%
  rownames_to_column("samplename")

normalized_counts <- normalized_counts %>%
  as.data.frame() %>%
  rownames_to_column("gene")


# Order results by padj values
top20_sig_wt_6h_genes <- res_table_wt_6h_df %>%
  arrange(padj) %>%
  pull(gene) %>%
  head(n = 20)
top20_sig_wt_6h_genes


# Subet normalized counts for top20 sigOE genes
top20_sig_wt_6h_norm <- normalized_counts %>%
  filter(gene %in% top20_sig_wt_6h_genes)


# Covert df to long format
gathered_top20_sig_wt_6h <- top20_sig_wt_6h_norm %>%
  gather(colnames(top20_sig_wt_6h_norm)[2:21], key = "samplename", value = "normalized_counts")
head(gathered_top20_sig_wt_6h)


# Merge with metadata
gathered_top20_sig_wt_6h_w_meta <- inner_join(Meta_data_HP, gathered_top20_sig_wt_6h, by = "samplename")
head(gathered_top20_sig_wt_6h_w_meta)


# Merge with metadata
gathered_top20_sig_wt_6h_w_meta <- inner_join(Meta_data_HP, gathered_top20_sig_wt_6h, by = "samplename")
head(gathered_top20_sig_wt_6h_w_meta)


# Plot
ggplot(
  gathered_top20_sig_wt_6h_w_meta,
  aes(x = gene, y = normalized_counts, color = sampletype)
) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_y_log10() +
  scale_color_manual(
    labels = c("ctrl", "Hpwt_2h", "Hpwt_6h","Hpwt_2h","Hpwt_6h"),
    values = c("gray", "yellow", "red", "blue","green")
  ) +
  labs(
    x = NULL, y = "log10 normalized counts", title = "Top 20 Significant DE Genes"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


# Extract normalized expression for significant genes from the OE and control samples, and set the gene column (1) to row names
norm_sig_wt_6h <- normalized_counts[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)] %>%
  filter(gene %in% sig_wt_6h$gene) %>%
  column_to_rownames("gene")
head(norm_sig_wt_6h)


# Annotate heatmap
annotation <- Meta_data_HP %>%
  dplyr::select(samplename, sampletype) %>%
  column_to_rownames("samplename")

# Set a color palette
heat_colors <- brewer.pal(8, "YlOrRd")

# 5.1 Heatmap#####
pheatmap(
  norm_sig_wt_6h,
  color = heat_colors,
  cluster_rows = TRUE, cluster_cols = TRUE,
  show_rownames = FALSE,
  annotation_col = annotation,
  fontsize = 10,
  border_color = NA,
  scale = "row",
  fontsize_row = 10,
  height = 20
)

#gom gen sig của 4 nhóm 
all_sig_genes <- union(sig_wt_6h$gene,
                       union(sig_mut_6h$gene,
                             union(sig_wt_2h$gene, sig_mut_2h$gene)))

norm_sig_all <- normalized_counts[, 1:21] %>%
  filter(gene %in% all_sig_genes) %>%
  column_to_rownames("gene")

# Annotate heatmap
annotation <- Meta_data_HP %>%
  dplyr::select(samplename, sampletype) %>%
  column_to_rownames("samplename")

# Set a color palette
heat_colors <- brewer.pal(8, "YlOrRd")
# Heatmap
pheatmap(
  norm_sig_all,
  color = heat_colors,
  cluster_rows = TRUE, cluster_cols = TRUE,
  show_rownames = FALSE,
  annotation_col = annotation,
  fontsize = 10,
  border_color = NA,
  scale = "row",
  fontsize_row = 10,
  height = 20
)

#5.2 Volcano plot với gen nổi bật #####
#5.2.1 Hpwt_2h#####
# Specify up/down/ns expressed genes
res_table_wt_2h_df <- res_table_wt_2h_df %>%
  mutate(
    expression = case_when(
      padj < 0.05 & log2FoldChange > 1 ~ "up",
      padj < 0.05 & log2FoldChange < -1 ~ "down",
      TRUE ~ "ns"
    )
  )

res_table_wt_2h_df

# Lọc những gene nổi bật
highlight_genes <- res_table_wt_2h_df %>%
  filter(padj < 10^-5, abs(log2FoldChange) > 1)
highlight_genes

# Vẽ volcano plot
res_table_wt_2h_df %>%
  ggplot(
    aes(x = log2FoldChange, y = -log10(padj), color = expression)
  ) +
  geom_point(size = 3, alpha = 0.5) +  # alpha đưa ra ngoài aes()
  geom_text_repel(
    data = highlight_genes,
    aes(label = gene),
    size = 3,
    max.overlaps = 15
  ) +
  geom_hline(
    yintercept = -log10(10^-5),
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = c(-1, 1),
    linetype = "dashed"
  ) +
  scale_color_manual(
    breaks = c("up", "down", "ns"),
    values = c("tomato", "steelblue", "grey")
  ) +
  guides(alpha = "none") +
  coord_cartesian(xlim = c(-2.5, 4), ylim = c(0, 350)) +
  labs(
    title = "Hpwt_2h",
    x = "log2(FC)", y = "-log10(padj)", color = "Expression\nchange"
  ) +
  theme_classic()
ggsave("Hpwt_2h_volcano.png", width = 4.5, height = 4, dpi = 300)

#5.2.2 Hpmut_2h#####
# Specify up/down/ns expressed genes
res_table_mut_2h_df <- res_table_mut_2h_df %>%
  mutate(
    expression = case_when(
      padj < 0.05 & log2FoldChange > 1 ~ "up",
      padj < 0.05 & log2FoldChange < -1 ~ "down",
      TRUE ~ "ns"
    )
  )

res_table_mut_2h_df

# Lọc những gene nổi bật
highlight_genes <- res_table_mut_2h_df %>%
  filter(padj < 10^-5, abs(log2FoldChange) > 1)
highlight_genes

# Vẽ volcano plot
res_table_mut_2h_df %>%
  ggplot(
    aes(x = log2FoldChange, y = -log10(padj), color = expression)
  ) +
  geom_point(size = 3, alpha = 0.5) +  # alpha đưa ra ngoài aes()
  geom_text_repel(
    data = highlight_genes,
    aes(label = gene),
    size = 3,
    max.overlaps = 15
  ) +
  geom_hline(
    yintercept = -log10(10^-5),
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = c(-1, 1),
    linetype = "dashed"
  ) +
  scale_color_manual(
    breaks = c("up", "down", "ns"),
    values = c("tomato", "steelblue", "grey")
  ) +
  guides(alpha = "none") +
  coord_cartesian(xlim = c(-2.5, 4), ylim = c(0, 350)) +
  labs(
    title = "Hpmut_2h",
    x = "log2(FC)", y = "-log10(padj)", color = "Expression\nchange"
  ) +
  theme_classic()
ggsave("Hpmut_2h_volcano.png", width = 4.5, height = 4, dpi = 300)


#5.2.3 Hpwt_6h#####
# Specify up/down/ns expressed genes
res_table_wt_6h_df <- res_table_wt_6h_df %>%
  mutate(
    expression = case_when(
      padj < 0.05 & log2FoldChange > 1.5 ~ "up",
      padj < 0.05 & log2FoldChange < -1.5 ~ "down",
      TRUE ~ "ns"
    )
  )

res_table_wt_6h_df

# Lọc những gene nổi bật
highlight_genes <- res_table_wt_6h_df %>%
  filter(padj < 10^-5, abs(log2FoldChange) > 1.5)
highlight_genes

# Vẽ volcano plot
res_table_wt_6h_df %>%
  ggplot(
    aes(x = log2FoldChange, y = -log10(padj), color = expression)
  ) +
  geom_point(size = 3, alpha = 0.5) +  # alpha đưa ra ngoài aes()
  geom_text_repel(
    data = highlight_genes,
    aes(label = gene),
    size = 3,
    max.overlaps = 10
  ) +
  geom_hline(
    yintercept = -log10(10^-5),
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = c(-1.5, 1.5),
    linetype = "dashed"
  ) +
  scale_color_manual(
    breaks = c("up", "down", "ns"),
    values = c("tomato", "steelblue", "grey")
  ) +
  guides(alpha = "none") +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 320)) +
  labs(
    title = "Hpwt_6h",
    x = "log2(FC)", y = "-log10(padj)", 
  ) +
  theme_classic()
ggsave("Hpwt_6h_volcano.png", width = 6, height = 4, dpi = 300)


#5.2.4 Hpmut_6h#####
# Specify up/down/ns expressed genes
res_table_mut_6h_df <- res_table_mut_6h_df %>%
  mutate(
    expression = case_when(
      padj < 0.05 & log2FoldChange > 1.5 ~ "up",
      padj < 0.05 & log2FoldChange < -1.5 ~ "down",
      TRUE ~ "ns"
    )
  )

res_table_mut_6h_df

# Lọc những gene nổi bật
highlight_genes <- res_table_mut_6h_df %>%
  filter(padj < 10^-5, abs(log2FoldChange) > 1.5)
highlight_genes

# Vẽ volcano plot
res_table_mut_6h_df %>%
  ggplot(
    aes(x = log2FoldChange, y = -log10(padj), color = expression)
  ) +
  geom_point(size = 3, alpha = 0.5) +  # alpha đưa ra ngoài aes()
  geom_text_repel(
    data = highlight_genes,
    aes(label = gene),
    size = 3,
    max.overlaps = 10
  ) +
  geom_hline(
    yintercept = -log10(10^-5),
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = c(-1.5, 1.5),
    linetype = "dashed"
  ) +
  scale_color_manual(
    breaks = c("up", "down", "ns"),
    values = c("tomato", "steelblue", "grey")
  ) +
  guides(alpha = "none") +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 320)) +
  labs(
    title = "Hpmut_6h",
    x = "log2(FC)", y = "-log10(padj)", 
  ) +
  theme_classic()
ggsave("Hpmut_6h_volcano.png", width = 6, height = 4, dpi = 300)


#5.3 Ven plot #####
#nhập dữ liệu
genes_hpwt_2h <- read_excel("D:/NCS 2024/Mon hoc/OMICS/Buoi 5/File run/bai bao/Bai 2/DE_genes_bai bao 2.xlsx", sheet = "Hpwt_2h")$gene
genes_hpmut_2h <- read_excel("D:/NCS 2024/Mon hoc/OMICS/Buoi 5/File run/bai bao/Bai 2/DE_genes_bai bao 2.xlsx", sheet = "Hpmut_2h")$gene
genes_hpwt_6h <- read_excel("D:/NCS 2024/Mon hoc/OMICS/Buoi 5/File run/bai bao/Bai 2/DE_genes_bai bao 2.xlsx", sheet = "Hpwt_6h")$gene
genes_hpmut_6h <- read_excel("D:/NCS 2024/Mon hoc/OMICS/Buoi 5/File run/bai bao/Bai 2/DE_genes_bai bao 2.xlsx", sheet = "Hpmut_6h")$gene

#vẽ biểu đồ
venn.plot <- venn.diagram(
  x = list(
    Hpwt_2h = genes_hpwt_2h,
    Hpmut_2h = genes_hpmut_2h,
    Hpwt_6h = genes_hpwt_6h,
    Hpmut_6h = genes_hpmut_6h
  ),
  filename = NULL,
  fill = c("tomato", "skyblue", "orange", "mediumseagreen"),
  alpha = 0.5,
  cex = 1.3,
  cat.cex = 1.2,
  cat.pos = 0,
  cat.dist = 0.25,
  margin = 0.1
)

grid.draw(venn.plot)

#vẽ cách khác
# Gom dữ liệu vào 1 list
gene_list <- list(
  "Hpwt_2h" = genes_hpwt_2h,
  "Hpmut_2h" = genes_hpmut_2h,
  "Hpwt_6h" = genes_hpwt_6h,
  "Hpmut_6h" = genes_hpmut_6h
)

# Vẽ biểu đồ Venn
ggVennDiagram(gene_list, label_alpha = 0) +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "Venn Diagram of DE Genes Across 4 Infected Conditions") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )


# Giao của tất cả 4 nhóm
genes_all_shared <- Reduce(intersect, list(genes_hpwt_2h, genes_hpmut_2h, genes_hpwt_6h, genes_hpmut_6h))
genes_all_shared
# Giao 3 nhóm: ví dụ chỉ giữa các nhóm 6h (Hpwt_6h và Hpmut_6h) và Hpwt_2h
genes_shared_3 <- Reduce(intersect, list(genes_hpwt_2h, genes_hpwt_6h, genes_hpmut_6h))
genes_shared_3
# Gene riêng của từng nhóm (không có trong 3 nhóm còn lại)
genes_only_hpwt_2h <- setdiff(genes_hpwt_2h, union(union(genes_hpmut_2h, genes_hpwt_6h), genes_hpmut_6h))
genes_only_hpmut_2h <- setdiff(genes_hpmut_2h, union(union(genes_hpwt_2h, genes_hpwt_6h), genes_hpmut_6h))
genes_only_hpwt_6h <- setdiff(genes_hpwt_6h, union(union(genes_hpwt_2h, genes_hpmut_2h), genes_hpmut_6h))
genes_only_hpmut_6h <- setdiff(genes_hpmut_6h, union(union(genes_hpwt_2h, genes_hpmut_2h), genes_hpwt_6h))

# Giao giữa đúng 2 nhóm (ví dụ: Hpwt_6h và Hpmut_6h) nhưng không có ở 2 nhóm còn lại
genes_shared_6h_only <- intersect(genes_hpwt_6h, genes_hpmut_6h)
genes_shared_6h_only <- setdiff(genes_shared_6h_only, union(genes_hpwt_2h, genes_hpmut_2h))


#6: DGE analysis using LRT in DESeq2 #####
dds <- DESeqDataSetFromMatrix(
  countData = round(data),
  colData = meta,
  design = ~ sampletype
)


# Likelihood ratio test
dds_lrt <- DESeq(dds, test = "LRT", reduced = ~ 1)


# Extract results
res_LRT <- results(dds_lrt)


res_LRT


# Subset the LRT results to filter genes with significant threshold
sig_res_LRT <- res_LRT %>%
  as.data.frame() %>%
  rownames_to_column("gene") %>%
  filter(padj < 0.01)

# Get sig gene lists
sigLRT_genes <- sig_res_LRT$gene

print("Significantly DGE from LRT test:")
length(sigLRT_genes)

# Compare to sig genes from previous Wald test
print("Significantly DGE between Hpwt_6h and Control:")
nrow(sig_wt_6h)
print("Significantly DGE between Hpmut_6h and Control:")
nrow(sig_mut_6h)


# Filter significant genes with padj < 1e-25
sigLRT_genes <- res_LRT %>%
  as.data.frame() %>%
  rownames_to_column("gene") %>%
  filter(padj < 1e-25) %>%
  pull(gene)


# Remaining sig genes
length(sigLRT_genes)


# Obtain rlog values for those significant genes
rlog_sigGenes_mat <- rld_mat[sigLRT_genes, ]


clusters <- degPatterns(
  rlog_sigGenes_mat,
  metadata = meta,
  time = "sampletype",
  col = NULL
)

# What type of data structure is the `clusters` output?
class(clusters)
# Data is stored in the "df" component
head(clusters$df)

#Trích các gene nhóm 1 để phân tích sâu hơn
group1_cluster <- clusters$df %>%
  filter(cluster == 1)
head(group1_cluster)

#7: Basic functional analysis#####
#7.1 GO #####
#7.1a. Hpwt_6h DGE result netween Hpwt_6h and Control using Wald test####
res_table_wt_6h
head(res_table_wt_6h)

# Create background dataset for hypergeometric testing using all genes tested for significance in the results
all_wt_6h_genes <- as.character(res_table_wt_6h_df$gene)
# Extract significant genes
sig_wt_6h_genes <- res_table_wt_6h_df %>%
  dplyr::filter(padj < 0.05, abs(log2FoldChange) > 0.58) %>%
  pull(gene) %>%
  as.character()
sig_wt_6h_genes


mapped <- bitr(sig_wt_6h_genes, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = org.Hs.eg.db)
head(mapped)
nrow(mapped)

gene_list <- sig_wt_6h$gene
gene_list

# Run gene ontology (GO) for over-representation analysis (ORA)
ORA_res <- enrichGO(gene = sig_wt_6h$gene,
                    universe = all_wt_6h_genes,
                    keyType = "ENTREZID",
                    OrgDb = org.Hs.eg.db,
                    ont = "BP",
                    pAdjustMethod = "BH",
                    qvalueCutoff = 0.05)
ORA_res

# Output results from GO ORA to a table
ORA_res_df <- data.frame(ORA_res)
head(ORA_res_df)

length(ORA_res)
# Save ORA result
write.csv(ORA_res_df, "ORA_wt_6h_Bai2_raw3.csv")


dotplot(ORA_res, showCategory = 10)


barplot(ORA_res, showCategory = 10)


#7.1b Hpmut_6h DGE result netween Hpmut_6h and Control using Wald test####
res_table_mut_6h
head(res_table_mut_6h)

# Create background dataset for hypergeometric testing using all genes tested for significance in the results
all_mut_6h_genes <- as.character(res_table_mut_6h_df$gene)
# Extract significant genes
sig_mut_6h_genes <- res_table_mut_6h_df %>%
  dplyr::filter(padj < 0.05, abs(log2FoldChange) > 0.58) %>%
  pull(gene) %>%
  as.character()
sig_mut_6h_genes


mapped <- bitr(sig_mut_6h_genes, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = org.Hs.eg.db)
head(mapped)
nrow(mapped)

gene_list <- sig_mut_6h$gene
gene_list

# Run gene ontology (GO) for over-representation analysis (ORA)
ORA_res <- enrichGO(gene = sig_mut_6h$gene,
                    universe = all_mut_6h_genes,
                    keyType = "ENTREZID",
                    OrgDb = org.Hs.eg.db,
                    ont = "BP",
                    pAdjustMethod = "BH",
                    qvalueCutoff = 0.05)
ORA_res

# Output results from GO ORA to a table
ORA_res_df <- data.frame(ORA_res)
head(ORA_res_df)

length(ORA_res)
# Save ORA result
write.csv(ORA_res_df, "ORA_mut_Bai2_raw3.csv")


dotplot(ORA_res, showCategory = 10)


barplot(ORA_res, showCategory = 10)


#7.2 KEGG #####
kegg_res <- enrichKEGG(
  gene = sig_mut_6h$gene,
  organism = "hsa",           # Homo sapiens
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.2
)
# Xem top pathway
head(kegg_res)

# Vẽ biểu đồ dotplot với 10 pathway được làm giàu nhiều nhất
dotplot(kegg_res, showCategory = 10)


#TNF mut_2h
kegg_res <- enrichKEGG(
  gene = sig_mut_2h$gene,      # danh sách ENTREZID của các DEGs
  organism = "hsa",             # "hsa" cho người
  pvalueCutoff = 0.05
)

# log2FoldChange vector tên gene là ENTREZID
gene_fc <- sig_mut_2h$log2FoldChange
names(gene_fc) <- sig_mut_2h$gene


# hsa04668 = TNF signaling pathway
pathview(
  gene.data  = gene_fc,
  pathway.id = "hsa04668",
  species    = "hsa",
  out.suffix = "TNF_pathway_2h"
)