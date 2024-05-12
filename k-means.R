library(dplyr)
library(tidyr)
library(stats)
library(fastDummies) # for one hot vector encoding
library(ggplot2)
library(ggtree) # ggplot for tree 
library(RColorBrewer)

colors.seven <- c("#1f77b4", "#ff7f0e", "#84919e", "#d62728", "#9467bd", "#8c564b", "#e377c2")
shapes.four <- c(16, 17, 18, 15)
comb <- expand.grid(color = colors.seven, shape = shapes.four)


d <- read.csv("data.csv")
d$strain <- as.factor(d$strain)
d2 <- d %>%
  mutate(Medium = case_when(
    Medium == "Poor" ~ "low",
    Medium == "Rich" ~ "high",
    TRUE ~ Medium
  )) %>%
  mutate(strain = paste("FC", strain, sep = "")) %>%
  mutate(strain = if_else(strain == "FC6170", "IFM65656", strain)) %>%
  mutate(dataset2 = paste(strain, Medium, Temperature, sep = "_"))

d2 <  d2 %>%
  mutate(Area_day4.day0 = scale(Area_day4.day0)) %>%
  mutate(Grayness_day4.day0 = scale(Grayness_day4.day0))

d_one_hot <- d2[, c("strain","Temperature","Medium","Area_day4.day0", "Grayness_day4.day0")]
d_one_hot <- dummy_cols(d_one_hot, select_columns = "strain")
d_one_hot <- dummy_cols(d_one_hot, select_columns = "Temperature")
d_one_hot <- dummy_cols(d_one_hot, select_columns = "Medium")
d2_cluster <- d_one_hot %>%
  select(-strain) %>%
  select(-Temperature) %>%
  select(-Medium)
row.names(d2_cluster) <- d2$ID

write.csv(d2_cluster, "data_for_clustering.csv", quote=F)

# k-means
k_values <- 2:28
cluster_results <- lapply(k_values, function(k) {
  set.seed(123) 
  kmeans(d2_cluster, centers = k, nstart = 25)
})


wss <- sapply(cluster_results, function(x) x$tot.withinss)
pdf("kmeans_scaled_one-hot-vector_k-2-28.color-strain.pdf")
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters K", ylab = "Total within-clusters sum of squares")
dev.off()

for (k in 2:28) {
  # k-means
  set.seed(123)  
  kmeans_result <- kmeans(d2_cluster, centers = k, nstart = 25)
  
  d2_cluster$cluster <- factor(kmeans_result$cluster)
  
  p <- ggplot(d2_cluster, aes(x = Grayness_day4.day0, y = Area_day4.day0, color = cluster, shape = cluster)) +
    geom_point(alpha = 1, size=5) +  # 透明度を設定して点を描画
    scale_color_manual(values = rep(colors.seven, length.out = k)) +
    scale_shape_manual(values = rep(shapes.four, length.out = k)) +
    theme_minimal() +
    theme(text = element_text(size = 24),
          panel.background = element_rect(fill = "white", colour = "black", size = 1.2) 
    ) +
    labs(x = "Whiteness", y = "Area",  title = paste("k =", k), color = "Cluster", shape = "Cluster") 
  ggsave(paste("kmeans_scaled_one-hot-vector_k-",k,".color-strain.pdf",sep=""), p, width = 10, height = 8)
}  


