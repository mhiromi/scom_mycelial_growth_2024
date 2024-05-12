library(dplyr)
library(tidyr)
library(stats)
library(fastDummies) # for one hot vector encoding
library(ggplot2)
library(ggtree) # ggplot for tree
library(ggnewscale)  # for adding new scale
library(ape)

colors.seven <- c("#84919e", "#1f77b4", "#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#e377c2")
shapes.four <- c(16, 17, 18, 15)

d <- read.csv("data.csv")
d$strain <- as.factor(d$strain)

d <- d %>%
  mutate(Medium = case_when(
    Medium == "Poor" ~ "low",
    Medium == "Rich" ~ "high",
    TRUE ~ Medium
  )) %>%
  mutate(strain = paste("FC", strain, sep = "")) %>%
  mutate(strain = if_else(strain == "FC6170", "IFM65656", strain)) %>%
  mutate(dataset2 = paste(strain, Medium, Temperature, sep = "_")) %>%
  mutate(Area_day4.day0 = scale(Area_day4.day0)) %>%
  mutate(Grayness_day4.day0 = scale(Grayness_day4.day0))

# define a function for clustering
create_hclust_plot <- function(method.dist, method.clust, space) {
  
  # extracting columns for clustering
  d_one_hot <- d[, c("strain", "Medium", "Temperature", "Area_day4.day0", "Grayness_day4.day0")]
  d_one_hot <- dummy_cols(d_one_hot, select_columns = "Temperature")
  d_one_hot <- dummy_cols(d_one_hot, select_columns = "Medium")
  d_one_hot <- dummy_cols(d_one_hot, select_columns = "strain")
  
  d2_cluster <- d_one_hot %>%
    select(-Temperature) %>%
    select(-Medium) %>%
    select(-strain)
  row.names(d2_cluster) <- d$ID
  
  write.csv(d2_cluster, "data_for_clustering.csv", quote=F)
  
  
  d2_dist <- dist(d2_cluster, method = method.dist)
  d2_hclust <- hclust(d2_dist, method = method.clust)
  
  # transforming dendrogram format to ggtree format
  dend <- as.phylo(d2_hclust)
  
  unique_datasets <- unique(d$dataset2)
  colors <- rainbow(length(unique_datasets))
  names(colors) <- unique_datasets
  
  p <- ggtree(dend) %<+% d + 
    scale_color_brewer(name="dataset2") + 
    ggtitle(paste("Hierarchical clustering: ", method.dist, " / ", method.clust, sep="")) +
    theme(legend.position = "none", axis.title.y = element_blank(), 
          plot.title = element_text(size = 16, face = "bold", hjust = 0.05, vjust = -15))
  
  strain <- data.frame("strain" = d$strain)
  unique_strain <- unique(d$strain)
  rownames(strain) <- d$ID
  
  h1 <- gheatmap(p, strain, offset = 0, width = 0.02, color = NULL, colnames = FALSE) +
    scale_fill_manual(name = "strain", values = colors.seven, breaks = unique_strain, labels = unique_strain) + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.box = "vertical", legend.margin = margin())
  
  medium <- data.frame("medium" = d$Medium)
  unique_medium <- unique(d$Medium)
  rownames(medium) <- d$ID
  
  h2 <- h1 + new_scale_fill()
  
  temp <- data.frame("temperature" = d$Temperature)
  unique_temp <- unique(d$Temperature)
  rownames(temp) <- d$ID
  
  h3 <- gheatmap(h2, medium, offset = space, width = 0.02, color = NULL, colnames = FALSE)+
    scale_fill_manual(name = "Medium",
                      values = c("yellow","#00d1b1"),
                      breaks = c( "low", "high"),
                      labels = c( "low", "high"))+
    theme(legend.position = "bottom",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.box = "vertical", legend.margin = margin())+
    guides(fill = guide_legend(nrow = 2,byrow = TRUE))
  
  h4 <- h3 + new_scale_fill()
  h5 <- gheatmap(h4, temp, offset = space * 2, width = 0.02, color = NULL, colnames = FALSE) +
    scale_fill_manual(name = "Temperature",
                      values = c("skyblue", "#9E0142"),
                      breaks = c("RT", "37"),
                      labels = c("RT", "37")) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.box = "vertical", legend.margin = margin()) +
    guides(shape = guide_legend(override.aes = list(size = 2)))
  
  filename <- paste("hclust_strain_",  method.dist, "_", method.clust, ".pdf", sep="")
  ggsave(filename, h5, width=10, height=8)
}

create_hclust_plot("manhattan", "ward.D2", 0.5)


