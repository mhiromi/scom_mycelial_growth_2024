library(dplyr)
library(tidyr)
library(dendextend)
library(stats)
library(tibble)
library(ggforce)
library(ggplot2)


# Shapes 0 to 24 are available; choose 5 distinct ones for illustration
shapes <- rep(c(16, 17, 18, 15),times=7)  # four combinations for each strain, x 7 strains
colors.seven <- c("#84919e", "#1f77b4", "#ff7f0e",  "#d62728", "#9467bd", "#8c564b", "#e377c2")


d <- read.csv("data.csv")
d$strain <- as.factor(d$strain)


# replacing values of column Medium (Poor/Rich) to low/high
d2 <- d %>%
  mutate(Medium = case_when(
    Medium == "Poor" ~ "low",
    Medium == "Rich" ~ "high",
    TRUE ~ Medium 
  )) %>%
  ## modifying strain names
  mutate(strain = paste("FC", strain, sep = "")) %>%
  mutate(strain = if_else(strain == "FC6170", "FC6170(IFM65656)", strain)) %>% 
  # creating a dataset2
  mutate(dataset2 = paste(strain, Medium, Temperature, sep = "_")) %>% 
  mutate(shape_label = paste(Medium, Temperature, sep="_"))


strain <- data.frame("strain" = d2$strain)
unique_strain <- unique(d2$strain)
rownames(strain) <- d2$ID


### colored by medium, no shape
colors <- c("#ff7f0e", "#1f77b4")
gp <- ggplot(d2, aes(x = Grayness_day4.day0, y = Area_day4.day0, color = Medium)) +
  geom_point(size=5) +
  scale_color_manual(values = colors) + 
  theme_minimal() +
  theme(text = element_text(size = 24),
        panel.background = element_rect(fill = "white", colour = "black", size = 1.2) 
  ) +
  labs(title = "",
       x = "Whiteness",
       y = "Area") +
  guides(color = guide_legend(title = "Medium Type"))
ggsave(file="scatterplot_medium.pdf", gp, width=297, height=210, units="mm", dpi=600)


### colored by temperature, no shape
colors <- c("#ff7f0e", "#1f77b4")
gp <- ggplot(d2, aes(x = Grayness_day4.day0, y = Area_day4.day0, color = Temperature)) +
  geom_point(size=5) +
  scale_color_manual(values = colors) + 
  theme_minimal() +
  theme(text = element_text(size = 24),
        panel.background = element_rect(fill = "white", colour = "black", size = 1.2) 
  ) +
  labs(title = "",
       x = "Whiteness",
       y = "Area") 
ggsave(file="scatterplot_temperature.pdf", width=297, height=210, units="mm", dpi=600)


## scatter plot of area and whiteness with all labels
gp <- ggplot(d2, aes(x = Grayness_day4.day0, y = Area_day4.day0, shape = dataset, color = dataset2)) +
  geom_point(size=5) +
  scale_color_manual(values = colors.seven) + 
  scale_shape_manual(values = shapes) +  
  theme_minimal() +
  theme(text = element_text(size = 24),
        panel.background = element_rect(fill = "white", colour = "black", size = 1.2) 
  ) +
  labs(title = "",
       x = "Whiteness",
       y = "Area") +
  guides(color = guide_legend(title = "strain"), shape = guide_legend(title = "culture condition"))
ggsave(file="scatterplot_color_by_strain_shape_by_condtion.pdf", width=297, height=210, units="mm", dpi=600)


##  Calculate summary statistics and plotting averaged Area and Whiteness
stats <- d2 %>%
  group_by(strain, Medium, Temperature) %>%
  summarise(
    Area_Average = mean(Area_day4.day0),
    Area_SD = sd(Area_day4.day0),
    Gray_Average = mean(Grayness_day4.day0),
    Gray_SD = sd(Grayness_day4.day0),
    .groups = 'drop'
  )  %>%
  mutate(shape_label = paste(Medium, Temperature, sep="_"))

gp <- ggplot(stats, aes(x = Gray_Average, y = Area_Average, color = strain, shape = shape_label)) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin = Area_Average - Area_SD, ymax = Area_Average + Area_SD), width = 0.02) +
  geom_errorbarh(aes(xmin = Gray_Average - Gray_SD, xmax = Gray_Average + Gray_SD), height = 1) +
  scale_color_manual(values = colors.seven) + 
  scale_shape_manual(values = shapes) +  
  theme_minimal() +
  theme(text = element_text(size = 24),
        panel.background = element_rect(fill = "white", colour = "black", size = 1.2)) +
  labs(title = "",
       x = "Whiteness",
       y = "Area",
       color = "Strain",
       shape = "Culture Condition") 
ggsave(file="scatterplot_average.pdf", plot = gp, width=297, height=210, units="mm", dpi=600)

## plot boxplot for d2
## making grey areas representing "Medium is poor"
cover_area <- data.frame(x1=c(seq(1, 28, 1) - 0.5), x2=c(seq(2, 29, 1) - 0.5), y1=rep(0, 28), y2=rep(Inf, 28), 
                         group=rep(c("Low","High"), 14))
gp <- ggplot(d2) + 
  geom_boxplot(aes(x=dataset, y=Area_day4.day0), outlier.shape = NA) +
  geom_rect(data=cover_area, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=group), color="gray", alpha=0.3) +
  scale_fill_manual(values=c("gray", "white"))+
  geom_jitter(aes(x=dataset, y=Area_day4.day0, color=Temperature), width=0.2) +
  scale_color_manual(values = c("#ff7f0e", "#1f77b4")) +
  theme_minimal() +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle=90, vjust = 0.4)) +
  labs(title = "",
       x = "dataset",
       y = "Area") +
  guides(color = guide_legend(title = "temperature"), fill = guide_legend(title = "medium"))

plot(gp)
ggsave(file="boxplot_gray-medium-poor_color-by-temp.pdf", width=297, height=210, units="mm", dpi=600)


cover_area <- data.frame(x1=c(seq(1, 28, 2) - 0.5), x2=c(seq(3, 29, 2) - 0.5), y1=rep(0, 28), y2=rep(Inf, 28), 
                         group=rep(c("high","low"), 14))
cover_area2 <- data.frame(x1=c(seq(1, 28, 4) - 0.5), x2=c(seq(5, 29, 4) - 0.5), y1=rep(0, 7), y2=rep(Inf, 7), 
                          group=c(unique(d2$strain)))
seven_colors <- c("#84919e", "#1f77b4", "#ff7f0e",  "#d62728", "#9467bd", "#8c564b", "#e377c2")


gp <- ggplot(d2) + 
  geom_boxplot(aes(x=dataset2, y=Area_day4.day0), outlier.shape = NA) +
  geom_rect(data=cover_area, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=group), color="gray", alpha=0.3) +
  scale_fill_manual(values=c("gray", "white"))+
  geom_jitter(aes(x=dataset2, y=Area_day4.day0, color=Temperature), width=0.2) +
  scale_color_manual(values = c("#ff7f0e", "#1f77b4"))+ 
  geom_rect(data=cover_area2, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black", alpha=0) +
  theme_minimal() +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle=90, vjust = 0.4)) +
  labs(title = "",
       x = "dataset",
       y = "Area") +
  guides(color = guide_legend(title = "temperature"), fill = guide_legend(title = "medium")) +
  annotate("text", x=1, y=80, label="FC6141", hjust=0, vjust=2, size=6) +
  annotate("text", x=5, y=80, label="FC6170", hjust=0, vjust=2, size=6) +
  annotate("text", x=9, y=80, label="FC8125", hjust=0, vjust=2, size=6) +
  annotate("text", x=13, y=80, label="FC8152", hjust=0, vjust=2, size=6) +
  annotate("text", x=17, y=80, label="FC8172", hjust=0, vjust=2, size=6) +
  annotate("text", x=21, y=80, label="FC8191", hjust=0, vjust=2, size=6) +
  annotate("text", x=25, y=80, label="FC8192", hjust=0, vjust=2, size=6) 
plot(gp)
ggsave(file="boxplot_gray-temp_color-by-med.pdf", width=297, height=210, units="mm", dpi=600)



### boxplot whiteness

cover_area <- data.frame(x1=c(seq(1, 28, 2) - 0.5), x2=c(seq(3, 29, 2) - 0.5), y1=rep(0, 28), y2=rep(Inf, 28), 
                         group=rep(c("high","low"), 14))
cover_area2 <- data.frame(x1=c(seq(1, 28, 4) - 0.5), x2=c(seq(5, 29, 4) - 0.5), y1=rep(0, 7), y2=rep(Inf, 7), 
                          group=c(unique(d2$strain)))
gp <- ggplot(d2) + 
  geom_boxplot(aes(x=dataset2, y=Grayness_day4.day0), outlier.shape = NA) +
  geom_rect(data=cover_area, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=group), color="gray", alpha=0.3) +
  scale_fill_manual(values=c("gray", "white"))+
  geom_jitter(aes(x=dataset2, y=Grayness_day4.day0, color=Temperature), width=0.2) +
  scale_color_manual(values = c("#ff7f0e", "#1f77b4")) +
  ## making areas for strains
  geom_rect(data=cover_area2, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black", alpha=0) +
  theme_minimal() +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle=90, vjust = 0.4)        ) +
  labs(title = "",
       x = "dataset",
       y = "Whiteness") +
  guides(color = guide_legend(title = "temperature"), fill = guide_legend(title = "medium")) +
  annotate("text", x=1, y=1.4, label="FC6141", hjust=0, vjust=2, size=6) +
  annotate("text", x=5, y=1.4, label="FC6170", hjust=0, vjust=2, size=6) +
  annotate("text", x=9, y=1.4, label="FC8125", hjust=0, vjust=2, size=6) +
  annotate("text", x=13, y=1.4, label="FC8152", hjust=0, vjust=2, size=6) +
  annotate("text", x=17, y=1.4, label="FC8172", hjust=0, vjust=2, size=6) +
  annotate("text", x=21, y=1.4, label="FC8191", hjust=0, vjust=2, size=6) +
  annotate("text", x=25, y=1.4, label="FC8192", hjust=0, vjust=2, size=6) 

plot(gp)
ggsave(file="boxplot_whiteness.pdf", width=297, height=210, units="mm", dpi=600)


### ANOVA for Area 
library(car) 

analyze_strain <- function(data, key) {
  current_strain <- key$strain
  print(paste("Analyzing strain:", current_strain))
  
  aov_result <- anova(aov(Area_day4.day0 ~ dataset, data = data))
  aov_summary <- summary(aov_result)
  aov_result2 <- unlist(aov_result)
  
  sum_df <- data.frame(
    Df_fx = aov_result2[1],
    Sum_Sq = aov_result2[3],
    Mean_Sq = aov_result2[5],
    F_value = aov_result2[7],
    Pr_gt_F = aov_result2[9],
    Df_res = aov_result2[2],
    Sum_res = aov_result2[4],
    Mean_res = aov_result2[6]
  )
  
  colnames(sum_df) <- c("Df_fx", "Sum_Sq", "Mean_Sq", "F_value", "P_value", "Df_Residuals", "Sum_Sq_Residuals", "Mean_Sq_Residuals")
  return(sum_df)
}

results <- d %>%
  group_by(strain) %>%
  group_modify(~ analyze_strain(.x, .y))

print(results)


### ANOVA for Whiteness 
library(car)  

analyze_strain <- function(data, key) {
  current_strain <- key$strain
  print(paste("Analyzing strain:", current_strain))
  
  aov_result <- anova(aov(Grayness_day4.day0 ~ dataset, data = data))
  aov_summary <- summary(aov_result)
  aov_result2 <- unlist(aov_result)
  
  sum_df <- data.frame(
    Df_fx = aov_result2[1],
    Sum_Sq = aov_result2[3],
    Mean_Sq = aov_result2[5],
    F_value = aov_result2[7],
    Pr_gt_F = aov_result2[9],
    Df_res = aov_result2[2],
    Sum_res = aov_result2[4],
    Mean_res = aov_result2[6]
  )
  
  colnames(sum_df) <- c("Df_fx", "Sum_Sq", "Mean_Sq", "F_value", "P_value", "Df_Residuals", "Sum_Sq_Residuals", "Mean_Sq_Residuals")
  return(sum_df)
}

results_whitness <- d %>%
  group_by(strain) %>%
  group_modify(~ analyze_strain(.x, .y))

print(results_whitness)


library(dplyr)
library(tidyr)
library(broom)   # tidy() 
library(agricolae) 

#### ANOVA for Area 
# ANOVA for each strain
results <- list()
d %>%
  group_by(strain) %>%
  split(.$strain) %>%
  purrr::map(function(data) {
    # ANOVA
    aov_result <- aov(Area_day4.day0 ~ dataset, data = data)
    # Tukey's HSD test
    tukey_results <- TukeyHSD(aov_result, "dataset")
    
    list(
      ANOVA = summary(aov_result),
      TukeyHSD = tukey_results
    )
  }) -> results 

print(results)

#### Whiteness
results_whiteness <- list()
d %>%
  group_by(strain) %>%
  split(.$strain) %>%
  purrr::map(function(data) {
    # ANOVA
    aov_result <- aov(Grayness_day4.day0 ~ dataset, data = data)
    # Tukey's HSD test
    tukey_results <- TukeyHSD(aov_result, "dataset")
    
    list(
      ANOVA = summary(aov_result),
      TukeyHSD = tukey_results
    )
  }) -> results_whiteness  

results_whiteness


results_df <- d %>%
  group_by(strain) %>%
  do({
    # ANOVA
    aov_result <- aov(Area_day4.day0 ~ dataset, data = .)
    # Tukey's HSDtest
    tukey_results <- TukeyHSD(aov_result, "dataset")
    tukey_df <- data.frame(tukey_results$dataset, check.names = FALSE)
    anova_df <- tidy(aov_result)
    data.frame(Dataset = rownames(tukey_df),
               Diff_tukey = tukey_df[, "diff"],
               Lower_tukey = tukey_df[, "lwr"],
               Upper_tukey = tukey_df[, "upr"],
               P_value_adj_tukey = tukey_df[, "p adj"])
  })
write.csv(results_df, file="ANOVA_TUKEY_AREA.csv", quote=F)

results_df <- d %>%
  group_by(strain) %>%
  do({
    # ANOVA
    aov_result <- aov(Grayness_day4.day0 ~ dataset, data = .)
    # Tukey's HSDS
   tukey_results <- TukeyHSD(aov_result, "dataset")
    tukey_df <- data.frame(tukey_results$dataset, check.names = FALSE)

    data.frame(Dataset = rownames(tukey_df),
               Diff_tukey = tukey_df[, "diff"],
               Lower_tukey = tukey_df[, "lwr"],
               Upper_tukey = tukey_df[, "upr"],
               P_value_adj_tukey = tukey_df[, "p adj"])
  })
write.csv(results_df, file="ANOVA_TUKEY_Whiteness.csv", quote=F)

