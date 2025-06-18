# viscosity before/after HS

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(tidyr)
data <- read_excel("C:/Users/anne-/OneDrive/Bureau/cellspaper/viscosity_data.xlsx", sheet = "Sheet1")
filtered_data <- data %>% filter(!is.na(Viscosity))
Temp_2 <- filtered_data %>% filter(Temperature < 3)

#average and plot all conditions
summarised_data <- Temp_2 %>% 
  group_by(Sample, Temperature) %>% 
  summarize(HSdiff = diff(Viscosity[HeatShock %in% c("A", "B")]), .groups = 'drop')

# Merge the viscosity differences back with the original data to retain Specie, Tissue, PI
merged_data <- Temp_2 %>%
  select(Specie, Tissue, PI, Temperature, Sample) %>%
  distinct() %>%
  inner_join(summarised_data, by = c("Sample", "Temperature"))

# Calculate the average viscosity differences grouped by Specie, Tissue, PI, Temperature
all_HSdiffdata <- merged_data %>%
  group_by(Specie, Tissue, PI, Temperature, Sample) %>%
  summarize(Average_HSdiff = mean(HSdiff, na.rm = TRUE),
            se_HSdiff = sd(HSdiff, na.rm = TRUE) / sqrt(n())
            , .groups = 'drop')

mean_HSdiff <- merged_data %>%
  group_by(Specie, Tissue, PI, Temperature) %>%
  summarize(Average_HSdiff = mean(HSdiff, na.rm = TRUE),
            se_HSdiff = sd(HSdiff, na.rm = TRUE) / sqrt(n())
            , .groups = 'drop')

# Define the colors for each condition
condition_colors <- c(
  "H.Muscle.Yes" = "#AB3836",
  "H.Muscle.No" = "#EC8383",
  "H.Liver.Yes" = "#2E7646",
  "H.Liver.No" = "#89D1A3",
  "L.Muscle.Yes" = "#248394",
  "L.Muscle.No" = "#9CDCE7",
  "L.Liver.Yes" = "#e59500"
)

# Reorder interaction(Specie, Tissue, PI) within each Temperature
mean_HSdiff <- mean_HSdiff %>%
  group_by(Temperature) %>%
  mutate(Condition = factor(interaction(Specie, Tissue, PI), levels = interaction(Specie, Tissue, PI)[order(Average_HSdiff)])) %>%
  ungroup()
# Explicitly set the levels of the Temperature factor
mean_HSdiff$Temperature <- factor(mean_HSdiff$Temperature, levels = c(-2, 0, 2))

# Plot the data with dodged bars and error bars, using custom colors and a polished theme
ggplot(mean_HSdiff, aes(x = Temperature, y = Average_HSdiff, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +  # Smaller dodged bars
  geom_errorbar(aes(ymin = Average_HSdiff - se_HSdiff, ymax = Average_HSdiff + se_HSdiff), width = 0.2, position = position_dodge(width = 0.7)) +  # Error bars
  scale_fill_manual(values = condition_colors) +  # Custom colors for each condition
  labs(x = "Temperature /°C", y = "Mean Increased Viscosity after Heat Shock /mPa.s", title = NULL) +  # Labels and title
  theme_minimal(base_size = 15) +  # Clean theme with larger base font size
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_text(size = 12),  # Customize legend title size
    legend.text = element_text(size = 10),  # Customize legend text size
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Center and bold the title
    axis.title = element_text(size = 14),  # Customize axis titles size
    axis.text = element_text(size = 12)  # Customize axis text size
  ) +
  guides(fill = guide_legend(title = NULL))  # Customize legend title

anova_test <- aov(Average_HSdiff ~ Specie + Tissue + PI, data = mean_HSdiff)
anova_sum <- summary(anova_test)
print(anova_sum)

# Extract the ANOVA results into a dataframe
anova_results <- data.frame(
  Factor = rownames(anova_sum[[1]]),
  Df = anova_sum[[1]]$Df,
  Sum_Sq = anova_sum[[1]]$`Sum Sq`,
  Mean_Sq = anova_sum[[1]]$`Mean Sq`,
  F_value = anova_sum[[1]]$`F value`,
  P_value = anova_sum[[1]]$`Pr(>F)`
)

# Remove the Residuals row
anova_results <- anova_results[anova_results$Factor != "Residuals", ]

# Add -log10(P-value)
anova_results$log10_P_value <- -log10(anova_results$P_value)

# Add significance levels
anova_results$Significance <- cut(anova_results$P_value, 
                                  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                                  labels = c("***", "**", "*", ".", " "))

# Plot the ANOVA results with -log10(P-value)
ggplot(anova_results, aes(x = Factor, y = log10_P_value, fill = Factor)) +
  geom_bar(stat = "identity", width = 0.6) +  # Bar plot
  geom_text(aes(label = Significance), vjust = -0.5, size = 5) +  # Add significance levels
  scale_fill_manual(values = c("#220901", "#009DDC", "#F26430", "#000")) +  # Custom colors
  labs(x = "Factor", y = "-log10(P-value)", title = "ANOVA Results: -log10(P-value) and Significance Levels") +  # Labels and title
  theme_minimal(base_size = 15) +  # Clean theme with larger base font size
  theme(
    legend.position = "none",  # Remove legend
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Center and bold the title
    axis.title = element_text(size = 14),  # Customize axis titles size
    axis.text = element_text(size = 12)  # Customize axis text size
  )