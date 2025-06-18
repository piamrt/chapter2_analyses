# Load libraries
library(ggplot2)
library(dplyr)
LDassay_raw <- read_excel("C:/Users/anne-/OneDrive/Bureau/cellspaper/LDassay_raw.xlsx")
MTassay_raw <- read_excel("C:/Users/anne-/OneDrive/Bureau/cellspaper/LDassay_raw.xlsx", 
                          +     sheet = "mtt_assay")
# Sample dataset (replace this with your actual dataset)
df <- LDassay_raw
#df <- MTassay_raw # for viability data

# Calculate live/dead ratio
df <- df %>%
  mutate(ratio = live / dead)  # Live/Dead ratio for each sample
df <- df %>%
  mutate(treatment = as.factor(osmolarity))  # Convert treatment to a factor

# Summarize data to get mean and standard error
summary_df <- df %>%
  group_by(week, osmolarity) %>%
  summarise(
    #mean_ratio = mean(mts_read),
    #se_ratio = sd(mts_read) / sqrt(n()),  # Standard Error
    mean_ratio = mean(ratio),
    se_ratio = sd(ratio) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

# Plot with ggplot2
ggplot(summary_df, aes(x = factor(week), y = mean_ratio, fill = factor(osmolarity))) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black", width = 0.7) +  # Adjust bar width for clarity
  geom_errorbar(aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.6) +  # Improve error bars
  labs(
    x = "Week of Incubation",
    y = "Live/Dead Ratio",
    #y = "absorbance at 570 nm / AU", #for viability
    title = NULL,
    fill = "Culture Medium \nOsmolarity\n / mosmols"  # Updated legend label
  ) +
  theme_minimal(base_size = 10) +  # Adjust text size for better readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered and bold title
    legend.position = "right",  # Move legend to the right for better spacing
    legend.key = element_rect(fill = "white", color = NA)  # Clean up legend appearance
  ) +
  scale_fill_manual(
    values = c("#f2efbb", "#DCE494", "#c5d86d", "#9ebc63", "#618943", "#4D6D36") #ldratio
    #values = c("#ede0d4","#e6ccb2","#ddb892", "#b08968", "#9c6644", "#7f5539" )#mtt
  )  # Custom colors


#####

# Load libraries
library(ggplot2)
library(dplyr)

# Sample dataset (replace this with your actual dataset)
df <- LDassay_raw

# Convert treatment to a factor
df <- df %>%
  mutate(treatment = as.factor(osmolarity))

# Summarize data to get mean and standard deviation
summary_df <- df %>%
  group_by(week, treatment) %>%
  summarise(
    mean_live = mean(live),
    sd_live = sd(live),
    .groups = "drop"
  )

# Plot mean live values with error bars for standard deviation
ggplot(summary_df, aes(x = factor(week), y = mean_live, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_live - sd_live, ymax = mean_live + sd_live),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.6) +
  labs(
    x = "Week of Incubation",
    y = "Live cells / unit",
    title = NULL,
    fill = "Culture Medium \nOsmolarity\n / mosmols"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.key = element_rect(fill = "white", color = NA)
  ) +
  scale_fill_manual(
    values = c("#F1B2AD", "#EA8C84", "#E3655B", "#BE5C59", "#985357", "#734A55")
  )