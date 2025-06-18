#viscosity analysis

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(tidyr)
library(reshape2)
data <- read_excel("C:/Users/anne-/OneDrive/Bureau/cellspaper/viscosity_data.xlsx", sheet = "Sheet1")
filtered_data <- data %>% filter(!is.na(Viscosity))
before_HS <- filtered_data %>% filter(HeatShock == "B")
noPI <- before_HS %>% filter(PI == "Yes")
withPI <- before_HS %>% filter(PI == "No")

#average and plot all conditions -2 to 30°C
summarized_data <- before_HS %>%
  group_by(Specie, Tissue, PI, Temperature, FishID) %>%
  summarize(
    mean_viscosity = mean(Viscosity, na.rm = TRUE),
    se_viscosity = sd(Viscosity, na.rm = TRUE) / sqrt(n())
  )
# Initialize an empty list to store the models
models <- list()

# Fit the model for each group and store the coefficients
for (i in unique(summarized_data$Specie)) {
  for (j in unique(summarized_data$Tissue)) {
    for (k in unique(summarized_data$PI)) {
      group_data <- summarized_data %>% filter(Specie == i, Tissue == j, PI == k)
      if (length(unique(group_data$Temperature)) > 2) {
        model <- lm(mean_viscosity ~ poly(Temperature, 2), data = group_data)
        models[[paste(i, j, k, sep = "_")]] <- model
      } else {
        models[[paste(i, j, k, sep = "_")]] <- NULL
      }
    }
  }
}

# Extract the coefficients and store them in a new data frame
coefficients <- data.frame(
  Specie = character(),
  Tissue = character(),
  PI = character(),
  Intercept = numeric(),
  Linear_Term = numeric(),
  Quadratic_Term = numeric()
)

for (name in names(models)) {
  if (!is.null(models[[name]])) {
    coef <- coef(models[[name]])
    parts <- strsplit(name, "_")[[1]]
    coefficients <- rbind(coefficients, data.frame(
      Specie = parts[1],
      Tissue = parts[2],
      PI = parts[3],
      Intercept = coef[1],
      Linear_Term = coef[2],
      Quadratic_Term = coef[3]
    ))
  }
}

# View the coefficients
print(coefficients)


# Plot the data with error bars
ggplot(summarized_data, aes(x = Temperature, y = mean_viscosity, color = interaction(Specie, Tissue, PI))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +  # Adds quadratic fit without error bands
  geom_errorbar(aes(ymin = mean_viscosity - se_viscosity, ymax = mean_viscosity + se_viscosity), width = 0.2) +  # Adds error bars
  scale_color_manual(values = c("#89D1A3", "#EC8383", "#9CDCE7", "#2E7646", "#e59500", "#AB3836", "#248394")) +  # Color for the interactions
  labs(x = "Temperature", y = "Mean Viscosity", title = "Mean Viscosity vs Temperature with Error Bars") +  # Labels and title
  theme_minimal() +  # Clean theme
  theme(legend.position = "bottom") +  # Position the legend at the bottom
  guides(color = guide_legend(title = "Conditions"))  # Customize legend title


# Run ANOVA analysis on the coefficients
anova_intercept <- aov(Intercept ~ Specie + Tissue + PI, data = coefficients)
anova_linear <- aov(Linear_Term ~ Specie + Tissue + PI, data = coefficients)
anova_quadratic <- aov(Quadratic_Term ~ Specie + Tissue + PI, data = coefficients)

# Extract the ANOVA results
anova_results_intercept <- summary(anova_intercept)
anova_results_linear <- summary(anova_linear)
anova_results_quadratic <- summary(anova_quadratic)

# Function to compute Eta squared
compute_eta_squared <- function(anova_results) {
  anova_df <- as.data.frame(anova_results[[1]])
  anova_df$Eta_Squared <- anova_df$`Sum Sq` / sum(anova_df$`Sum Sq`)
  return(anova_df)
}

eta_sq_intercept <- compute_eta_squared(anova_results_intercept)
eta_sq_linear <- compute_eta_squared(anova_results_linear)
eta_sq_quadratic <- compute_eta_squared(anova_results_quadratic)

effect_sizes <- data.frame(
  Factor = rownames(eta_sq_intercept),
  Intercept = eta_sq_intercept$Eta_Squared,
  Linear = eta_sq_linear$Eta_Squared,
  Quadratic = eta_sq_quadratic$Eta_Squared
)

effect_sizes_long <- effect_sizes %>%
  pivot_longer(cols = c(Intercept, Linear, Quadratic),
               names_to = "Type",
               values_to = "Eta_Squared")

ggplot(effect_sizes_long, aes(x = Factor, y = Eta_Squared, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  theme_minimal(base_size = 14, base_family = "Arial") +
  labs(title = NULL, x = "Factors", y = expression(eta^2)) +
  scale_fill_manual(values = c("#002642","#840032", "#e59500")) +
  theme(legend.position = "top", 
        panel.grid.major = element_line(color = "gray95", size = 0.5), # Lighter grid lines
        panel.grid.minor = element_blank(),                           # Remove minor g
        panel.background = element_rect(color = "black", size = 1),   # Add a frame
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"))

# Melt the dataset for easier plotting
data_melted <- melt(coefficients, id.vars = c("Specie", "Tissue", "PI"))

# Plot violin plots with visible datapoints and legend
ggplot(data_melted, aes(x = variable, y = value, fill = Specie)) +
  geom_violin(trim = FALSE) +
  geom_jitter(aes(color = Tissue, shape = PI), width = 0.2, size = 2) +
  labs(title = NULL,
       x = NULL, y = "Value / AU") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("H" = "#30C5D2", "L" = "#BD320F")) +
  scale_color_manual(values = c("Liver" = "#D1D930", "Muscle" = "#373E40")) +
  scale_shape_manual(values = c("No" = 16, "Yes" = 17))