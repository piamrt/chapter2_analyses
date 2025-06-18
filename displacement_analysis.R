library(ggplot2)
library(dplyr)

# Step 1: Identify numerical columns in the dataframe
dsp2d2s <- sapply(disp2dc2sec, is.numeric)
dsp2d3s <- sapply(disp_2dc3sec, is.numeric)
#dsp2d4s <- sapply(disp_2dc4sec, is.numeric)
dsp2d6s <- sapply(disp_2dc6sec, is.numeric)

dsp4d6s <- sapply(disp_4dc6sec, is.numeric)

#dsp10d1s <- sapply(disp_10dc1secWF, is.numeric)
#dsp10d3s <- sapply(disp_10dc3sec, is.numeric)

dsp20d1s <- sapply(disp_20dc1sec, is.numeric)
dsp20d2s <- sapply(disp_20dc2sec, is.numeric)
dsp20d3s <- sapply(disp_20dc3sec, is.numeric)
dsp20d4s <- sapply(disp_20dc4sec, is.numeric)

# Step 2: Subset the dataframe to include only numerical columns
num_data2d2s <- disp2dc2sec[, dsp2d2s]
num_data2d3s <- disp_2dc3sec[, dsp2d3s]
#num_data2d4s <- disp_2dc4sec[, dsp2d4s]
num_data2d6s <- disp_2dc6sec[, dsp2d6s]

num_data4d6s <- disp_4dc6sec[, df4d6s]

#num_data10d1s <- speed10dc1secWF[, df10d1s]
#num_data10d3s <- speed10dc3sec[, df10d3s]

num_data20d1s <- disp_20dc1sec[, df20d1s]
num_data20d2s <- disp_20dc2sec[, df20d2s]
num_data20d3s <- disp_20dc3sec[, df20d3s]
num_data20d4s <- disp_20dc4sec[, df20d4s]

# Step 3: Convert the subsetted dataframe to a single vector
all_2dc <- c(unlist(num_data2d2s, use.names = FALSE), unlist(num_data2d3s, use.names = FALSE), unlist(num_data2d6s, use.names = FALSE))
all_4dc <- unlist(num_data4d6s, use.names = FALSE)
#all_10dc <- c(unlist(num_data10d1s, use.names = FALSE), unlist(num_data10d3s, use.names = FALSE))
all_20dc <- c(unlist(num_data20d1s, use.names = FALSE), unlist(num_data20d2s, use.names = FALSE), unlist(num_data20d3s, use.names = FALSE), unlist(num_data20d4s, use.names = FALSE))

#larg_outliers <- sort(all_2dc, decreasing = TRUE)[1:10]
#filt_2dc <- all_2dc[!all_2dc %in% larg_outliers]

# Display the vector to confirm
#print(all_numerical_values)
larg_outliers <- sort(all_2dc, decreasing = TRUE)[1:2]
filt_2dc <- all_2dc[!all_2dc %in% larg_outliers]

data <- data.frame(
  values = c(filt_2dc, all_4dc, all_20dc),
  group = factor(c(rep("2°C", length(filt_2dc)),
                   rep("4°C", length(all_4dc)),
                   #rep("10°C", length(all_10dc)),
                   rep("20°C", length(all_20dc))))  # Single group for simplicity
)

# Violin plot for all data without specific groups
ggplot(data, aes(x = group, y = values)) +
  geom_violin(fill = "skyblue", color = "black", alpha = 0.7) +
  #geom_jitter(width = 0.1, size = 1, color = "darkblue", alpha = 0.6) +  # Jittered points
  labs(title = "Mitochondrial displacement", x = "Group", y = "mitochondrial displacement /um.s-1") +
  theme_minimal()

#boxplot
ggplot(data, aes(x = group, y = values)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) + 
  #geom_jitter(width = 0.1, size = 1, color = "darkblue", alpha = 0.6) +  # Add jittered points
  labs(title = "Mitochondrial Displacement", x = "Group", y = "Mitochondrial displacement (um/s)") +
  theme_minimal()