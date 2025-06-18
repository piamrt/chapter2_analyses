

library(ggplot2)
library(ggsignif)
library(dplyr)

#import all the files
#concatenate all values into temperatures
#plot 100 fastest or something
#speeds
speed2dc2sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/2sec/20241111_2dc2sec_speed.txt", header=FALSE)
speed2dc3sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/3sec/20241111_2dc3sec_speed.txt", header=FALSE)
speed2dc4sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/4sec/20241111_2dc4sec_speed.txt", header=FALSE)
speed2dc6sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/6sec/20241107_2dc6sec_speed.txt", header=FALSE)

speed4dc6sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/4dc6sec/20241107_4dc6sec_speed.txt", header=FALSE)

speed10dc1sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/10dc/1sec/20241111_10dc1secWF_speed.txt", header=FALSE)
speed10dc3sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/10dc/3sec/20241111_10dc3sec_speed.txt", header=FALSE)

speed20dc1sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/1sec/20241111_WF20dc1sec_speed.txt", header=FALSE)
speed20dc2sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/2sec/20241107_20dc2sec_speed.txt", header=FALSE)
speed20dc3sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/3sec/20241111_20dc3sec_speed.txt", header=FALSE)
speed20dc4sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/4sec/20241111_20dc4sec_speed.txt", header=FALSE)

# Step 1: Identify numerical columns in the dataframe
df2d2s <- sapply(speed2dc2sec, is.numeric)
df2d3s <- sapply(speed2dc3sec, is.numeric)
df2d4s <- sapply(speed2dc4sec, is.numeric)
df2d6s <- sapply(speed2dc6sec, is.numeric)

df4d6s <- sapply(speed4dc6sec, is.numeric)

df10d1s <- sapply(speed10dc1sec, is.numeric)
df10d3s <- sapply(speed10dc3sec, is.numeric)

df20d1s <- sapply(speed20dc1sec, is.numeric)
df20d2s <- sapply(speed20dc2sec, is.numeric)
df20d3s <- sapply(speed20dc3sec, is.numeric)
df20d4s <- sapply(speed20dc4sec, is.numeric)

# Step 2: Subset the dataframe to include only numerical columns
num_data2d2s <- speed2dc2sec[, df2d2s]
num_data2d3s <- speed2dc3sec[, df2d3s]
num_data2d4s <- speed2dc4sec[, df2d4s]
num_data2d6s <- speed2dc6sec[, df2d6s]

num_data4d6s <- speed4dc6sec[, df4d6s]

num_data10d1s <- speed10dc1sec[, df10d1s]
num_data10d3s <- speed10dc3sec[, df10d3s]

num_data20d1s <- speed20dc1sec[, df20d1s]
num_data20d2s <- speed20dc2sec[, df20d2s]
num_data20d3s <- speed20dc3sec[, df20d3s]
num_data20d4s <- speed20dc4sec[, df20d4s]

# Step 3: Convert the subsetted dataframe to a single vector
all_2dc <- c(unlist(num_data2d2s, use.names = FALSE), unlist(num_data2d3s, use.names = FALSE), unlist(num_data2d4s, use.names = FALSE), unlist(num_data2d6s, use.names = FALSE))
all_4dc <- unlist(num_data4d6s, use.names = FALSE)
all_10dc <- c(unlist(num_data10d1s, use.names = FALSE), unlist(num_data10d3s, use.names = FALSE))
all_20dc <- c(unlist(num_data20d1s, use.names = FALSE), unlist(num_data20d2s, use.names = FALSE), unlist(num_data20d3s, use.names = FALSE), unlist(num_data20d4s, use.names = FALSE))

larg_outliers <- sort(all_2dc, decreasing = TRUE)[1:5]
filt_2dc <- all_2dc[!all_2dc %in% larg_outliers]

clean2dc <- na.omit(filt_2dc)
clean10dc <- na.omit(all_10dc)
clean4dc <- na.omit(all_4dc)
clean20dc <- na.omit(all_20dc)


#vectors <- list(clean2dc = clean2dc, clean4dc = clean4dc, clean10dc = clean10dc, clean20dc = clean20dc)
vectors <- list(clean2dc = clean2dc, clean4dc = clean4dc, clean20dc = clean20dc)

# Create the data frame using the names of the vectors
allspeeds <- do.call(rbind, lapply(names(vectors), function(name) {
  data.frame(
    VectorID = name,
    Value = vectors[[name]]
  )
}))

# Define the custom order for VectorID
#custom_order <- c("clean2dc", "clean4dc", "clean10dc", "clean20dc")
custom_order <- c("clean2dc", "clean4dc", "clean20dc")

# Ensure VectorID is treated as a factor with the specified order
allspeeds$VectorID <- factor(allspeeds$VectorID, levels = custom_order)

# Define custom colors
#custom_colors <- c("#FF5733", "#3357FF", "#FF33A1", "#A133FF")
custom_colors <- c("#44679C", "#99A7C2", "#BA2926")

ggplot(allspeeds, aes(x = VectorID, y = Value, fill = VectorID)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  #geom_jitter(color = "grey90", size = 0.3, alpha = 0.3) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.2, color = "black", fill = "#000") +
  scale_fill_manual(values = custom_colors)  + 
  geom_signif(
    comparisons = list(c("clean2dc", "clean4dc"), c("clean2dc", "clean20dc"), c("clean4dc", "clean20dc")),
    map_signif_level = TRUE
  ) +
  theme_minimal() +
  labs(title = "Speed - Undirected movement", x = NULL, y = expression(paste("mitochondrial speed /", mu, "m.s"^-1)))


#top values
#threshold <- 0.2

# Filter the data
#fastest_speed <- allspeeds[allspeeds$Value > threshold, ]
# Ensure VectorID is treated as a factor with the specified order
#fastest_speed$VectorID <- factor(fastest_speed$VectorID, levels = custom_order)

# Define the threshold percentage
top_percentage <- 0.25

# Filter the top 10% highest values for each VectorID
fastest_speed <- allspeeds %>%
  group_by(VectorID) %>%
  arrange(desc(Value)) %>%
  #arrange(Value) %>%
  slice(1:ceiling(n() * top_percentage)) %>%
  ungroup()

# Define custom colors
#custom_colors <- c("#D25409",  "#B25400", "#AF33A1", "#A13399")
custom_colors <- c("#44679C", "#99A7C2", "#BA2926")

ggplot(fastest_speed, aes(x = VectorID, y = Value, fill = VectorID)) +
  geom_violin() +
  #geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  #geom_jitter(color = "grey90", size = 0.3, alpha = 0.3) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.2, color = "black", fill = "#000") +
  scale_fill_manual(values = custom_colors) +
  geom_signif(
    comparisons = list(c("clean2dc", "clean4dc"), c("clean2dc", "clean20dc"), c("clean4dc", "clean20dc")),
    map_signif_level = TRUE
  ) +
  theme_minimal() +
  labs(title = expression(paste("Fastest fraction - Speed > 0.5", mu, "m.s"^-1)), x = NULL, y = expression(paste("mitochondrial speed /", mu, "m.s"^-1)))

##### ------ #####
# velocities
veloc2dc2sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/2sec/20241111_2dc2sec_velocity.txt", header=FALSE)
veloc2dc3sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/3sec/20241111_2dc3sec_velocity.txt", header=FALSE)
veloc2dc4sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/4sec/20241111_2dc4sec_velocity.txt", header=FALSE)
veloc2dc6sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/6sec/20241107_2dc6sec_velocity.txt", header=FALSE)

veloc4dc6sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/4dc6sec/20241107_4dc6sec_velocity.txt", header=FALSE)

veloc10dc1sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/10dc/1sec/20241111_10dc1secWF_velocity.txt", header=FALSE)
veloc10dc3sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/10dc/3sec/20241111_10dc3sec_velocity.txt", header=FALSE)

veloc20dc1sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/1sec/20241111_WF20dc1sec_velocity.txt", header=FALSE)
veloc20dc2sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/2sec/20241107_20dc2sec_velocity.txt", header=FALSE)
veloc20dc3sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/3sec/20241111_20dc3sec_velocity.txt", header=FALSE)
veloc20dc4sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/4sec/20241111_20dc4sec_velocity.txt", header=FALSE)
#velocity
veldf2d2s <- sapply(veloc2dc2sec, is.numeric)
veldf2d3s <- sapply(veloc2dc3sec, is.numeric)
veldf2d4s <- sapply(veloc2dc4sec, is.numeric)
veldf2d6s <- sapply(veloc2dc6sec, is.numeric)

veldf4d6s <- sapply(veloc4dc6sec, is.numeric)

veldf10d1s <- sapply(veloc10dc1sec, is.numeric)
veldf10d3s <- sapply(veloc10dc3sec, is.numeric)

veldf20d1s <- sapply(veloc20dc1sec, is.numeric)
veldf20d2s <- sapply(veloc20dc2sec, is.numeric)
veldf20d3s <- sapply(veloc20dc3sec, is.numeric)
veldf20d4s <- sapply(veloc20dc4sec, is.numeric)
# velocity
num_data2d2s_v <- veloc2dc2sec[, veldf2d2s]
num_data2d3s_v <- veloc2dc3sec[, veldf2d3s]
num_data2d4s_v <- veloc2dc4sec[, veldf2d4s]
num_data2d6s_v <- veloc2dc6sec[, veldf2d6s]

num_data4d6s_v <- veloc4dc6sec[, veldf4d6s]

num_data10d1s_v <- veloc10dc1sec[, veldf10d1s]
num_data10d3s_v <- veloc10dc3sec[, veldf10d3s]

num_data20d1s_v <- veloc20dc1sec[, veldf20d1s]
num_data20d2s_v <- veloc20dc2sec[, veldf20d2s]
num_data20d3s_v <- veloc20dc3sec[, veldf20d3s]
num_data20d4s_v <- veloc20dc4sec[, veldf20d4s]
#velocity 
# Step 3: Convert the subsetted dataframe to a single vector
all_2dc_v <- c(unlist(num_data2d2s_v, use.names = FALSE), unlist(num_data2d3s_v, use.names = FALSE), unlist(num_data2d4s_v, use.names = FALSE), unlist(num_data2d6s_v, use.names = FALSE))
all_4dc_v <- unlist(num_data4d6s_v, use.names = FALSE)
all_10dc_v <- c(unlist(num_data10d1s_v, use.names = FALSE), unlist(num_data10d3s_v, use.names = FALSE))
all_20dc_v <- c(unlist(num_data20d1s_v, use.names = FALSE), unlist(num_data20d2s_v, use.names = FALSE), unlist(num_data20d3s_v, use.names = FALSE), unlist(num_data20d4s_v, use.names = FALSE))

larg_outliers_v <- sort(all_2dc_v, decreasing = TRUE)[1:5]
filt_2dc_v <- all_2dc_v[!all_2dc_v %in% larg_outliers_v]
larg_outliers_v <- sort(all_4dc_v, decreasing = TRUE)[1:1]
filt_4dc_v <- all_4dc_v[!all_4dc_v %in% larg_outliers_v]

clean2dc_v <- na.omit(filt_2dc_v)
clean10dc_v <- na.omit(all_10dc_v)
clean4dc_v <- na.omit(filt_4dc_v)
clean20dc_v <- na.omit(all_20dc_v)

#vectors <- list(clean2dc_v = clean2dc_v, clean4dc_v = clean4dc_v, clean10dc_v = clean10dc_v, clean20dc_v = clean20dc_v)
vectors <- list(clean2dc_v = clean2dc_v, clean4dc_v = clean4dc_v, clean20dc_v = clean20dc_v)

# Create the data frame using the names of the vectors
allvel <- do.call(rbind, lapply(names(vectors), function(name) {
  data.frame(
    VectorID = name,
    Value = vectors[[name]]
  )
}))

#custom_order <- c("clean2dc_v", "clean4dc_v", "clean10dc_v", "clean20dc_v")
custom_order <- c("clean2dc_v", "clean4dc_v", "clean20dc_v")

# Ensure VectorID is treated as a factor with the specified order
allvel$VectorID <- factor(allvel$VectorID, levels = custom_order)

  # Define custom colors
#custom_colors <- c( "#D25409",  "#B25400", "#AF33A1", "#A13399")
custom_colors <- c( "#00A372",  "#62E4BD", "#B44237")

ggplot(allvel, aes(x = VectorID, y = Value, fill = VectorID)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  #geom_jitter(color = "grey90", size = 0.3, alpha = 0.3) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.2, color = "black", fill = "#000") +
  scale_fill_manual(values = custom_colors) +
  geom_signif(
    comparisons = list(c("clean2dc_v", "clean4dc_v"), c("clean2dc_v", "clean20dc_v"), c("clean4dc_v", "clean20dc_v")),
    map_signif_level = TRUE
  ) +
  theme_minimal() +
  labs(title = "Velocity - directed movement", x = NULL, y = expression(paste("mitochondrial velocity /", mu, "m.s"^-1)))

#top values
#threshold <- 0.2

# Filter the data
#fastest_vel <- allvel[allvel$Value > threshold, ]
# Ensure VectorID is treated as a factor with the specified order
#fastest_vel$VectorID <- factor(fastest_vel$VectorID, levels = custom_order)

top_percentage <- 0.25

# Filter the top 10% highest values for each VectorID
fastest_vel <- allvel %>%
  group_by(VectorID) %>%
  arrange(desc(Value)) %>%
  slice(1:ceiling(n() * top_percentage)) %>%
  ungroup()

# Define custom colors
#custom_colors <- c("#D25409",  "#B25400", "#AF33A1", "#A13399")
custom_colors <- c( "#00A372",  "#62E4BD", "#B44237")

ggplot(fastest_vel, aes(x = VectorID, y = Value, fill = VectorID)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  #geom_jitter(color = "grey90", size = 0.3, alpha = 0.3) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.2, color = "black", fill = "#000") +
  scale_fill_manual(values = custom_colors) +
  geom_signif(
    comparisons = list(c("clean2dc_v", "clean4dc_v"), c("clean2dc_v", "clean20dc_v"), c("clean4dc_v", "clean20dc_v")),
    map_signif_level = TRUE
  ) +
  theme_minimal() +
  labs(title = expression(paste("Fastest fraction - Velocity > 0.5 ",mu,"m.s"^-1)), x = NULL, y = expression(paste("mitochondrial velocity /", mu, "m.s"^-1)))

##### ------ #####

# displacement
disp2dc2sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/2sec/20241111_2dc2sec_displacement.txt", header=FALSE)
disp2dc3sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/3sec/20241111_2dc3sec_displacement.txt", header=FALSE)
disp2dc4sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/4sec/20241111_2dc4sec_displacement.txt", header=FALSE)
disp2dc6sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/2dc/6sec/20241107_2dc6sec_displacement.txt", header=FALSE)

disp4dc6sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/4dc6sec/20241107_4dc6sec_displacement.txt", header=FALSE)

disp10dc1sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/10dc/1sec/20241111_10dc1secWF_displacement.txt", header=FALSE)
disp10dc3sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/10dc/3sec/20241111_10dc3sec_displacement.txt", header=FALSE)

disp20dc1sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/1sec/20241111_WF20dc1sec_displacement.txt", header=FALSE)
disp20dc2sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/2sec/20241107_20dc2sec_displacement.txt", header=FALSE)
disp20dc3sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/3sec/20241111_20dc3sec_displacement.txt", header=FALSE)
disp20dc4sec <- read.csv("C:/Users/anne-/OneDrive/Bureau/20dc/4sec/20241111_20dc4sec_displacement.txt", header=FALSE)

#dispity
dispdf2d2s <- sapply(disp2dc2sec, is.numeric)
dispdf2d3s <- sapply(disp2dc3sec, is.numeric)
dispdf2d4s <- sapply(disp2dc4sec, is.numeric)
dispdf2d6s <- sapply(disp2dc6sec, is.numeric)

dispdf4d6s <- sapply(disp4dc6sec, is.numeric)

dispdf10d1s <- sapply(disp10dc1sec, is.numeric)
dispdf10d3s <- sapply(disp10dc3sec, is.numeric)

dispdf20d1s <- sapply(disp20dc1sec, is.numeric)
dispdf20d2s <- sapply(disp20dc2sec, is.numeric)
dispdf20d3s <- sapply(disp20dc3sec, is.numeric)
dispdf20d4s <- sapply(disp20dc4sec, is.numeric)
# dispity
num_data2d2s_dis <- disp2dc2sec[, dispdf2d2s]
num_data2d3s_dis <- disp2dc3sec[, dispdf2d3s]
num_data2d4s_dis <- disp2dc4sec[, dispdf2d4s]
num_data2d6s_dis <- disp2dc6sec[, dispdf2d6s]

num_data4d6s_dis <- disp4dc6sec[, dispdf4d6s]

num_data10d1s_dis <- disp10dc1sec[, dispdf10d1s]
num_data10d3s_dis <- disp10dc3sec[, dispdf10d3s]

num_data20d1s_dis <- disp20dc1sec[, dispdf20d1s]
num_data20d2s_dis <- disp20dc2sec[, dispdf20d2s]
num_data20d3s_dis <- disp20dc3sec[, dispdf20d3s]
num_data20d4s_dis <- disp20dc4sec[, dispdf20d4s]
#dispity 
# Step 3: Convert the subsetted dataframe to a single vector
all_2dc_dis <- c(unlist(num_data2d2s_dis, use.names = FALSE), unlist(num_data2d3s_dis, use.names = FALSE), unlist(num_data2d4s_dis, use.names = FALSE), unlist(num_data2d6s_dis, use.names = FALSE))
all_4dc_dis <- unlist(num_data4d6s_dis, use.names = FALSE)
all_10dc_dis <- c(unlist(num_data10d1s_dis, use.names = FALSE), unlist(num_data10d3s_dis, use.names = FALSE))
all_20dc_dis <- c(unlist(num_data20d1s_dis, use.names = FALSE), unlist(num_data20d2s_dis, use.names = FALSE), unlist(num_data20d3s_dis, use.names = FALSE), unlist(num_data20d4s_dis, use.names = FALSE))

larg_outliers_dis <- sort(all_2dc_dis, decreasing = TRUE)[1:10]
filt_2dc_dis <- all_2dc_dis[!all_2dc_dis %in% larg_outliers_dis]
larg_outliers_dis <- sort(all_4dc_dis, decreasing = TRUE)[1:10]
filt_4dc_dis <- all_4dc_dis[!all_4dc_dis %in% larg_outliers_dis]

clean2dc_dis <- na.omit(filt_2dc_dis)
clean10dc_dis <- na.omit(all_10dc_dis)
clean4dc_dis <- na.omit(filt_4dc_dis)
clean20dc_dis <- na.omit(all_20dc_dis)


#vectors <- list(clean2dc_dis = clean2dc_dis, clean4dc_dis = clean4dc_dis, clean10dc_dis = clean10dc_dis, clean20dc_dis = clean20dc_dis)
vectors <- list(clean2dc_dis = clean2dc_dis, clean4dc_dis = clean4dc_dis, clean20dc_dis = clean20dc_dis)

# Create the data frame using the names of the vectors
alldisp <- do.call(rbind, lapply(names(vectors), function(name) {
  data.frame(
    VectorID = name,
    Value = vectors[[name]]
  )
}))

#custom_order <- c("clean2dc_dis", "clean4dc_dis", "clean10dc_dis", "clean20dc_dis")
custom_order <- c("clean2dc_dis", "clean4dc_dis", "clean20dc_dis")

# Ensure VectorID is treated as a factor with the specified order
alldisp$VectorID <- factor(alldisp$VectorID, levels = custom_order)

# Define custom colors
#custom_colors <- c( "#025409",  "#0FA400", "#0F33A1", "#0FA399")
custom_colors <- c( "#8A8E52", "#B5B78F", "#AA3839")

ggplot(alldisp, aes(x = VectorID, y = Value, fill = VectorID)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  #geom_jitter(color = "grey90", size = 0.3, alpha = 0.3) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.2, color = "black", fill = "#000") +
  scale_fill_manual(values = custom_colors) +
  geom_signif(
    comparisons = list(c("clean2dc_dis", "clean4dc_dis"), c("clean2dc_dis", "clean20dc_dis"), c("clean4dc_dis", "clean20dc_dis")),
    map_signif_level = TRUE
  ) +
  theme_minimal() +
  labs(title = "Displacement - distance travelled", x = NULL, y = expression(paste("mitochondrial displacement /",mu,"m")))


# Define the threshold
#threshold <- 0.75

# Filter the data
#fastest_disp <- alldisp[alldisp$Value > threshold, ]
# Ensure VectorID is treated as a factor with the specified order
#fastest_disp$VectorID <- factor(fastest_disp$VectorID, levels = custom_order)

top_percentage <- 0.25

# Filter the top 10% highest values for each VectorID
fastest_disp <- alldisp %>%
  group_by(VectorID) %>%
  arrange(desc(Value)) %>%
  slice(1:ceiling(n() * top_percentage)) %>%
  ungroup()

# Define custom colors
#custom_colors <- c( "#025409",  "#0FA400", "#0F33A1", "#0FA399")
custom_colors <- c( "#8A8E52", "#B5B78F", "#AA3839")

ggplot(fastest_disp, aes(x = VectorID, y = Value, fill = VectorID)) +
  geom_violin() +
  #geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  #geom_jitter(color = "grey90", size = 0.3, alpha = 0.3) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.2, color = "black", fill = "#000") +
  scale_fill_manual(values = custom_colors) +
  geom_signif(
    comparisons = list(c("clean2dc_dis", "clean4dc_dis"), c("clean2dc_dis", "clean20dc_dis"), c("clean4dc_dis", "clean20dc_dis")),
    map_signif_level = TRUE
  ) +
  theme_minimal() +
  labs(title = expression(paste("Fastest Displacement > 1.5",mu,"m")), x = NULL, y = expression(paste("mitochondrial displacement /",mu,"m")))
