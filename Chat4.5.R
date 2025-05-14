# Load necessary libraries
library(ggplot2)
library(plotly)
library(reshape2)
library(sqldf)

# Set graphics device to display plots in RStudio
options(device = "RStudioGD")

# Load dataset
data <- read.csv('data.csv', stringsAsFactors = TRUE)

sqldf("SELECT * from data limit 1000;")

# Convert diagnosis to factor with meaningful labels
data$diagnosis <- factor(data$diagnosis,
                         levels = c('B', 'M'),
                         labels = c('Benign', 'Malignant'))

# Remove unwanted columns
if ('Unnamed..32' %in% names(data)) data$Unnamed..32 <- NULL
data$id <- NULL

# 1. Histogram of radius_mean by diagnosis
hist_plot <- ggplot(data, aes(x = radius_mean, fill = diagnosis)) +
  geom_histogram(alpha = 0.6, position = 'identity', bins = 30) +
  theme_minimal() +
  labs(
    title = 'Distribution of Mean Radius by Diagnosis',
    x = 'Mean Radius',
    y = 'Frequency',
    fill = 'Diagnosis'
  )
# Display histogram plot in RStudio
print(hist_plot)
# Alternative display method if print doesn't work
# dev.new(); plot(hist_plot); dev.flush()

# 2. Boxplot of area_mean by diagnosis
box_plot <- ggplot(data, aes(x = diagnosis, y = area_mean, fill = diagnosis)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = 'Boxplot of Mean Area by Diagnosis',
    x = 'Diagnosis',
    y = 'Mean Area',
    fill = 'Diagnosis'
  )
# Display boxplot in RStudio
print(box_plot)
# Alternative display method if print doesn't work
# dev.new(); plot(box_plot); dev.flush()

# 3. Scatter plot: concavity_mean vs concave.points_mean
scatter <- ggplot(data, aes(x = concavity_mean, y = concave.points_mean, color = diagnosis)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(
    title = 'Concavity vs Concave Points by Diagnosis',
    x = 'Concavity Mean',
    y = 'Concave Points Mean',
    color = 'Diagnosis'
  )
# Display the static ggplot version first
print(scatter)

# Convert to interactive plotly
plotly_scatter <- ggplotly(scatter)
# Display interactive plotly version (will appear in Viewer pane)
print(plotly_scatter)

# 4. Correlation Heatmap of mean features
# Select only the mean features
mean_features <- data[, grepl('_mean$', names(data))]

# Compute correlation matrix
corr_matrix <- cor(mean_features, use = 'pairwise.complete.obs')

# Melt for ggplot
df_corr <- melt(corr_matrix)

# Format correlation values for display (round to 2 decimal places)
df_corr$value_text <- round(df_corr$value, 2)

# Heatmap
heatmap <- ggplot(df_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = 'white') +
  # Add text labels with correlation values
  geom_text(aes(label = value_text), color = 'black', size = 3) +
  scale_fill_gradient2(
    low = 'blue', mid = 'white', high = 'red',
    midpoint = 0, limit = c(-1, 1), space = 'Lab',
    name = 'Correlation'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(title = 'Correlation Heatmap of Mean Features')

# Display the static ggplot version first
print(heatmap)

# Interactive heatmap
plotly_heatmap <- ggplotly(heatmap)
# Display interactive plotly version (will appear in Viewer pane)
print(plotly_heatmap)
