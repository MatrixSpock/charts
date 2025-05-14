# Breast Cancer Wisconsin Diagnostic Dataset Visualization
# R Code for MHI 5250 Final Project

# Install required packages if not already installed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("GGally")) install.packages("GGally")
if (!require("plotly")) install.packages("plotly")
if (!require("corrplot")) install.packages("corrplot")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("viridis")) install.packages("viridis")
if (!require("shiny")) install.packages("shiny")

# Load libraries
library(tidyverse)
library(ggplot2)
library(GGally)
library(plotly)
library(corrplot)
library(RColorBrewer)
library(viridis)
library(shiny)

# Set a consistent theme for all visualizations
theme_set(theme_minimal(base_size = 12) + 
          theme(
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            legend.position = "bottom",
            legend.title = element_text(size = 10),
            axis.title = element_text(size = 11),
            panel.grid.minor = element_line(color = "gray90"),
            panel.grid.major = element_line(color = "gray85"),
            panel.border = element_rect(color = "gray70", fill = NA, size = 0.5)
          ))

# Define color palette
benign_color <- "#2563eb"  # Blue
malignant_color <- "#dc2626"  # Red
diagnosis_colors <- c("B" = benign_color, "M" = malignant_color)

# Load data
# Modify path as needed or use file.choose()
data_path <- "data.csv"  # Replace with your file path
# If you prefer to select the file manually, uncomment the line below:
# data_path <- file.choose()

# Read the data
cancer_data <- read.csv(data_path, stringsAsFactors = FALSE)

# Basic data cleaning
# Remove any empty column that might exist at the end
cancer_data <- cancer_data[, colSums(is.na(cancer_data)) < nrow(cancer_data)]

# Convert diagnosis to factor
cancer_data$diagnosis <- as.factor(cancer_data$diagnosis)

# Check dimensions and column names
cat("Dataset dimensions:", dim(cancer_data), "\n")
cat("Column names:", paste(colnames(cancer_data)[1:10], collapse=", "), "...\n")

# Summary statistics
summary_stats <- cancer_data %>%
  group_by(diagnosis) %>%
  summarize(
    count = n(),
    radius_mean_avg = mean(radius_mean),
    concave_points_mean_avg = mean(`concave.points_mean`),
    area_mean_avg = mean(area_mean),
    concavity_mean_avg = mean(concavity_mean)
  )

print(summary_stats)

#------------------------------------------------------------------------------
# 1. Box Plot Comparison of Key Features
#------------------------------------------------------------------------------

# Prepare data in long format for box plots
key_features <- c("radius_mean", "concave.points_mean", "area_mean", "concavity_mean")
box_plot_data <- cancer_data %>%
  select(diagnosis, all_of(key_features)) %>%
  pivot_longer(cols = -diagnosis, 
               names_to = "feature", 
               values_to = "value")

# Add a factor level for ordered display
box_plot_data$feature <- factor(box_plot_data$feature, 
                             levels = key_features,
                             labels = c("Radius", "Concave Points", "Area", "Concavity"))

# Create the box plot
boxplot_chart <- ggplot(box_plot_data, aes(x = feature, y = value, fill = diagnosis)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1) +
  scale_fill_manual(values = diagnosis_colors, 
                   name = "Diagnosis",
                   labels = c("Benign", "Malignant")) +
  labs(title = "Feature Distribution Comparison",
       subtitle = "Between Benign and Malignant Tumors",
       x = "Feature",
       y = "Value (normalized scale)",
       caption = "Note: Area values scaled down to fit on the same axis") +
  facet_wrap(~ feature, scales = "free_y") +
  theme(strip.text = element_blank()) # Remove duplicate feature labels

print(boxplot_chart)

# Save the plot
ggsave("boxplot_comparison.png", boxplot_chart, width = 10, height = 6, dpi = 300)

# Interactive version with plotly
boxplot_interactive <- ggplotly(boxplot_chart)
boxplot_interactive

#------------------------------------------------------------------------------
# 2. Scatter Plot with Decision Boundary
#------------------------------------------------------------------------------

# Determine the best threshold for radius_mean
# Calculate accuracy for different threshold values
calculate_accuracy <- function(data, feature, threshold) {
  predictions <- ifelse(data[[feature]] >= threshold, "M", "B")
  accuracy <- mean(predictions == data$diagnosis)
  return(accuracy)
}

# Find optimal threshold
radius_thresholds <- seq(min(cancer_data$radius_mean), 
                        max(cancer_data$radius_mean), 
                        length.out = 100)
accuracy_results <- data.frame(
  threshold = radius_thresholds,
  accuracy = sapply(radius_thresholds, function(t) 
    calculate_accuracy(cancer_data, "radius_mean", t))
)

best_threshold <- accuracy_results[which.max(accuracy_results$accuracy), ]
cat("Best threshold for radius_mean:", best_threshold$threshold, 
    "with accuracy:", best_threshold$accuracy * 100, "%\n")

# Create the scatter plot with decision boundary
scatter_plot <- ggplot(cancer_data, 
                      aes(x = radius_mean, 
                          y = `concave.points_mean`, 
                          color = diagnosis)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_vline(xintercept = best_threshold$threshold, 
             linetype = "dashed", 
             color = "#ff7300",
             size = 1) +
  annotate("text", 
           x = best_threshold$threshold + 1.5, 
           y = max(cancer_data$`concave.points_mean`) * 0.9,
           label = paste0("Threshold: ", round(best_threshold$threshold, 2)),
           color = "#ff7300",
           fontface = "bold") +
  annotate("text",
           x = best_threshold$threshold + 1.5,
           y = max(cancer_data$`concave.points_mean`) * 0.8,
           label = paste0("Accuracy: ", round(best_threshold$accuracy * 100, 1), "%"),
           color = "#ff7300") +
  scale_color_manual(values = diagnosis_colors,
                    name = "Diagnosis",
                    labels = c("Benign", "Malignant")) +
  labs(title = "Scatter Plot: Radius Mean vs. Concave Points Mean",
       subtitle = "With Decision Boundary for Classification",
       x = "Radius Mean",
       y = "Concave Points Mean",
       caption = "Decision boundary at optimal threshold for radius_mean")

print(scatter_plot)

# Save the plot
ggsave("scatter_plot.png", scatter_plot, width = 10, height = 6, dpi = 300)

# Interactive version with plotly
scatter_interactive <- ggplotly(scatter_plot)
scatter_interactive

#------------------------------------------------------------------------------
# 3. Feature Importance Bar Chart
#------------------------------------------------------------------------------

# Calculate feature importance as ratio of malignant to benign means
feature_means <- cancer_data %>%
  select(diagnosis, contains("_mean")) %>%
  group_by(diagnosis) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(cols = -diagnosis, names_to = "feature", values_to = "mean")

feature_ratios <- feature_means %>%
  pivot_wider(names_from = diagnosis, values_from = mean) %>%
  mutate(ratio = M / B) %>%
  arrange(desc(ratio))

# Keep only the top features (remove _mean suffix for display)
top_features <- feature_ratios %>%
  filter(feature != "symmetry_mean" & feature != "fractal_dimension_mean") %>%
  mutate(feature_name = gsub("_mean", "", feature)) %>%
  mutate(feature_name = str_to_title(gsub("_", " ", feature_name))) %>%
  head(9)

# Create the bar chart
feature_importance_chart <- ggplot(top_features, aes(x = reorder(feature_name, ratio), y = ratio)) +
  geom_bar(stat = "identity", aes(fill = ratio), width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Feature Importance",
       subtitle = "Ratio of Malignant to Benign Mean Values",
       x = NULL,
       y = "Ratio (Malignant/Benign)",
       caption = "Higher values indicate features that differ more significantly between malignant and benign tumors") +
  theme(legend.position = "none")

print(feature_importance_chart)

# Save the plot
ggsave("feature_importance.png", feature_importance_chart, width = 10, height = 6, dpi = 300)

# Interactive version with plotly
importance_interactive <- ggplotly(feature_importance_chart)
importance_interactive

#------------------------------------------------------------------------------
# 4. Correlation Matrix Heatmap
#------------------------------------------------------------------------------

# Select only the mean features
mean_features <- cancer_data %>%
  select(contains("_mean"))

# Compute correlation matrix
correlation_matrix <- cor(mean_features)

# Create correlation plot
png("correlation_matrix.png", width = 1200, height = 1000, res = 120)
corrplot(correlation_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         diag = TRUE,
         col = colorRampPalette(c("#3182CE", "#F7FAFC", "#E53E3E"))(200),
         main = "Correlation Matrix of Feature Measurements",
         mar = c(0,0,2,0))
dev.off()

# Display correlation matrix
corrplot(correlation_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         diag = TRUE,
         col = colorRampPalette(c("#3182CE", "#F7FAFC", "#E53E3E"))(200),
         main = "Correlation Matrix of Feature Measurements")

#------------------------------------------------------------------------------
# 5. Interactive Shiny Dashboard (Optional)
#------------------------------------------------------------------------------

# This is a basic interactive dashboard that you can run to explore the data
# Uncomment and run the code below to launch the Shiny app

# shinyApp(
#   ui = fluidPage(
#     titlePanel("Breast Cancer Wisconsin Diagnostic Data Explorer"),
#     
#     sidebarLayout(
#       sidebarPanel(
#         selectInput("xFeature", "X-Axis Feature:", 
#                     choices = names(mean_features),
#                     selected = "radius_mean"),
#         selectInput("yFeature", "Y-Axis Feature:", 
#                     choices = names(mean_features),
#                     selected = "concave.points_mean"),
#         sliderInput("threshold", "Classification Threshold:",
#                     min = min(cancer_data$radius_mean),
#                     max = max(cancer_data$radius_mean),
#                     value = best_threshold$threshold),
#         checkboxGroupInput("diagnosisFilter", "Filter by Diagnosis:",
#                          choices = c("Benign" = "B", "Malignant" = "M"),
#                          selected = c("B", "M")),
#         hr(),
#         helpText("Adjust the controls to explore different feature relationships.")
#       ),
#       
#       mainPanel(
#         tabsetPanel(
#           tabPanel("Scatter Plot", plotOutput("scatterPlot")),
#           tabPanel("Feature Importance", plotOutput("importancePlot")),
#           tabPanel("Correlation Matrix", plotOutput("correlationPlot")),
#           tabPanel("Data Summary", verbatimTextOutput("summaryStats"))
#         )
#       )
#     )
#   ),
#   
#   server = function(input, output) {
#     # Filter data based on diagnosis selection
#     filteredData <- reactive({
#       cancer_data %>%
#         filter(diagnosis %in% input$diagnosisFilter)
#     })
#     
#     # Calculate accuracy based on selected threshold
#     currentAccuracy <- reactive({
#       predictions <- ifelse(cancer_data[[input$xFeature]] >= input$threshold, "M", "B")
#       mean(predictions == cancer_data$diagnosis) * 100
#     })
#     
#     # Scatter plot
#     output$scatterPlot <- renderPlot({
#       ggplot(filteredData(), aes_string(x = input$xFeature, y = input$yFeature, color = "diagnosis")) +
#         geom_point(alpha = 0.7, size = 3) +
#         geom_vline(xintercept = input$threshold, linetype = "dashed", color = "#ff7300", size = 1) +
#         annotate("text", 
#                  x = input$threshold + (max(cancer_data[[input$xFeature]]) - min(cancer_data[[input$xFeature]])) * 0.05, 
#                  y = max(cancer_data[[input$yFeature]]) * 0.9,
#                  label = paste0("Accuracy: ", round(currentAccuracy(), 1), "%"),
#                  color = "#ff7300") +
#         scale_color_manual(values = diagnosis_colors,
#                           name = "Diagnosis",
#                           labels = c("Benign", "Malignant")) +
#         labs(title = paste("Scatter Plot:", input$xFeature, "vs.", input$yFeature),
#              x = input$xFeature,
#              y = input$yFeature)
#     })
#     
#     # Feature importance plot
#     output$importancePlot <- renderPlot({
#       feature_importance_chart
#     })
#     
#     # Correlation plot
#     output$correlationPlot <- renderPlot({
#       corrplot(correlation_matrix, 
#                method = "color", 
#                type = "upper", 
#                order = "hclust",
#                addCoef.col = "black",
#                tl.col = "black", 
#                tl.srt = 45,
#                diag = TRUE,
#                col = colorRampPalette(c("#3182CE", "#F7FAFC", "#E53E3E"))(200),
#                main = "Correlation Matrix of Feature Measurements")
#     })
#     
#     # Summary statistics
#     output$summaryStats <- renderPrint({
#       cat("Dataset Overview:\n")
#       cat("Total samples:", nrow(cancer_data), "\n")
#       cat("Benign samples:", sum(cancer_data$diagnosis == "B"), "\n")
#       cat("Malignant samples:", sum(cancer_data$diagnosis == "M"), "\n\n")
#       
#       cat("Current Feature Statistics:\n")
#       summary(cancer_data[c(input$xFeature, input$yFeature)])
#     })
#   }
# )

#------------------------------------------------------------------------------
# Code to generate all charts for the poster at once
#------------------------------------------------------------------------------

# Create a directory for the outputs if it doesn't exist
dir.create("poster_charts", showWarnings = FALSE)

# Save all charts
ggsave("poster_charts/boxplot_comparison.png", boxplot_chart, width = 10, height = 6, dpi = 300)
ggsave("poster_charts/scatter_plot.png", scatter_plot, width = 10, height = 6, dpi = 300)
ggsave("poster_charts/feature_importance.png", feature_importance_chart, width = 10, height = 6, dpi = 300)

# Save correlation matrix
png("poster_charts/correlation_matrix.png", width = 1200, height = 1000, res = 120)
corrplot(correlation_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         diag = TRUE,
         col = colorRampPalette(c("#3182CE", "#F7FAFC", "#E53E3E"))(200),
         main = "Correlation Matrix of Feature Measurements",
         mar = c(0,0,2,0))
dev.off()

cat("\nAll visualizations have been created and saved to the 'poster_charts' directory.\n")