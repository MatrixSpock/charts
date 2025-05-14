# Interactive Breast Cancer Data Explorer with Shiny
# This app provides multiple linked visualizations for exploring the Wisconsin Breast Cancer dataset
# install.packages("shiny")
# install.packages("corrplot")
# install.packages("factoextra")
# install.packages("GGally")
# install.packages("shinydashboard")
# install.packages("ggrepel")

library(shiny)
library(tidyverse)
library(plotly)
library(corrplot)
library(DT)
library(factoextra)
library(GGally)
library(shinydashboard)
library(RColorBrewer)
library(ggrepel)

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Breast Cancer Data Explorer"),
  
  dashboardSidebar(
    checkboxGroupInput("filter_diagnosis", "Filter Diagnosis:",
                       choices = c("Benign", "Malignant"),
                       selected = c("Benign", "Malignant")),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Feature Explorer", tabName = "features", icon = icon("chart-simple")),
      menuItem("Correlation Analysis", tabName = "correlation", icon = icon("chart-line")),
      menuItem("PCA Visualization", tabName = "pca", icon = icon("braille")),
      menuItem("About the Data", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Introduction",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  HTML("<p>This interactive app explores the Wisconsin Breast Cancer Diagnostic dataset, allowing users to filter by diagnosis and examine feature distributions, correlations, and PCA visualizations.</p>")
                )
              ),
              fluidRow(
                box(
                  title = "Diagnosis Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("diagnosisPlot", height = 300)
                ),
                box(
                  title = "Feature Summary Statistics",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("featureSummaryPlot", height = 300)
                )
              ),
              fluidRow(
                box(
                  title = "Key Features Comparison",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("keyFeaturesPlot", height = 700)
                )
              )
      ),
      
      # Feature Explorer tab
      tabItem(tabName = "features",
              fluidRow(
                box(
                  title = "Select Features",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("xfeature", "X-axis Feature:", 
                              choices = NULL,
                              selected = "radius_mean"),
                  selectInput("yfeature", "Y-axis Feature:", 
                              choices = NULL,
                              selected = "texture_mean"),
                  checkboxInput("add_ellipse", "Add Classification Ellipses", TRUE),
                  selectInput("color_by", "Color by:",
                              choices = c("diagnosis"),
                              selected = "diagnosis"),
                  hr(),
                  helpText("Select different features to visualize their relationship.")
                ),
                box(
                  title = "Interactive Scatter Plot",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput("scatterPlot", height = 500)
                )
              ),
              fluidRow(
                box(
                  title = "Selected Feature Data",
                  width = 12,
                  DT::dataTableOutput("featureTable")
                )
              )
      ),
      
      # Correlation Analysis tab
      tabItem(tabName = "correlation",
              fluidRow(
                box(
                  title = "Feature Correlation Settings",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("corr_type", "Correlation Type:",
                              choices = c("All Features" = "all",
                                          "Mean Features" = "mean",
                                          "SE Features" = "se",
                                          "Worst Features" = "worst"),
                              selected = "all"),
                  sliderInput("corr_threshold", "Correlation Threshold:",
                              min = 0, max = 1, value = 0.7, step = 0.05),
                  hr(),
                  helpText("Adjust settings to filter the correlation matrix.")
                ),
                box(
                  title = "Correlation Heatmap",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput("corrPlot", height = 550)
                )
              )
      ),
      
      # PCA Visualization tab
      tabItem(tabName = "pca",
              fluidRow(
                box(
                  title = "PCA Settings",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("pca_features", "Feature Set:",
                              choices = c("All Features" = "all",
                                          "Mean Features" = "mean",
                                          "SE Features" = "se",
                                          "Worst Features" = "worst"),
                              selected = "all"),
                  checkboxInput("show_loadings", "Show Feature Loadings", TRUE),
                  checkboxInput("show_ellipses", "Show Classification Ellipses", TRUE),
                  sliderInput("n_components", "Number of Components to Plot:",
                              min = 2, max = 5, value = 2, step = 1)
                ),
                box(
                  title = "PCA Plot",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput("pcaPlot", height = 600)
                )
              ),
              fluidRow(
                box(
                  title = "Component Variance Explained",
                  width = 12,
                  plotlyOutput("variancePlot", height = 300)
                )
              )
      ),
      
      # About the Data tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About the Wisconsin Breast Cancer Dataset",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  HTML("<p>This dataset contains features computed from digitized images of fine needle 
                 aspirates of breast masses. The features describe characteristics of the cell nuclei 
                 present in the images.</p>
                 
                 <h4>Features:</h4>
                 <ul>
                   <li><strong>ID number</strong>: Identification number</li>
                   <li><strong>Diagnosis</strong>: Diagnosis (M = malignant, B = benign)</li>
                   <li><strong>Ten real-valued features</strong> computed for each cell nucleus:</li>
                   <ul>
                     <li>radius (mean of distances from center to points on the perimeter)</li>
                     <li>texture (standard deviation of gray-scale values)</li>
                     <li>perimeter</li>
                     <li>area</li>
                     <li>smoothness (local variation in radius lengths)</li>
                     <li>compactness (perimeter^2 / area - 1.0)</li>
                     <li>concavity (severity of concave portions of the contour)</li>
                     <li>concave points (number of concave portions of the contour)</li>
                     <li>symmetry</li>
                     <li>fractal dimension (\"coastline approximation\" - 1)</li>
                   </ul>
                 </ul>
                 
                 <p>For each feature, three values are provided:</p>
                 <ul>
                   <li><strong>mean</strong>: Mean value for the cell nuclei</li>
                   <li><strong>se</strong>: Standard error for the cell nuclei</li>
                   <li><strong>worst</strong>: Mean of the three largest values for the cell nuclei</li>
                 </ul>
                 
                 <h4>Source:</h4>
                 <p>Breast Cancer Wisconsin (Diagnostic) Data Set, UCI Machine Learning Repository</p>
                 <p>Creators: Dr. William H. Wolberg, W. Nick Street, and Olvi L. Mangasarian</p>
                 <p>Dataset also available on <a href=\"https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data\">Kaggle</a>.</p>")
                )
              ),
              fluidRow(
                box(
                  title = "Dataset Statistics",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("datasetStats")
                )
              ),
              fluidRow(
                box(
                  title = "Conclusion",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  HTML("<p>The analysis shows that malignant tumors tend to have larger mean radii, perimeter, and area. PCA effectively separates the two classes, indicating feature patterns that could support classification models.</p>")
                ),
                box(
                  title = "Recommendations",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  HTML("<p>Future work should include building predictive models, exploring additional feature engineering, and validating results on external datasets.</p>")
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Load the data
  bc_data <- reactive({
    # In a real app, you would replace this with the actual data loading
    # For now, we'll use the read.csv function assuming the file has been downloaded
    data <- read.csv("data.csv")
    
    # Data cleaning
    data <- data %>% select(-id)  # Remove ID column
    data$diagnosis <- as.factor(data$diagnosis)
    levels(data$diagnosis) <- c("Benign", "Malignant")
    
    # Apply diagnosis filter
    data <- data %>% filter(diagnosis %in% input$filter_diagnosis)
    
    return(data)
  })
  
  # Update UI inputs once data is loaded
  observe({
    data <- bc_data()
    feature_names <- names(data)[names(data) != "diagnosis"]
    
    # Update the feature selection dropdowns
    updateSelectInput(session, "xfeature", choices = feature_names, selected = "radius_mean")
    updateSelectInput(session, "yfeature", choices = feature_names, selected = "texture_mean")
  })
  
  # Function to filter features based on type (mean, se, worst)
  filtered_features <- reactive({
    data <- bc_data()
    feature_type <- input$pca_features
    
    if (feature_type == "all") {
      return(data %>% select(-diagnosis))
    } else if (feature_type == "mean") {
      return(data %>% select(ends_with("_mean")))
    } else if (feature_type == "se") {
      return(data %>% select(ends_with("_se")))
    } else if (feature_type == "worst") {
      return(data %>% select(ends_with("_worst")))
    }
  })
  
  # Diagnosis distribution plot
  output$diagnosisPlot <- renderPlotly({
    data <- bc_data()
    
    diagnosis_counts <- data %>%
      count(diagnosis) %>%
      mutate(percentage = n / sum(n) * 100)
    
    p <- ggplot(diagnosis_counts, aes(x = diagnosis, y = n, fill = diagnosis)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
                position = position_stack(vjust = 0.5), color = "white", size = 4) +
      labs(x = "Diagnosis", y = "Count") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
    
    ggplotly(p) %>% 
      layout(showlegend = FALSE)
  })
  
  # Feature summary boxplot
  output$featureSummaryPlot <- renderPlotly({
    data <- bc_data()
    
    # Select a few key features for the summary
    key_features <- data %>% 
      select(diagnosis, radius_mean, texture_mean, perimeter_mean, area_mean) %>%
      pivot_longer(cols = -diagnosis, names_to = "feature", values_to = "value")
    
    p <- ggplot(key_features, aes(x = feature, y = value, fill = diagnosis)) +
      geom_boxplot() +
      labs(x = "Feature", y = "Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1")
    
    ggplotly(p)
  })
  
  # Key features pairs plot
  output$keyFeaturesPlot <- renderPlotly({
    data <- bc_data()
    
    # Select key features for the pairs plot
    key_data <- data %>% 
      select(diagnosis, radius_mean, texture_mean, perimeter_mean, area_mean, concavity_mean)
    
    # Create a subset of the data for performance
    key_data_sample <- key_data
    if (nrow(key_data) > 200) {
      set.seed(123)
      key_data_sample <- key_data[sample(nrow(key_data), 200), ]
    }
    
    # Create pairs plot with ggpairs
    p <- GGally::ggpairs(
      key_data_sample, 
      columns = 2:6,
      aes(color = diagnosis),
      upper = list(continuous = "cor"),
      lower = list(continuous = "points"),
      diag = list(continuous = "densityDiag"),
      title = "Relationships Between Key Features",
      axisLabels = "show",
      progress = FALSE
    ) + 
      theme_minimal() +
      theme(
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 9),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      ) +
      scale_color_brewer(palette = "Set1") +
      scale_fill_brewer(palette = "Set1")
    
    ggplotly(p, height = 700)
  })
  
  # Interactive scatter plot
  output$scatterPlot <- renderPlotly({
    data <- bc_data()
    
    p <- ggplot(data, aes_string(x = input$xfeature, y = input$yfeature, color = input$color_by)) +
      geom_point(alpha = 0.7, size = 3) +
      labs(title = paste("Scatter Plot of", input$xfeature, "vs", input$yfeature),
           x = input$xfeature, 
           y = input$yfeature) +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
    
    if(input$add_ellipse) {
      p <- p + stat_ellipse(aes(fill = diagnosis), alpha = 0.2, geom = "polygon")
    }
    
    ggplotly(p)
  })
  
  # Feature data table
  output$featureTable <- DT::renderDataTable({
    data <- bc_data()
    
    selected_data <- data %>%
      select(diagnosis, input$xfeature, input$yfeature)
    
    DT::datatable(selected_data, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Correlation matrix for selected feature type
  output$corrPlot <- renderPlotly({
    data <- bc_data()
    
    # Filter features based on selection
    if (input$corr_type == "all") {
      features <- data %>% select(-diagnosis)
    } else if (input$corr_type == "mean") {
      features <- data %>% select(ends_with("_mean"))
    } else if (input$corr_type == "se") {
      features <- data %>% select(ends_with("_se"))
    } else {
      features <- data %>% select(ends_with("_worst"))
    }
    
    # Calculate correlation matrix
    correlation_matrix <- cor(features)
    
    # Apply threshold filter if needed
    if (input$corr_threshold > 0) {
      correlation_matrix[abs(correlation_matrix) < input$corr_threshold] <- NA
    }
    
    # Convert to data frame for plotly
    corr_data <- as.data.frame(as.table(correlation_matrix))
    names(corr_data) <- c("Feature1", "Feature2", "Correlation")
    
    # Remove NA values
    corr_data <- corr_data %>% filter(!is.na(Correlation))
    
    # Create label column for displaying correlation values on heatmap
    corr_data$label <- round(corr_data$Correlation, 2)
    
    # Create heatmap
    plot_ly(
      data = corr_data,
      x = ~Feature1,
      y = ~Feature2,
      z = ~Correlation,
      type = "heatmap",
      text = ~label,
      texttemplate = "%{text}",
      textfont = list(color = "black"),
      colorscale = "RdBu",
      zmin = -1,
      zmax = 1,
      colorbar = list(title = "Correlation")
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
  
  # PCA analysis
  pca_result <- reactive({
    # Get filtered features
    features <- filtered_features()
    
    # Scale data
    scaled_data <- scale(features)
    
    # Perform PCA
    prcomp(scaled_data)
  })
  
  # PCA plot
  output$pcaPlot <- renderPlotly({
    data <- bc_data()
    pca <- pca_result()
    
    # Get the number of components to plot
    n_comp <- min(input$n_components, ncol(pca$x))
    
    # Create data frame for plotting
    pca_data <- as.data.frame(pca$x[, 1:n_comp])
    pca_data$diagnosis <- data$diagnosis
    
    # If we're showing only 2 components, use a scatter plot
    if (n_comp == 2) {
      p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = diagnosis)) +
        geom_point(alpha = 0.7, size = 3) +
        labs(title = "PCA Visualization",
             x = paste0("PC1 (", round(summary(pca)$importance[2, 1] * 100, 1), "% variance)"),
             y = paste0("PC2 (", round(summary(pca)$importance[2, 2] * 100, 1), "% variance)")) +
        theme_minimal() +
        scale_color_brewer(palette = "Set1")
      
      if (input$show_ellipses) {
        p <- p + stat_ellipse(aes(fill = diagnosis), alpha = 0.2, geom = "polygon")
      }
      
      if (input$show_loadings) {
        # Extract loadings
        loadings <- as.data.frame(pca$rotation[, 1:2])
        loadings$feature <- rownames(loadings)
        
        # Scale loadings for visualization
        scaling_factor <- 5
        loadings$PC1 <- loadings$PC1 * scaling_factor
        loadings$PC2 <- loadings$PC2 * scaling_factor
        
        # Add arrows
        p <- p +
          geom_segment(data = loadings, 
                       aes(x = 0, y = 0, xend = PC1, yend = PC2),
                       arrow = arrow(length = unit(0.2, "cm")), 
                       color = "darkgray") +
          geom_text(data = loadings, 
                    aes(x = PC1 * 1.1, y = PC2 * 1.1, label = feature),
                    color = "black", size = 3)
        # p <- p +
        #   geom_segment(data = loadings,
        #               aes(x = 0, y = 0, xend = PC1, yend = PC2),
        #               arrow = arrow(length = unit(0.2, "cm")),
        #               color = "darkgray") +
        #   geom_text_repel(data = loadings,
        #                   aes(x = PC1, y = PC2, label = feature),
        #                   # you can tweak these:
        #                   size         = 3,
        #                   box.padding  = 0.5, 
        #                   point.padding= 0.3,
        #                   segment.size = 0.2,
        #                   segment.color= "grey50")
      }
      
      ggplotly(p)
    } else {
      # For more than 2 components, use pairs plot
      pairs_data <- pca_data %>% select(1:n_comp, diagnosis)
      
      # Create pairs plot with GGally
      p <- GGally::ggpairs(
        pairs_data,
        columns = 1:n_comp,
        aes(color = diagnosis),
        upper = list(continuous = "cor"),
        lower = list(continuous = "points"),
        diag = list(continuous = "densityDiag"),
        title = "PCA Components Relationships"
      ) + 
        theme_minimal() +
        scale_color_brewer(palette = "Set1") +
        scale_fill_brewer(palette = "Set1")
      
      ggplotly(p)
    }
  })
  
  # Variance explained plot
  output$variancePlot <- renderPlotly({
    pca <- pca_result()
    
    # Calculate variance explained
    var_explained <- summary(pca)$importance[2, ] * 100
    var_data <- data.frame(
      Component = paste0("PC", 1:length(var_explained)),
      VarExplained = var_explained
    )
    
    # Calculate cumulative variance
    var_data$Cumulative <- cumsum(var_explained)
    
    # Reshape for plotting
    var_long <- var_data %>%
      select(Component, VarExplained, Cumulative) %>%
      pivot_longer(cols = c(VarExplained, Cumulative),
                   names_to = "Type", values_to = "Value")
    
    # Create plot
    p <- ggplot(var_long, aes(x = Component, y = Value, fill = Type, group = Type)) +
      geom_col(data = subset(var_long, Type == "VarExplained"), alpha = 0.7) +
      geom_line(data = subset(var_long, Type == "Cumulative"), aes(color = Type), size = 1) +
      geom_point(data = subset(var_long, Type == "Cumulative"), aes(color = Type), size = 2) +
      scale_fill_manual(values = c("VarExplained" = "#619CFF", "Cumulative" = "#F8766D"),
                        labels = c("Individual", "Cumulative")) +
      scale_color_manual(values = c("VarExplained" = "#619CFF", "Cumulative" = "#F8766D"),
                         labels = c("Individual", "Cumulative")) +
      labs(title = "PCA Variance Explained",
           x = "Principal Component",
           y = "Variance Explained (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Dataset statistics table
  output$datasetStats <- DT::renderDataTable({
    data <- bc_data()
    
    # Calculate basic statistics for each feature
    stats_df <- data %>%
      select(-diagnosis) %>%
      summarise(across(everything(), 
                       list(min = min, 
                            q1 = ~quantile(., 0.25), 
                            median = median, 
                            mean = mean, 
                            q3 = ~quantile(., 0.75), 
                            max = max,
                            sd = sd)))
    
    # Reshape to long format
    stats_long <- data.frame(
      Feature = rep(names(data %>% select(-diagnosis)), each = 7),
      Statistic = rep(c("Min", "Q1", "Median", "Mean", "Q3", "Max", "SD"), times = ncol(data %>% select(-diagnosis))),
      Value = as.numeric(as.matrix(stats_df))
    )
    
    # Convert to wide format for display
    stats_wide <- stats_long %>%
      pivot_wider(names_from = Statistic, values_from = Value)
    
    # Format to 3 decimal places
    stats_wide <- stats_wide %>%
      mutate(across(where(is.numeric), ~round(., 3)))
    
    DT::datatable(stats_wide,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
}

# Run the app (uncomment to run)
shinyApp(ui = ui, server = server)



# Load dataset
datav2 <- read.csv('data.csv', stringsAsFactors = TRUE)

sqldf("SELECT * from datav2 limit 1000;")
