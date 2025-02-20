## If you don't have the libraries uncomment and install
# install.packages("arules")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("scales")
# install.packages("plotly")
# install.packages("shiny")
# install.packages("shinythemes")
library(shiny)          # Library for creating interactive web applications
library(shinythemes)    # Library to apply themes to Shiny interfaces
library(DT)             # Library for creating interactive tables
library(ggplot2)        # Library for creating visualizations using the Grammar of Graphics
library(dplyr)          # Library for data manipulation and analysis
library(stringr)        # Library for handling text and strings
library(arules)         # Library for generating association rules
library(arulesViz)      # Library for visualizing association rules
library(plotly)         # Library for creating interactive visualizations

# User Interface (UI)
ui <- fluidPage(
  theme = shinytheme("darkly"), # Applying the "darkly" theme to the interface
  titlePanel("Advanced Data Science Dashboard"), # Application title
  sidebarLayout(
    sidebarPanel(
      h4(style = "color: #FFFFFF;", "Input Parameters"), # Header for input section
      fileInput("file", "Upload CSV file", accept = ".csv"), # File input for uploading a CSV file
      actionButton("clean", "Clean Data& RUN"), # Button to trigger data cleaning and execution
      sliderInput("clusters", "Number of Clusters:", min = 2, max = 4, value = 3), # Slider to choose the number of clusters for K-Means
      numericInput("min_support", "Minimum Support:", value = 0.01, min = 0.001, max = 1, step = 0.01), # Input for setting minimum support
      numericInput("min_confidence", "Minimum Confidence:", value = 0.01, min = 0.001, max = 1, step = 0.01), # Input for setting minimum confidence
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("KMeans Clusters", DTOutput("kmeans_table")), # Tab to display K-Means clustering results
        tabPanel("Association Rules", verbatimTextOutput("rule")), # Tab to display association rules
        tabPanel("Visualizations", 
                 fluidRow(
                   column(6, plotOutput("p1")), # Visualization: Payment type distribution
                   column(6, plotOutput("p2"))  # Visualization: Age vs Total Spending
                 ),
                 fluidRow(
                   column(6, plotOutput("p3")), # Visualization: Total Spending by City
                   column(6, plotOutput("p4"))  # Visualization: Spending distribution histogram
                 ),
                 hr(), # Horizontal line for separation
                 h4(style = "color: #FFFFFF;", "K-Means Clusters Visualization"), # Header for K-Means visualizations
                 plotlyOutput("kmeans_plot") # Interactive plot for K-Means clusters
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Load data from the uploaded file
  raw_data <- reactive({
    req(input$file) # Ensure a file is uploaded
    df <- read.csv(input$file$datapath) # Read the uploaded file
    return(df)
  })
  
  # Clean the uploaded data
  cleaned_data <- eventReactive(input$clean, {
    df <- raw_data() # Get the raw data
    df <- df %>%
      distinct() %>% na.omit() %>% # Remove duplicates and NA values
      filter_if(is.numeric, all_vars(. >= 0)) # Remove negative or zero values
    boxplot(df$count) # Check for outliers using a boxplot
    df[!df$count %in% (boxplot.stats(df$count)$out), ] # Remove outliers from data
    df$items <- str_trim(df$items) # Remove leading and trailing spaces
    return(df)
  })
  
  # Apply KMeans clustering
  kmeans_result <- reactive({
    req(cleaned_data()) # Ensure data is cleaned
    df <- cleaned_data() %>% select_if(is.numeric) # Select only numeric columns
    if (ncol(df) < 2) stop("Data must have at least two numeric columns for KMeans clustering.") # Check column requirement
    df_pca <- as.data.frame(prcomp(df, scale. = TRUE)$x[, 1:2]) # Perform PCA for dimensionality reduction
    kmeans(df_pca, centers = input$clusters) # Apply K-Means clustering
  })
  
  # Display KMeans clustering results in a table
  output$kmeans_table <- renderDT({
    req(kmeans_result()) # Ensure clustering is complete
    cluster_data <- cleaned_data() # Get cleaned data
    cluster_data$Cluster <- kmeans_result()$cluster # Add cluster assignments to the data
    datatable(cluster_data) # Display as an interactive table
  })
  
  # Render interactive KMeans cluster plot
  output$kmeans_plot <- renderPlotly({
    req(kmeans_result()) # Ensure clustering is complete
    df <- cleaned_data() %>% select_if(is.numeric) # Select numeric columns
    df_pca <- as.data.frame(prcomp(df, scale. = TRUE)$x[, 1:2]) # Perform PCA
    kmeans_res <- kmeans_result() # Get clustering results
    df_pca$Cluster <- as.factor(kmeans_res$cluster) # Assign clusters to data
    
    p <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(size = 3) +
      labs(title = "K-Means Clustering Visualization", x = "Principal Component 1", y = "Principal Component 2") +
      theme_minimal() # Create a scatter plot of clusters
    ggplotly(p) # Make the plot interactive
  })
  
  # Extract association rules
  output$rule <- renderPrint({
    req(cleaned_data()) # Ensure data is cleaned
    items_list <- strsplit(as.character(cleaned_data()$items), ",") # Split item strings into lists
    transactions <- as(items_list, "transactions") # Convert to transactions format
    rule <- apriori(transactions, 
                    parameter = list(support = input$min_support, confidence = input$min_confidence)) # Apply Apriori algorithm
    if (length(rule) > 0) {
      # Display top rules sorted by confidence
      cat("Top 100 Rules by Confidence:\n")
      inspect(head(sort(rule, by = "confidence"), 100))
      # Display top rules sorted by support
      cat("Top 100 Rules by Support:\n")
      inspect(head(sort(rule, by = "support"), 100))
    } else {
      cat("No rules were generated. Try lowering the support or confidence thresholds.\n") # Message if no rules found
    }
  })
  
  # Visualization 1: Payment Type Distribution
  output$p1 <- renderPlot({
    req(cleaned_data()) # Ensure data is cleaned
    ggplot(cleaned_data(), aes(x = "", fill = paymentType)) +
      geom_bar(width = 1) + coord_polar("y") + # Create a pie chart
      labs(title = "Payment Type Distribution") +
      theme_void() # Remove unnecessary chart elements
  })
  
  # Visualization 2: Age vs Total Spending
  output$p2 <- renderPlot({
    req(cleaned_data()) # Ensure data is cleaned
    ggplot(cleaned_data(), aes(x = factor(age), y = total, fill = factor(age))) +
      geom_bar(stat = "identity") + # Create a bar chart
      labs(title = "Age vs Total Spending", x = "Age", y = "Total Spending") +
      theme_minimal() # Apply a minimal theme
  })
  
  # Visualization 3: Total Spending by City
  output$p3 <- renderPlot({
    req(cleaned_data()) # Ensure data is cleaned
    sorted_data <- cleaned_data() %>%
      group_by(city) %>%
      summarise(total = sum(total, na.rm = TRUE)) %>% # Summarize total spending by city
      arrange(desc(total)) # Sort cities by spending in descending order
    ggplot(sorted_data, aes(x = reorder(city, -total), y = total, fill = city)) +
      geom_bar(stat = "identity") + # Create a bar chart
      labs(title = "Total Spending by City", x = "City", y = "Total Spending") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability
  })
  
  # Visualization 4: Total Spending Distribution
  output$p4 <- renderPlot({
    req(cleaned_data()) # Ensure data is cleaned
    ggplot(cleaned_data(), aes(x = total)) +
      geom_histogram(binwidth = 50, fill = "green", color = "purple") + # Create a histogram
      labs(title = "Total Spending Distribution", x = "Total Spending", y = "Frequency") +
      theme_minimal() # Apply a minimal theme
  })
}

# Run the application
shinyApp(ui = ui, server = server)