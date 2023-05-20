library(survival)
library(survminer)
library(ComplexHeatmap)
library(SummarizedExperiment)
library(circlize)
library(RColorBrewer)
library(ggplot2)


source('data.R')

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Survival Plot
  output$suvival_plot <- renderPlot ({
    
      filtered_data <- colAnnotation[colAnnotation$ajcc_stage %in% input$Stages,]
    
      fit <- survfit(Surv(PFI.time.months, OS) ~ ajcc_stage, data = filtered_data)
    
      ggsurvplot(fit, data=filtered_data, 
                                    legend="right",
                                    surv.median.line = 'hv',
                                    legend.title="Stages",
                                    legent.labs = c("Stage I", "Stage II", "Stage III", "Stage IV"),
                                    risk.table = TRUE,
                                    tables.height = 0.5,
                                    tables.theme = theme_cleantable())
  })
  
  # Cox plot
  output$coxph_plot <- renderPlot({
    
    filtered_data <- colAnnotation[colAnnotation$ajcc_stage %in% input$Stages,]
    
    fit <- coxph(Surv(OS.time, OS) ~ ajcc_stage, data = filtered_data)
    
    ggforest(fit, data = filtered_data)
    
  })
  
  # Heat Map
  output$heat_map <- renderPlot({
    
  # Run clustering analysis and plot a large heat map with ComplexHeatmap
  Heatmap(x, col = colors, name = "RNA-seq", 
            column_split = colAnnotation_filt$Tumor_Stage,
            show_row_names = F, show_column_names = F, 
            top_annotation=top_annotation,
            clustering_method_rows = "ward.D2", 
            clustering_distance_rows="spearman",
            clustering_method_columns = "ward.D2", 
            clustering_distance_columns = "spearman")
    
  })
  
  # Simple data distribution of vital status
  output$vital_Status <- renderTable({
    
    filtered_data <- colAnnotation[colAnnotation$ajcc_stage %in% input$Stages,]
    filtered_data$vital_tatus <- ifelse(filtered_data$OS == 1, "Deceased", "Alive")
    table_vital_status <- table(filtered_data$vital_tatus)
    df_vital_status <- data.frame(table_vital_status)
    names(df_vital_status) <- c("Vital Status", "Freq")
    
    df_vital_status
  })
  
  # Gender Distribution
  output$genderPlot <- renderPlot({
    
    gender_counts <- table(colAnnotation$gender)
    df_gender_counts <- data.frame(gender_counts)
    head(df_gender_counts)
    
    names(df_gender_counts) <- c("Gender", "Freq")
    
    # Create a pie chart using ggplot2
    ggplot(df_gender_counts, aes(x = "Gender", y = Freq, fill = Gender)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      labs(fill = "Gender") +
      theme_void() +
      theme(legend.position = "bottom") +
      ggtitle("Which gender shows the higher number of cases?")
    
    
  })
  
  
}
