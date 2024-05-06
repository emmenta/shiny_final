library(tidyverse)
library(shiny)
library(shinyWidgets)
library(gridExtra)
library(ggExtra)
library(hrbrthemes)
library(viridis)

Heart_Disease <- read_csv("Heart_Disease_Data.csv")
Heart_Disease <- Heart_Disease |> select(-1)
Heart_Disease |> 
  mutate(
    Sex = factor(Sex),
    Chest_paint_type = factor(Chest_paint_type),
    FBS_over_120 = factor(FBS_over_120),
    EKG_results = factor(EKG_results),
    Exercise_angina = factor(Exercise_angina),
    Slope_of_ST = factor(Slope_of_ST),
    Thallium = factor(Thallium),
    Heart_Disease = factor(Heart_Disease)
  ) -> Heart_Disease


ui <- fluidPage(
  titlePanel("Alphonso Saiewane, Emmenta Janneh, Silvy Saint-Jean"),
  
  setBackgroundColor(
    color = c("white", "skyblue"),
    gradient = "linear",
    direction = c("bottom")
    
  ),
  mainPanel(
    h2("Heart Disease Analysis", style = "color:#000080, font-style:bold"),
    tabsetPanel(type = "tab",
                tabPanel("Introduction", uiOutput("intro")),
                tabPanel("Summary", 
                         selectInput("variables", "Select Variable:", choices = names(Heart_Disease)),
                         verbatimTextOutput("summ"),
                         dataTableOutput("table")
                         ),
                tabPanel("Distribution", 
                         selectInput("disvar", "Select Variable:", choices = names(Heart_Disease)),
                         uiOutput("slider"),
                         plotOutput("dist", height = "600px")
                         ),
                tabPanel("Compare",
                         selectInput("x", "Select X-Axis Variable:", choices = names(Heart_Disease)),
                         selectInput("y", "Select Y-Axis Variable:", choices = names(Heart_Disease)),
                         selectInput("categ", "Select Grouping Category Variable:", choices = c("Sex", "Chest_paint_type", "FBS_over_120", "EKG_results", "Exercise_angina", "Slope_of_ST", "Thallium", "Heart_Disease")),
                         uiOutput("lm"),
                         uiOutput("group_lm"),
                         plotOutput("compare")
                )
    )
  )
)

server <- function(input, output, session) {
  output$intro <- renderUI({
  text <- "<p><br>This is a data set used to predict heart disease. Patients were 
              classified as having or not having heart disease based on cardiac 
              catheterization, the gold standard.<br><br>
              If they had more than 50% narrowing of a coronary artery they were 
              labeled as having heart disease. In this cohort, there are 270 patients 
              and there are 13 independent predictive variables or column attributes. 
              The attributes are explained on the website: 
              <a>https://archive.ics.uci.edu/ml/datasets/Heart+Disease</a> <br><br>
              After this dataset became available, the UCI data repository made 
              another cohort available with 303 patients. They shared this with 
              Kaggle which is a data competition initiative. First, the file format 
              is .data which is uncommonly used. Secondly, the outcome was reversed 
              by accident. This is why we are still using the older cohort of patients.<br><br>
              The description of the dataset is as follow:
              <ul>
                  <li><b>Age:</b> in years</li>
                  <li><b>Sex:</b> Male and Female</li>
                  <li><b>Chest pain type:</b> Value 1: typical angina -- Value 2: atypical 
                          angina -- Value 3: non-anginal pain -- Value 4: asymptomatic</li>
                  <li><b>BP:</b> Resting blood pressure (in mm Hg on admission to the hospital)</li>
                  <li><b>Cholesterol:</b> serum cholestoral in mg/dl</li>
                  <li><b>FBS_Over_120:</b> fasting blood sugar</li>
                  <li><b>EKG_results:</b> resting electrocardiographic results</li>
                  <li><b>Max_hr:</b> Maximum heart rate achieved</li>
                  <li><b>Exercise_angina:</b> exercise induced angina</li>
                  <li><b>ST_depression:</b> ST depression induced by exercise relative to rest</li>
                  <li><b>Slope_of_ST:</b> the slope of the peak exercise ST segment</li>
                  <li><b>Number_of_vessels_fluro:</b> number of major vessels (0-3) colored by flourosopy</li>
                  <li><b>Thallium:</b> Normal, Fixed defect, and reversable defect</li>
                  <li><b>Heart disease:</b> Present or Absent</li>
              </ul>
          </p>"
      HTML(text)
    })
  output$summ <- renderPrint({
    summary(Heart_Disease[[input$variables]])
  })
  output$table <- renderDataTable({
    Heart_Disease
  })
  output$slider <- renderUI({
    req(input$disvar) 
    if (is.numeric(Heart_Disease[[input$disvar]])) {
      sliderInput("bin", "Select bin value", value = 20, min = 1, max = 100)
    } else {
      NULL
    }
  })
  output$dist <- renderPlot({
    if(is.numeric(Heart_Disease[[input$disvar]])){
      histogram <- ggplot(Heart_Disease, aes(x = .data[[input$disvar]])) +
        geom_histogram(bins = input$bin, fill = "#69b3a2", color = "black") +
        ggtitle(str_c("Histogram for", input$disvar, sep = " ")) +
        theme(plot.title = element_text(size = 18, hjust = .5))
      
      boxplot <- ggplot(Heart_Disease, aes(y = .data[[input$disvar]])) +
        geom_boxplot(fill = "#69b3a2", color = "black") +
        ggtitle(str_c("Boxplot for", input$disvar, sep = " ")) +
        theme(plot.title = element_text(size = 18, hjust = .5))
      
      dist <- grid.arrange(histogram, boxplot, nrow = 2, heights = c(6,6))
      
      print(dist)
    } else {
      bar <- ggplot(Heart_Disease, aes(x = .data[[input$disvar]], fill = .data[[input$disvar]])) +
        geom_bar() + 
        ggtitle(str_c("Box Plot for", input$disvar, sep = " ")) +
        theme(plot.title = element_text(size = 18, hjust = .5))
      
      pie_data <- data.frame(table(Heart_Disease[[input$disvar]]))
      pie_data$percentage <- pie_data$Freq / sum(pie_data$Freq) * 100
      
      pie <- ggplot(pie_data, aes(x = "", y = percentage, fill = Var1)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        ggtitle(str_c("Pie Chart for ", input$disvar)) +
        theme(plot.title = element_text(size = 18, hjust = .5))
      
      dist <- grid.arrange(pie, bar, nrow = 2, heights = c(6,6))
      
      print(dist)
    }
  })
  output$lm <- renderUI({
    req(input$x, input$y)
    if (is.numeric(Heart_Disease[[input$x]]) && is.numeric(Heart_Disease[[input$y]])){
      checkboxInput("linear_model", "Do you want to show the Linear Model?", value = FALSE)
    }
  })
  output$group_lm <- renderUI({
    req(input$x, input$y, input$categ)
    if (is.numeric(Heart_Disease[[input$x]]) && is.numeric(Heart_Disease[[input$y]])){
      checkboxInput("group_linear_model", "Do you want to show the Linear Model for individual groups?", value = FALSE)
    }
  })
  output$compare <- renderPlot({
    if(is.numeric(Heart_Disease[[input$x]]) && is.numeric(Heart_Disease[[input$y]])){
      plot <- ggplot(Heart_Disease, aes(x = .data[[input$x]] , y = .data[[input$y]], color = .data[[input$categ]])) +
        geom_point() +
        ggtitle(str_c("Scatter Plot for", input$x, "and", input$y, sep = " ")) +
        theme(plot.title = element_text(size = 18, hjust = .5))
      
        if(input$linear_model == TRUE && input$group_linear_model == FALSE){
          plot <- plot + 
            geom_smooth(aes(color = NULL), method = lm, se = FALSE, color = 'black')
        }
        else if(input$linear_model == FALSE && input$group_linear_model == TRUE){
          plot <- plot + 
            geom_smooth(method = lm, se = FALSE)
        }
        else if(input$linear_model == TRUE && input$group_linear_model == TRUE){
          plot <- plot + 
            geom_smooth(aes(color = NULL), method = lm, se = FALSE, color = 'black') + 
            geom_smooth(aes(group = .data[[input$categ]]),method = lm, se = FALSE, linetype = "dashed")
        }
      plot2 <- ggMarginal(plot, type="density", fill = "slateblue")
      print(plot2)
    }
    else if(is.numeric(Heart_Disease[[input$x]]) || is.numeric(Heart_Disease[[input$y]])){
      ggplot(Heart_Disease, aes(x = .data[[input$x]], y = .data[[input$y]], fill = .data[[input$categ]])) +
        geom_violin() +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5)
        ) +
        ggtitle(str_c("Violin Plot for", input$x, "and", input$y, sep = " ")) +
        xlab("")
    }
    else
      ggplot() + ggtitle("Cannot compare two categorical variables") +
      theme(
        legend.position="none",
        plot.title = element_text(size = 18, hjust = 0.5)
      )
  })
  
}

shinyApp(ui, server)

