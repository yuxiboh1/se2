#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# shiny
library(shiny)
library(shinydashboard)

# load data
aba = read.table("./abalone.data", sep = ",")
colnames(aba) = c("Sex","Length","Diameter","Height","Whole_weight",
                  "Shucked_weight","Viscera_weight","Shell_weight","Rings")
allpredictors = colnames(aba)[1:8]

# UI
ui <- dashboardPage(
    dashboardHeader(title = "Case Study 3"),
    dashboardSidebar(
        ## Sidebar content
        dashboardSidebar(
            sidebarMenu(
                menuItem("description", tabName = "description"),
                menuItem("analysis", tabName = "analysis"),
                menuItem("algorithm", tabName = "algorithm"),
                menuItem("interactive modeling", tabName = "modeling"),
                menuItem("findings", tabName = "findings")
               
            )
        )
    ),
    dashboardBody(
        tabItems(
            
            # First tab content
            tabItem(tabName = "description",
                    box(h3("What data did I collect ?"),
                        p("The data is from ："),
                        a(href = "https://archive.ics.uci.edu/ml/datasets/Abalone", 
                          "https://archive.ics.uci.edu/ml/datasets/Abalone"),
                        p("The Abalone dataset contains 4177 instances with 8 attributes"),
                        p("The attributes includes Sex,Length,Diameter,Height,Whole_weight,Shucked_weight,Viscera_weight,Shell_weight,Rings"),
                        p("The purpose of this project is predicting the age of abalone from physical measurements"),
                        p("Since for the age of abalone is Rings + 1.5, the purpose is equivalent to predicting the Rings of abalone from physical measurements"),
                        width = 12),
                    box(h3("Why this topic is interesting or important ?"),
                        p("The age of abalone is determined by cutting the shell through the cone, staining it, and counting the number of rings through a microscope -- which is a boring and time-consuming task."),
                        p("Therefore, it is very interesting and important to use other measurements, which are easier to obtain, to predict the age."),
                        width = 12),
                    box(h3("How did I analyze the data ?"),
                        p("Please refer to the third tab ."),
                        width = 12),
                    box(h3("What did I find in the data ?"),
                        p("Please refer to the fifth tab ."),
                        width = 12)
            ),
            
            # Second tab content
            tabItem(tabName = "analysis",
                    box(h3("How did I analyze the data ?"),
                        p("The analysis includes two parts: (1) descripative analysis and (2) regression analysis"),
                        h3("Descripative analysis"),
                        selectInput(inputId = "x", 
                                    label = "Choose a predictor",
                                    choices = allpredictors,
                                    selected = allpredictors[1]),
                        plotOutput("plot1"),
                        p("From the graph, it can be seen that the Rings of abalone is effected by the predictor"),
                        h3("Regression analysis"),
                        p("Please refer to the fourth tab to see the result of regression analysis "),
                        width = 12)
                    ),
            
            # Third tab content
            tabItem(tabName = "algorithm",
                    # hr(),
                    box(h3("The linear regression algorithm"),
                        p("The linear regression algorithm is been used for the purpose of predicting the age of abalone from physical measurements."),
                        # img(src = "./linear_regression_explaination.png", ),
                        HTML("<figure><img src='./linear_regression_explaination.png' width='700' ></figure>"),
                        width = 12)
            ),
            
            
            # Fourth tab content
            tabItem(tabName = "modeling",
                    # hr(),
                    h3("Interactive regression modeling"),
                    sidebarLayout(
                        sidebarPanel(
                            selectizeInput(inputId = "predictors", 
                                           label = "Change the predictors in the regression model",
                                           choices = allpredictors,
                                           selected = allpredictors[1:2],
                                           multiple = TRUE
                                           )
                            ),
                        mainPanel(h3("The model formula is:"),
                                  verbatimTextOutput("model_formula"),
                                  h3("The model summary is:"),
                                  verbatimTextOutput("model_summary"))
                    )
            ),
            
            # Fifth tab content
            tabItem(tabName = "findings",
                    # hr(),
                    h3("What did I find in the data ?"),
                    p("Several important findings are listed here. "),
                    box(h3("The age of abalone is highly correlated with whole weight"),
                        plotOutput("plot2"), 
                        width = 6),
                    box(h3("The age of abalone is not effected by the sex(female or male)"),
                        plotOutput("plot3"), 
                        width = 6),
                    p()
            )
            # END of tab
        )
    )
)

server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        x = input$x
        if(x == "Sex"){
            boxplot(aba$Rings ~ aba[,x], xlab = x, ylab = "Rings")
        }else{
            plot(aba$Rings ~ aba[,x], xlab = x, ylab = "Rings")
        }
    })
    
    output$model_formula <- renderPrint({
        paste0("Rings ~ ",paste(input$predictors,collapse = "+"))
    })
    
    output$model_summary <- renderPrint({

        # model:
        model_formula = paste0("Rings ~ ",paste(input$predictors,collapse = "+"))
        lm_mod = lm(as.formula(model_formula), data = aba)
        summary(lm_mod)
        
    })
    
    output$plot2 <- renderPlot({
        plot(Rings ~ Whole_weight, data = aba)
    })
    
    output$plot3 <- renderPlot({
        boxplot(Rings ~ Sex, data = aba[aba$Sex != "I",])
    })
    
}

shinyApp(ui, server)
