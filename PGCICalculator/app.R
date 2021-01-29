#
# This is a Shiny web application whose purpose is to allow a physician group to easily calculate confidence intervals for patient satisfaction surveys. This is important because these surveys are typically designed to appraise a hospital rather than an individual and, as such, the n is typically far too low to allow for non-random separation of individual physicians. This program will use a binomial distribution to calculate the chance of a "positive" response.
#

library(shiny)
library(dplyr)
library(ggplot2)
library(rhandsontable)

dummy_data <- data.frame(Doc.Name = "Name Here", Display.Graph = rep(TRUE, times = 10), n = as.integer(0), Percent.Top.Box = 100, Low.CI.Percent = 0, High.CI.Percent = 100, stringsAsFactors = FALSE)

# Define UI.
ui <- fluidPage(

    # Application title
    titlePanel("Patient Satisfaction Scientificator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("This is a helpful tool for visualizing patient satisfaction confidence intervals.",
                     br(), 
                     br(),
                     "Fill in the information in the table to the side - Doc.Name is the doctor's name, Display.Graph allows you to select which doctors you want to have displayed for making comparisons, n is the number of surveys completed, and Percent.Top.Box is the percentage (0-100%) of respondents giving the doctor a top score. You may add or remove rows by right-clicking on the row.",
                     br(),
                     br(),
                     "When you have filled in the table, your low and high 95% confidence intervals calculated using a binomial distribution will appear to the side. The button below will generate a graphical representation. The entire report may be printed.",
                     br(),
                     br(),
                     "In order to know that a doctor's score represents a real difference from another doctor, you should see minimal overlap in the colored curves. A large overlap means any observed difference might be due to randomness.",
                     br(),
                     br(),
                     tags$b("Be patient after clicking!")),
            br(),
            actionButton("gen_graph", "Generate Graph of CIs"),
            br(),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            rHandsontableOutput("doc_table"),
            br(),
            br(),
            plotOutput("pg_graph")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    previous <- reactive({dummy_data})
    
    MyChanges <- reactive({
        if(is.null(input$doc_table)){return(previous())}
        else if(!identical(previous(),input$doc_table)){
            # Converts table to df
            mytable <- as.data.frame(hot_to_r(input$doc_table))
            mytable[,'Low.CI.Percent'] <- qbinom(0.025, size = mytable[,'n'], prob = (mytable[,'Percent.Top.Box']/100))*100/mytable[,'n']
            mytable[,'High.CI.Percent'] <- qbinom(0.975, size = mytable[,'n'], prob = (mytable[,'Percent.Top.Box']/100))*100/mytable[,'n']
            mytable
        }
    })

    output$doc_table <- renderRHandsontable({
        rhandsontable(MyChanges()) %>%
            hot_context_menu(allowColEdit = FALSE) %>%
            hot_col("Low.CI.Percent", readOnly = TRUE) %>%
            hot_col("High.CI.Percent", readOnly = TRUE)
    })
    
    graph_calcs <- eventReactive(input$gen_graph, {
        test_table <- MyChanges()
        test_table <- filter(test_table, Display.Graph)
        sampling_frame <- data.frame('Percent.TB' = c(), 'Doc' = c())
        for (i in 1:nrow(test_table)) {
            avg_vector <- vector()
            doc_vector <- rep(test_table[i,'Doc.Name'], times = 10000)
            for (j in 1:10000) {
                avg_vector <- c(avg_vector, mean(sample(c(TRUE, FALSE), size = test_table[i, 'n'], replace = TRUE, prob = c(test_table[i, 'Percent.Top.Box']/100, 1-(test_table[i, 'Percent.Top.Box']/100)))))
            }
            temp_frame <- data.frame('Percent.TB' = avg_vector, 'Doc' = doc_vector)
            sampling_frame <- bind_rows(sampling_frame, temp_frame)
        }
        sampling_frame
        })
    
    output$pg_graph <- renderPlot({ggplot(graph_calcs(), aes(x = Percent.TB, fill = Doc, color = Doc)) + geom_density(alpha = 0.25, position = "identity", adjust = 3) + theme(axis.text.y = element_blank(), panel.background = element_blank(), axis.ticks.y = element_blank(), legend.title = element_blank()) + ylab("") + xlab("Percent Top Box Responses") + ggtitle("Sampling Distribution of Top Box Scores by Doctor")})
}

# Run the application 
shinyApp(ui = ui, server = server)
