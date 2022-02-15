library(dplyr)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Posterior Beta Distribution"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(
                inputId = "user_alpha",
                label = "Input alpha parameter",
                min = 0,
                value = .5,
                step = 1
            ),
            numericInput(
                inputId = "user_beta",
                label = "Input beta parameter",
                min = 0,
                value = .5,
                step = 1
            ),
            numericInput(inputId = "successes",
                        label = "Number of successes:",
                        min = 0,
                        value = 25,
                        step = 1
            ),
            numericInput(inputId = "total",
                        label = "Number of total trials:",
                        min = 0,
                        value = 50,
                        step = 1
            ),
            numericInput(inputId = "requirement",
                        label = "Enter product requirement:",
                        min = 0,
                        max = 1,
                        value = .99,
                        step = .01
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("p_greater"),
            textOutput("p_means"),
            plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    x <- seq(0, 1, by=.001)
    
    observe({
        posterior_alpha <- input$user_alpha + input$successes
        posterior_beta <- input$user_beta + input$total - input$successes
        
        plot_data <- data.frame(
            x = x,
            densities = dbeta(x, 
                              shape1 = posterior_alpha,
                              shape2 = posterior_beta)
        )
        
        output$p_greater <- renderText(
            paste0("Posterior probability of greater than or equal to requirement: ",
                   round(
                       pbeta(input$requirement, 
                             shape1 = posterior_alpha,
                             shape2 = posterior_beta,
                             lower.tail = FALSE)*100, 5), "%")
        )
        
        output$p_means <- renderText(
            paste0("Observed estimate: ", round(input$successes/input$total*100, 5), "%", "\n",
                   "Posterior Mean: ", round(posterior_alpha/(posterior_alpha + posterior_beta)*100, 5), "%")
        )
    
        output$distPlot <- renderPlotly({
            validate(
                need(input$successes <= input$total,
                     "Number of successes needs to be less than or equal to total.")
            )
            p <- ggplot(plot_data) +
                labs(x = "probability",
                     y = "pdf",
                     title = paste0("Posterior Beta \u03b1 = ", posterior_alpha, " \u03b2 = ", posterior_beta)) +
                geom_line(mapping = aes(x = x, y = densities),
                          color = "black",
                          alpha = .7) +
                geom_ribbon(mapping = aes(x = x, ymax = densities),
                            ymin = 0,
                            fill = "dodgerblue1",
                            alpha = .4) +
                geom_vline(xintercept = input$requirement,
                           linetype = "dotted",
                           color = "black",
                           size = .1) 
            ggplotly(p)
        })
    }) # end of observe() environment
}

# Run the application 
shinyApp(ui = ui, server = server)
