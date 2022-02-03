library(dplyr)
library(shiny)
library(gganimate)
library(transformr)
library(tweenr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Posterior Beta Distribution"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("prior",
                        label = "Select beta prior:",
                        choices = list("Jeffrey's prior" = 1,
                                       "User-defined" = 2),
                        selected = 1
            ),
                conditionalPanel(
                        condition = "input.prior == 2",
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
                        )
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
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           imageOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    x <- seq(0, 1, by=.01)
    plot_data <- list()
    
    observe({
        prior_alpha <- if_else(input$prior == 1, .5, input$user_alpha)
        prior_beta <- if_else(input$prior == 1, .5, input$user_beta)
        posterior_alpha <- prior_alpha + input$successes
        posterior_beta <- prior_beta + input$total - input$successes
        
        alphas <- seq(prior_alpha, posterior_alpha, by = abs((posterior_alpha - prior_alpha)/99))
        betas <- seq(prior_beta, posterior_beta, by = abs((posterior_beta - prior_beta)/99))
       
        for (frame_number in 1:100) {
            tmp_plot_data <- data.frame(
                x = x,
                frame = frame_number,
                densities = c(
                    dbeta(x, 
                          shape1 = alphas[frame_number],
                          shape2 = betas[frame_number]),
                )
            )
            plot_data[[i]] <- tmp_plot_data
        }
        plot_data <- bind_rows(plot_data)
    
        output$distPlot <- renderImage({
            outfile <- tempfile(fileext = ".gif")
            p <- ggplot(plot_data) +
                geom_line(mapping = aes(x = x,
                                        y = densities)) +
                transition_states(frames, state_length = 1)
            anim_save("outfile.gif", animate(p))
            list(
                src = "outfile.gif",
                contentType = "image/gif"
            )
        }, deleteFile = TRUE)
    }) # end of observe() environment
}

# Run the application 
shinyApp(ui = ui, server = server)
