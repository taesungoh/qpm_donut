
library(shiny) 
library(ggplot2) 
library(tidyverse)

# Global variables can go here
n <- 5000

# Define the UI
ui <- bootstrapPage(
  sliderInput("n",
              "Trial:",
              min = 0,  max = 5000, value = 5000),
  plotOutput('plot'), 
  plotOutput('plot2')
)

server <- function(input, output) {
  flip_coin <- reactive({
    # Get the input value n 
    n <- input$n 
    
    # Vectors to store results
    times <- 1:n
    num_heads <- numeric(n)
    frequency <- numeric(n)
    
    # Setting seed
    set.seed(12345)
    
    # Simulate coin flips
    flips <- rbinom(n, 1, 0.5)  # 0 = Tails, 1 = Heads
    
    # Compute cumulative heads and frequencies
    for (i in 1:n) {
      num_heads[i] <- sum(flips[1:i])
      frequency[i] <- num_heads[i] / i
    }

    # Create a data frame
    data.frame(times, num_heads, frequency)
    })
  
  output$plot <- renderPlot({
    results <- flip_coin() 
    ggplot(results, aes(x = times, y = frequency)) + 
    geom_line(color = "darkred") +
      geom_hline(yintercept = 0.49, color = "blue", lty = "dashed", 
                 alpha = 0.2) + 
      geom_hline(yintercept = 0.5, color = "blue", lty = "dashed", 
                 alpha = 0.2) +
      geom_hline(yintercept = 0.51, color = "blue", lty = "dashed", 
                 alpha = 0.2) + 
      xlim(0, 5000) + 
      ylim(0, 1)+
      theme_light() + 
      xlab("Sample Size") +
      ylab("Proportion of Heads") + 
      labs(title = "Law of Large Numbers")
  })
  
  output$plot2 <- renderPlot({
    results2 <- flip_coin() |> 
      dplyr::filter(times == input$n) |> 
      mutate(num_tails = times - num_heads) |> 
      dplyr::select(num_tails, num_heads) |> 
      pivot_longer(
        cols = c(num_tails, num_heads),
        names_to = c("coin_side"),
        values_to = "frequency")
      ggplot(results2, aes(x = coin_side,y = frequency)) + 
      geom_bar(stat = "identity") + 
      ylim(0, 2600) +
      scale_x_discrete(labels = c("Heads", "Tails")) + 
      labs(title = "Number of Heads and Tails", 
             x = "Head or Tale", 
             y = "Number of Each Side") + 
      geom_label(aes(label = frequency)) + 
      theme_bw() 
    }) 
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)

