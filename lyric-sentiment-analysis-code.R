# ===============================================
# Fill in the following fields
# ===============================================
# Title: Project 2: Retirement Withdrawal Simulator
# Description: Shiny app to run simulations, and to display the output of what
#       could happen when using a certain withdrawal rate, under a given set of
#       rates of return and inflation.
# Author: Nathan Harounian
# Date: 5 November 2021
# ===============================================
# Required packages
# ===============================================
library(tidyverse)
library(shiny)
library(ggtext)



# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Retirement Balance Portfolio"),
  fluidRow(
    # Inputs for initial portfolio, retirement age, and withdrawal rate
    column(3,
           h4(),
           # Initial Portfolio Amount
           numericInput(inputId = "initial",
                        label = "Initial Portfolio Amount",
                        value = 1000000,
                        min = 1
           ),
           # Retirement Age
           sliderInput(inputId = 'retirement_age',
                       label = "Retirement Age",
                       value = 60,
                       min = 20,
                       max = 100,
                       step = 1
           ),
           # Withdrawal Rate
           sliderInput(inputId = 'withdrawal_rate',
                       label = "Withdrawal Rate",
                       value = 0.04,
                       min = 0,
                       max = 1,
                       step = 0.01
           ),
    ),
    
    # Inputs for mean and standard deviation of annual return rates
    column(3,
           h4(),
           # Average Annual Return
           numericInput(inputId = 'mean_r',
                        label = "Average Annual Return",
                        min = 0,
                        max = 1,
                        value = 0.10
           ),
           # Average Return Volatility
           numericInput(inputId = 'sd_r',
                        label = "Average Return Volatility",
                        min = 0,
                        max = 1,
                        value = 0.18
           )
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(3,
           h4(),
           # Average Inflation Rate
           numericInput(inputId = 'mean_i',
                        label = "Average Annual Inflation",
                        min = 0,
                        max = 1,
                        value = 0.03
           ),
           # Average Inflation Volatility
           numericInput(inputId = 'sd_i',
                        label = "Average Inflation Volatility",
                        min = 0,
                        max = 1,
                        value = 0.035
           )
    ),
    
    # Inputs for number of simulations, and random seed
    column(3,
           h4(),
           # Number of Simulations
           numericInput(inputId = 'simulations',
                        label = "Number of Simulations",
                        value = 50,
                        min = 1,
           ),
           # Value of the Random Seed
           numericInput(inputId = 'random.seed',
                        label = 'Value of the Random Seed',
                        value = 12345
           )
    )
  ),
  
  hr(),
  h4('Timelines'),
  plotOutput('plot'),
  
  hr(),
  h4('Times You Ran Out of $ in Simulations'),
  verbatimTextOutput('table'),
  
  hr(),
  h4('Summary Statistics By Year (in Millions)'),
  dataTableOutput('table1')
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {

  # creating and appending matrix
  dat <- reactive({
    set.seed(input$random.seed)
    
    years = 100-input$retirement_age
    sims = input$simulations
    
    # matrix element for random return rate from normal
    return_rate = matrix(0, years, sims)
    return_rate[] = rnorm(years * sims, mean = input$mean_r, sd = input$sd_r)
    
    # matrix element for random inflation rate from normal
    inflation_rate = matrix(0, years, sims)
    inflation_rate[] = rnorm(years * sims, mean = input$mean_i, sd = input$sd_i)
    
    # simulate balance
    balance = matrix(input$initial, years + 1, sims)
    for (i in 1:years) {
      balance[i+1,] = balance[i,]*(1 + return_rate[i,]) - 
        (input$initial*input$withdrawal_rate) * (1 + inflation_rate[i,])
    }
    
    
    
    #convert balance to millions
    balance = balance / 1000000 
    
    balance.df = as.data.frame(balance)
    balance.df = mutate(balance.df, years = c(0:(nrow(balance.df)-1)))
    return(balance.df)
  })
  
  
  # code for graph
  # (e.g. reactive data frame used for graphing purposes)
  output$plot <- renderPlot({
    balance.df = dat()
    # subsetting columns (sims) for 10th, 50th (median), and 90th percentiles
    # and the mean
    tenth = apply(balance.df, 1, quantile, probs = c(0.10))
    fifthieeth = apply(balance.df, 1, quantile, probs = c(0.50))
    ninthieeth = apply(balance.df, 1, quantile, probs = c(0.90))
    meandata = apply(balance.df, 1, mean)
    
    balance.dff = pivot_longer(
      balance.df,
      cols = starts_with('V'),
      names_to = c('simulation'),
      values_to = 'amount')
    
    balance.dff
    
    ggplot(data.frame()) + 
      geom_line(data = balance.dff, aes(x = years, y = amount, color = amount > 0, 
                                        linetype = simulation)) +
      labs(x = 'Years til reaching age 100', y = "Portfolio balance (millions)", title = 'Projected Portofolio Balances After Retirement', 
           caption = 'Subject to change according to modifications made above',
           subtitle = 
             "Green Lines — Sims w/ + Balance                  Red Lines — Sims w/ - Balance") + theme_minimal() +
      geom_segment(aes(x = -3, xend = 100-input$retirement_age+1, y = 0, yend = 0), color = 'red') +
      scale_linetype_manual(values = c(rep('solid', input$simulations))) +
      scale_colour_manual(name = 'x > 0', values = setNames(c('darkseagreen', 'lightcoral'), c(T,F))) +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 20),
            plot.subtitle = element_text(face = 'bold', hjust = 0.5),
            plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
            axis.title.x = element_text(face = 'bold', hjust = 0.5),
            axis.title.y = element_text(face = 'bold', hjust = 0.5),
            legend.position = 'none'
      )+ 
      geom_line(aes(x = 0:(100-input$retirement_age), y = tenth), color = 'black', lwd = 1.15) +
      geom_line(aes(x = 0:(100-input$retirement_age), y = fifthieeth), color = 'purple4', lwd = 1.15) +
      geom_line(aes(x = 0:(100-input$retirement_age), y = ninthieeth), color = 'black', lwd = 1.15) +
      geom_line(aes(x = 0:(100-input$retirement_age), y = meandata), color = 'tan4', lwd = 1.15) +
      geom_text(aes(label = '10th Percentile', x = 100-input$retirement_age+1.85, y = tenth[100-input$retirement_age+1], fontface = 2)) +
      geom_text(aes(label = 'Median', x = 100-input$retirement_age+1, y = fifthieeth[100-input$retirement_age+1], fontface = 2)) +
      geom_text(aes(label = 'Mean', x = 100-input$retirement_age+0.80, y = meandata[100-input$retirement_age+1], fontface = 2)) +
      geom_text(aes(label = '90th Percentile', x = 100-input$retirement_age+1.9, y = ninthieeth[100-input$retirement_age+1], fontface = 2))

  })
  
  
  # code for statistics
  output$table <- renderPrint({
    # number of simulations which resulted in negative balances
    sum(dat()[nrow(dat()),] < 0)
  })
  output$table1 <- renderDataTable({
    pivot_longer(
      dat(),
      cols = starts_with('V'),
      names_to = c('simulation'),
      values_to = 'amount') %>% group_by(years) %>% 
      summarise(minimum = min(amount),
                percentile_10th = quantile(amount, probs = c(0.1)),
                percentile_25th = quantile(amount, probs = c(0.25)),
                median = quantile(amount, probs = c(0.5)),
                average = mean(amount),
                std_deviation = sd(amount),
                percentile_75th = quantile(amount, probs = c(0.75)),
                percentile_90th = quantile(amount, probs = c(0.9)),
                maximum = max(amount))
    
  })

}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

