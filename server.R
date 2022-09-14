#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidymodels)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$distPlot <- renderPlotly({

        contracts_subset <- contracts %>%
            filter(salary < input$salary) %>%
            filter(Fantasy.Team %in% input$team_choice)
        y <- contracts$Fantasy.Points
        X <- contracts$salary

        lm_model <- linear_reg() %>%
            set_engine('lm') %>%
            set_mode('regression') %>%
            fit(Fantasy.Points ~ salary, data = contracts)

        x_range <- seq(min(X), max(X), length.out = 100)
        x_range <- matrix(x_range, nrow=100, ncol=1)
        xdf <- data.frame(x_range)
        colnames(xdf) <- c('salary')

        ydf <- lm_model %>% predict(xdf)
        colnames(ydf) <- c('Fantasy.Points')
        xy <- data.frame(xdf, ydf)

        fig <- contracts_subset %>%
            plot_ly(
                type = 'scatter',
                mode = 'markers',
                x = ~salary,
                y = ~Fantasy.Points,
                color = ~get(input$stratification),
                customdata = ~Fantasy.Team,
                text =  ~paste(First.Name, Last.Name),
                hovertemplate = paste(
                    "<b>%{text}</b><br><br>",
                    "Points: %{y:,.1f}<br>",
                    "Salary: %{x:$.1f}<br>",
                    "Team: %{customdata}",
                    "<extra></extra>"
                )
            ) %>%
            layout(showlegend = FALSE) %>%
            add_trace(data = xy, x = ~salary, y = ~Fantasy.Points, name = 'Regression Fit', mode = 'lines', alpha = 1, inherit = FALSE)
        fig
    })

})
