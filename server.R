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

        if(input$update_line == TRUE) {
            xy <- create_best_fit(contracts_subset)
        } else {
            xy <- create_best_fit()
        }


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
