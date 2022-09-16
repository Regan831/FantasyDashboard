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

#Table theme
# theme <- reactableTheme(color = "hsl(0, 0%, 87%)", backgroundColor = "hsl(220, 13%, 18%)",
#                         borderColor = "hsl(0, 0%, 22%)", stripedColor = "rgba(255, 255, 255, 0.04)",
#                         highlightColor = "rgba(255, 255, 255, 0.06)", inputStyle = list(backgroundColor = "hsl(0, 0%, 24%)"),
#                         selectStyle = list(backgroundColor = "hsl(0, 0%, 24%)"),
#                         pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 24%)"),
#                         pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 28%)"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$all_plot <- renderPlotly({
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

    output$team_plot <- renderPlotly({
        contracts_subset <- contracts %>%
            filter(Fantasy.Team == input$team_select)

        xy <- create_best_fit(contracts_subset)
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

    output$team_table <- renderReactable(
        contracts %>%
            filter(Fantasy.Team == input$team_select) %>%
            mutate(Name = paste(Last.Name, First.Name, sep=', ')) %>%
            select(Name, Pro.Team, salary, Fantasy.Points) %>%
            reactable(filterable = TRUE, searchable = TRUE, resizable = TRUE,
                                                        paginationType = "jump", showPageSizeOptions = TRUE, highlight = TRUE,
                                                        showSortable = TRUE, defaultSorted = c("salary"),
                                                        columns = list(Name = colDef(name = 'Name'),
                                                                       Pro.Team = colDef(name = "Team"),
                                                                       salary = colDef(name = "Salary"),
                                                                       Fantasy.Points = colDef(name = "Points cored"))
                                                        ))

})
