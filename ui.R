library(shiny)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("sandstone"),

    ##########
    ## PAGE 1
    ##########
    # Application title
    navbarPage("League of Nations",
        tabPanel(
            "",
            titlePanel("Salary Prices And Scoring"),

            fluidRow(column(12,
                plotlyOutput("distPlot")
            )),
            hr(),

            # Sidebar with a slider input for number of bins
            fluidRow(
                column(6, align='center',
                 sliderInput("salary",
                            "Max Salary:",
                            min = 0,
                            max = 50,
                            value = 50)
                 )
                 ,column(6, align='left',
                        radioButtons("stratification"
                              ,"Plot Markers:"
                              ,c("Fantasy Team" = "Fantasy.Team"
                                 ,"Player Position" = "Pos")
                        )
                )
                ,column(12,align='center',
                        checkboxGroupInput(
                            "team_choice", h4(paste("Display Teams:")),
                            inline = TRUE,
                            selected = contracts %>% distinct(Fantasy.Team) %>% pull(),
                            choices = contracts %>% distinct(Fantasy.Team) %>% pull()
                        ))
            )
        )
    )
))
