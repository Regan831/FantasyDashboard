# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # includeCSS('stylesheet.css'),
    useShinydashboard(),
    theme = shinytheme("slate"),

    ##########
    ## PAGE 1
    ##########
    # Application title
    navbarPage("League of Nations",
        tabPanel(
            "Home",
            titlePanel("Salary Prices And Scoring"),

            fluidRow(column(12,
                plotlyOutput("all_plot")
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
                 ,column(3, align='left',
                        radioButtons("stratification"
                              ,"Plot Markers:"
                              ,c("Fantasy Team" = "Fantasy.Team"
                                 ,"Player Position" = "Pos")
                        )
                )
                ,column(3, align='left',
                         radioButtons("update_line"
                                      ,"Update Best Fit Line:"
                                      ,c("Yes" = TRUE
                                         ,"No" = FALSE)
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
        ),
        ##########
        ## PAGE 2
        ##########
        # Application title
        tabPanel(
           "Manager Stats",
           titlePanel("Manager Stats"),
           fluidRow(
               column(selectInput('team_select'
                           ,'Team: '
                           ,choices = contracts %>% distinct(Fantasy.Team) %>% pull()), width=6)
               ,valueBoxOutput("team_contract_avg", width = 6)
           )
           ,hr()
           ,fluidRow(
               column(
                   width = 6,
                   reactableOutput("team_table")
               ),
                column(6, plotlyOutput("team_plot"))
           )
        ),
        ##########
        ## PAGE 3
        ##########
        # Application title
        tabPanel(
            "All Contracts"
            ,titlePanel("All Contracts")
            ,fluidRow(
                column(
                    width = 12,
                    reactableOutput("all_players_table")
                )
            )
        )
   )
))
