#Created by Ryan Egan 1/27/22
## LIBS ========================================================================
library(shiny)
library(tidyverse)
library(shinydashboard)
library(reactable)
library(dplyr)
library(rsconnect)
library(ggplot2)
library(plotly)

## SOURCE ======================================================================
## If you have any functions not in global.R or server.R, source it here.

## REMOTE DATA =================================================================
players <- read.csv('players.csv')

contracts <- read.csv('capOverview.csv') %>%
  mutate(salary = case_when(Contract.Length == 2 & Remaining.Years == 2 ~ Value * 0.49
                            ,Contract.Length == 3 & Remaining.Years == 3 ~ Value * 0.3133
                            ,Contract.Length == 3 & Remaining.Years == 2 ~ Value * 0.3333
                            ,Contract.Length == 4 & Remaining.Years == 4 ~ Value * 0.22
                            ,Contract.Length == 4 & Remaining.Years == 3 ~ Value * 24
                            ,Contract.Length == 4 & Remaining.Years == 2 ~ Value * 26
                            ,Remaining.Years == 1 ~ as.numeric(Remaining.Value))) %>%
  mutate(salary = salary / 1000000)


## VARS ========================================================================
## Primary reference table for the application filters. This needs to be a table
## so we can keep leverage the relationships between columns in server.R to
## update filter options from user input.

table_decorator <-
  JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#5e3294', 'color': '#FFFFFF', 'text-align': 'center'});",
    "}"
  )
