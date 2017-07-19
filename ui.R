library(shiny)
library(shinydashboard)


shinyUI(dashboardPage(
  skin = "red",
  dashboardHeader(title = "Foodborne Disease"),
  
  
  dashboardSidebar(
    sidebarUserPanel("NYC DSA",
                     image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("bar-chart")),
      menuItem("By year and month", tabName = "byyearandmonth", icon = icon("calendar")),
      menuItem("By state", tabName = "bystate", icon = icon("map")),
      menuItem("By other", tabName = "other", icon = icon("map"))
    )
  ),
  
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "overview",
              fluidRow(box(width = 8,
                           h1("Foodborne Disease", align = "center"),
                           p('My first paragraph, with some ',
                             strong('bold'),
                             ' text.')
                           ),
                       box(
                         img(src="https://blogs.scientificamerican.com/food-matters/files/2014/07/iStock_000015861098Small.jpg",
                             width="100%",
                             align = "middle"),
                         width = 4
                       ))
              ),
      
      
      tabItem(tabName = "byyearandmonth",
              fluidRow(valueBoxOutput("illnesses_count"),
                       valueBoxOutput("hospitalization_count"),
                       valueBoxOutput("fatalities_count")
                       ),
              fluidRow(box(htmlOutput("allbyyear")),
                       box(htmlOutput("allbymonth"))),
              fluidRow(
                radioButtons("count_by1", label = "Count by",
                             choices = list("Number of cases" = "ncases",
                                            "Number of people affected" = "npeople"),
                             selected = "ncases",
                             inline = T),
                sliderInput(inputId = "illnesses",
                            label = "Minimun number of illnesses",
                            min = 0, max = 100, value = 2,
                            step = 10))
              ),
      
      tabItem(tabName = "bystate",
              fluidRow(box(htmlOutput("map"), width = 6),
                       box(htmlOutput("rankingbystate"), width = 6)),
              fluidRow(box(htmlOutput("statebyyear"), width = 6),
                       box(selectizeInput("statename",
                                          "Select a state",
                                          state_name),
                           radioButtons("display_by", label = "Color map by",
                                        choices = list("Number of cases" = "ncases",
                                                       "Chances" = "chance"),
                                        selected = "ncases",
                                        inline = T),
                           width = 6)
                       )
              ),
      
      tabItem(tabName = "other",
              fluidRow(box(htmlOutput("location"), width = 6))
              )
    )
  )
))


