library(shiny)
library(shinydashboard)


shinyUI(dashboardPage(
  skin = "red",
  dashboardHeader(title = "Foodborne Disease"),
  
  
  dashboardSidebar(
    sidebarUserPanel("Yadi Li",
                     image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
    sidebarMenu(
      menuItem("Background", tabName = "background", icon = icon("bar-chart")),
      menuItem("By year and month", tabName = "byyearandmonth", icon = icon("calendar")),
      menuItem("By state", tabName = "bystate", icon = icon("map")),
      menuItem("Other analysis", tabName = "other", icon = icon("map"))
    )
  ),
  
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "background",
              fluidRow(box(width = 8,
                           h1("Foodborne Disease", align = "center"),
                           br(),
                           p(h2("Foodborne illness, also called food poisoning is a common public health problem. It's estimated that", 
                             strong("48 million cases of foodborne illness occur in US annually"), "- the equivalent of sickening 1 in 6 Americans each year. And each year these illnesses result in an estimated", strong("128,000 hospitalizations and 3,000 deaths."))),
                           br(),
                           p(h2("A foodborne disease outbreak occurs when two or more people get the same illness from the same contaminated food or drink. This app analyses a dataset of foodborne disease outbreaks reported to CDC from 1998 through 2015, and tries to obtain important imformation on: whether foodborne disease outbreaks are increasing or decreasing, which food causes the most illnesses, what location for food preparation poses the greatest risk of foodborne illness."))
                    
                           ),
                       column(
                         img(src="https://blogs.scientificamerican.com/food-matters/files/2014/07/iStock_000015861098Small.jpg",
                             width="100%"
                             ),
                         img(src="http://www.yourlawyer.com/i/Food_Poisoning.jpg",
                             width="100%"),
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
              fluidRow(box(htmlOutput("map"), 
                           width = 6),
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
              fluidRow(box(htmlOutput("location"), width = 6),
                       box(htmlOutput("multistate"), width = 6)),
              fluidRow(box(htmlOutput("byspecies")),
                       box(
                         h3("Some usefull links:"),
                         h4(a("https://www.fda.gov/food/foodborneillnesscontaminants/foodborneillnessesneedtoknow/default.htm",
                              href="https://www.fda.gov/food/foodborneillnesscontaminants/foodborneillnessesneedtoknow/default.htm",target="_blank")),
                         h4(a("https://www.cdc.gov/foodsafety/foodborne-germs.html",
                              href="https://www.cdc.gov/foodsafety/foodborne-germs.html", target="_blank")),
                         h4(a("http://www.foodborneillness.com/",
                              href="http://www.foodborneillness.com/", target="_blank"))
                       ))
              )
    )
  )
))


