library(shiny)
library(ggplot2)
library(dplyr)
library(googleVis)

shinyServer(function(input, output) {
  

  cal_ill <- function(choice) {
    if (choice == "ncases") {
      n = nrow(outbreaks %>% filter(Illnesses>0))
      text = "Number of cases (Illnesses)"
    } else {
      n = sum(outbreaks$Illnesses, na.rm = T)
      text = "Number of people affected (Illnesses) "
    }
    return(list(value = n, title = text))
  }
  
  cal_hos <- function(choice) {
    if (choice == "ncases") {
      n = nrow(outbreaks %>% filter(Hospitalizations>0))
      text = "Number of cases (Hospitalizations)"
    } else {
      n = sum(outbreaks$Hospitalizations, na.rm = T)
      text = "Number of people affected (Hospitalizations) "
    }
    return(list(value = n, title = text))
  }
  
  cal_fat <- function(choice) {
    if (choice == "ncases") {
      n = nrow(outbreaks %>% filter(Fatalities>0))
      text = "Number of cases (Fatalities)"
    } else {
      n = sum(outbreaks$Fatalities, na.rm = T)
      text = "Number of people affected (Fatalities) "
    }
    return(list(value = n, title = text))
  }
  
  n_by_month <- function(limit, caseorppl) {
    a = outbreaks %>% 
      filter(Illnesses > limit) 
    if (caseorppl == "ncases") {
      a = a %>% group_by(Month) %>% summarise(count = n())
    } else {
      a = a %>% group_by(Month) %>% summarise(count = sum(Illnesses))
    }
    a = a[order(match(a$Month, month.name)),]
    return(a)
  }
  
  n_by_year <- function(limit, caseorppl) {
    b = outbreaks %>% 
      filter(Illnesses > limit)
    if (caseorppl == "ncases") {
      b = b %>% group_by(Year) %>% summarise(count = n())
    } else {
      b = b %>% group_by(Year) %>% summarise(count = sum(Illnesses))
    }
    return(b)
    
  }
  
  n_by_state <- function(caseorchance) {
    if (caseorchance == "ncases") {
      return(outbreaks %>% 
               group_by(State) %>% 
               summarise(count = n()))
    } else {
      return(outbreaks %>% 
               group_by(State) %>% 
               summarise(total = sum(Illnesses)) %>% 
               inner_join(population_by_state) %>% 
               mutate(count = total / Population))
    }
  }
  
  rank_food_by_state <- function(n) {
    food_list_raw = outbreaks[outbreaks$State == n, 'Food']
    food_list_split1 = unname(unlist(sapply(unique(food_list_raw), 
                                            function(x) strsplit(x, split = "; "))))
    food_list_split2 = unname(unlist(sapply(unique(food_list_split1), 
                                            function(x) strsplit(x, split = ", "))))
    ranklist = sort(sapply(unique(food_list_split2), 
                           function(x) length(grep(x, food_list_raw))),
                    decreasing = T)
    ranklist["Unspecified"] = 0
    ranklist["Other"] = 0
    ranklist = sort(ranklist, decreasing = T)
    rankdf = data.frame(Food = names(ranklist), rank = ranklist)
    top10 = head(rankdf, 10)
    return(top10)
  }

  rank_food_multistate <- function() {
    food_list_raw = outbreaks[outbreaks$State == "Multistate", 'Food']
    food_list_split1 = unname(unlist(sapply(unique(food_list_raw), 
                                            function(x) strsplit(x, split = "; "))))
    food_list_split2 = unname(unlist(sapply(unique(food_list_split1), 
                                            function(x) strsplit(x, split = ", "))))
    ranklist = sort(sapply(unique(food_list_split2), 
                           function(x) length(grep(x, food_list_raw))),
                    decreasing = T)
    ranklist["Unspecified"] = 0
    ranklist["Other"] = 0
    ranklist["Ground"] = 0
    ranklist["Beef"] = 0
    ranklist = sort(ranklist, decreasing = T)
    rankdf = data.frame(Food = names(ranklist), rank = ranklist)
    top10 = head(rankdf, 10)
    return(top10)
  }
  
  rank_species <- function() {
    rankspe = sort(sapply(contaminant,
                          function(x) length(grep(x, outbreaks$Species))),
                   decreasing = T)
    rankdf = data.frame(species = names(rankspe), rank = rankspe)
    return(head(rankdf, 10))
  }
  
  ncase_by_stateyear <- reactive({
    outbreaks %>% filter(State == input$statename) %>% 
      group_by(Year) %>% summarise(count = n())
  })
  

  output$allbyyear <- renderGvis(
    gvisColumnChart(data = n_by_year(input$illnesses, input$count_by1),
                    xvar = "Year", yvar = "count",
                    options = list(title = ifelse(input$count_by1 == "ncases",
                                                  "Number of cases from 1998 to 2015",
                                                  "Number of people affected from 1998 to 2015"),
                                   hAxis = "{title:'Years', format:'####'}",
                                   vAxis = ifelse(input$count_by1 == "ncases",
                                                  "{title:'Number of cases'}",
                                                  "{title:'Number of people'}"),
                                   width = 600, height = 400))
  )
  
  output$allbymonth <- renderGvis(
    gvisColumnChart(data = n_by_month(input$illnesses, input$count_by1), 
                    xvar = "Month", yvar = "count",
                    options = list(title = ifelse(input$count_by1 == "ncases",
                                                  "Number of cases by month", 
                                                  "Number of peolple affected by month"),
                                   hAxis = "{title:'Months', format:'###'}",
                                   vAxis = ifelse(input$count_by1 == "ncases",
                                                  "{title:'Number of cases'}",
                                                  "{title:'Number of people'}"),
                                   width = 600, height = 400))
  )
  
  output$map <- renderGvis({
    gvisGeoChart(data = n_by_state(input$display_by), 
                 locationvar = "State", 
                 colorvar = "count",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width = 600, height = 450))
  })
  
  output$statebyyear <- renderGvis(
    gvisColumnChart(data = ncase_by_stateyear(), 
                    xvar = "Year", yvar = "count",
                    options = list(title = "Number of cases by year",
                                   hAxis = "{title:'Year', format:'####'}",
                                   vAxis = "{title:'Number of cases'}",
                                   width = 600)
                    )
  )
  
  output$illnesses_count <- renderValueBox({
    countill = cal_ill(input$count_by1)  
    valueBox(
      countill[[1]],
      subtitle = tags$p(countill[[2]], style = "font-size: 150%;")
    )
  })
  
  output$hospitalization_count <- renderValueBox({
    counthosp = cal_hos(input$count_by1)
      valueBox(
      counthosp[[1]],
      subtitle = tags$p(counthosp[[2]], style = "font-size: 150%;"),
      color = "yellow"
    )
  })
  
  output$fatalities_count <- renderValueBox({
    countfat = cal_fat(input$count_by1)
    valueBox(
      countfat[[1]],
      subtitle = tags$p(countfat[[2]], style = "font-size: 150%;"),
      color = "red"
    )
  })  
  
  output$rankingbystate <- renderGvis(
    gvisBarChart(rank_food_by_state(input$statename),
                 xvar = "Food",
                 yvar = "rank",
                 options = list(title = "Top 10 foods that case most illnesses in selected state",
                                width = 600, height = 450)
                 )
  )
  
  loca_count = sort(apply(outbreaks_loc[, 9:25], 2, sum), 
                    decreasing = T)
  loca_count = as.data.frame(loca_count)
  loca_count$location = rownames(loca_count)
  loca_count = loca_count[, c('location', 'loca_count')]
  
  output$location <- renderGvis(
    gvisPieChart(data = loca_count,
                 labelvar = "location",
                 numvar = "loca_count",
                 options = list(title = "Illnesses by location",
                                is3D = T,
                                height = 300)
      
    )
  )

  output$multistate <- renderGvis(
    gvisBarChart(rank_food_multistate(),
                 xvar = "Food",
                 yvar = "rank",
                 options = list(height = 300,
                                title = "Top 10 foods that case most illnesses in multistate outbreaks")
    )
  )
  
  output$byspecies <- renderGvis(
    gvisBarChart(rank_species(),
                 xvar = "species",
                 yvar = "rank",
                 options = list(height = 300,
                                title = "Top 10 species that case most illnesses")
    )
  )
  
})
