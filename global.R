

outbreaks <- read.csv('outbreaks.csv', stringsAsFactors = F)
population_by_state <- read.csv('population.csv', stringsAsFactors = F)
population_by_state$Population = as.numeric(gsub(",", 
                                                 "", 
                                                 population_by_state$Population, 
                                                 fixed = T)) / 1000
outbreaks_loc <- read.csv('outbreaks_location.csv', stringsAsFactors = F)

loca = c("Banquet Facility", "Camp", "Catering Service", "Child Daycare",
         "Fair/Festival", "Farm/Dairy", "Restaurant", "Grocery Store",
         "Hospital", "Hotel/Motel", "Nursing Home/Assisted Living Facility",
         "Office/Indoor Workplace", "Prison/Jail", "Private Home/Residence",
         "Religious Facility", "School/College/University", "Ship/Boat")

state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
               "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
               "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
               "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
               "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
               "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
               "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
               "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
               "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
               "West Virginia", "Wisconsin", "Wyoming") 