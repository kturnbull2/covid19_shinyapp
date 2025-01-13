#################################### load libraries ######################################
library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(maps)

################################### load data #############################################
## Read in Johns Hopkins COVID-19 data
# Source: 
# https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv
covid_data_df = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
               show_col_types = FALSE)

# Read in Johns Hopkins population data
# Source: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv
population_data_df = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv", 
                            show_col_types = FALSE)

# map data
world_map_df = map_data("world")

################################### wrangle data ####################################
# make covid data long
long_covid_data_df <- covid_data_df %>% 
  gather(Date, Cases, `1/22/20`:`3/9/23`) 

# make date date type
long_covid_data_df$Date <- mdy(long_covid_data_df$Date) 

# make dataframe just country, date, and cases
long_covid_data_summary_df <- long_covid_data_df %>% 
  select(c(`Country/Region`, Date, Cases)) %>% 
  group_by(`Country/Region`, Date) %>% 
  summarize(Cases = sum(Cases, na.rm=TRUE)) %>% 
  ungroup() 

# add new cases and new cases weekly rolling avg columns
covid_avg_df <- long_covid_data_summary_df %>% 
  group_by(`Country/Region`) %>% 
  mutate(new_cases = Cases - lag(Cases)) %>% 
  mutate(newcases_7dayavg = rollmean(new_cases, k=7, fill=NA)) %>% 
  ungroup()  

# load population data
population_data_filtered_df <- population_data_df %>% 
  filter(is.na(Province_State) == TRUE) 

# rename covid 19 data columns 
colnames(covid_avg_df) <- c("Combined_Key", "Date", "Cases", "New_Cases", "Wk_Rolling_Average") 

# join covid 19 data and population data
covid_pop_combined_df <- left_join(covid_avg_df, population_data_filtered_df, by="Combined_Key") 

# make population/million column
covid_pop_combined_df <- covid_pop_combined_df %>% 
  mutate(pop_in_mills = Population/1000000)  

# make new cases/(population/million) column
covid_pop_combined_df <- covid_pop_combined_df %>% 
  mutate(new_cases_per_mill = New_Cases/pop_in_mills)

# make rolling average of new cases/(population/million) column
covid_pop_combined_df <- covid_pop_combined_df %>% 
  group_by(Country_Region) %>% 
  mutate(newcases_7dayavg_permill = rollmean(new_cases_per_mill, k=7, fill=NA)) %>% 
  ungroup() 

# filter to just before 10-31-2020
covid_pop_combined_df <- covid_pop_combined_df %>%
  filter(Date <= as.Date("2020-10-31"))

# get limits for outcomes of interest (Wk_Rolling_Average, newcases_7dayavg_permill)
# make round_any function for rounding beyond decimals
round_any = function(x, accuracy, f){f(x/ accuracy) * accuracy}
min_wk_rolling_avg <- round_any(min(covid_pop_combined_df$Wk_Rolling_Average, na.rm = TRUE), 100, f=floor)
max_wk_rolling_avg <- round_any(max(covid_pop_combined_df$Wk_Rolling_Average, na.rm = TRUE), 1000, f=ceiling)
min_newcases_7dayavg_permill <- round_any(min(covid_pop_combined_df$newcases_7dayavg_permill, na.rm = TRUE), 100, f=floor)
max_newcases_7dayavg_permill <- round_any(max(covid_pop_combined_df$newcases_7dayavg_permill, na.rm = TRUE), 100, f=ceiling)

# make a list to recode country names to match map data
list <- c("Antigua and Barbuda" = "Antigua",
          "Burma" = "Myanmar",
          "Cabo Verde" = "Cape Verde",
          "Congo (Kinshasa)" = "Democratic Republic of the Congo",
          "Congo (Brazzaville)" = "Republic of Congo",
          "Cote d'Ivoire" = "Ivory Coast",
          "Czechia" = "Czech Republic",
          "Eswatini" = "Swaziland",
          "Holy See" = "Vatican",
          "Korea, South" = "South Korea",
          "North Macedonia" = "Macedonia",
          "Saint Kitts and Nevis" = "Saint Kitts",
          "Saint Vincent and the Grenadines" = "Saint Vincent",
          "Taiwan*" = "Taiwan",
          "Trinidad and Tobago" = "Trinidad",
          "United Kingdom" = "UK",
          "US" = "USA") 


############################################## Define UI ##################################
ui <- fluidPage(
  # app title
  titlePanel("Global COVID-19 Cases"), 
  # first row
  fluidRow(
    # first column: radio button/selectize/date selection
    column(width=4, 
           # radio button for new cases rolling avg vs. new cases/pop/million rolling avg
           radioButtons(
             inputId = "variable",
             label = "Choose a variable:",
             choiceValues = c("Wk_Rolling_Average", "newcases_7dayavg_permill"),
             choiceNames = c("New Cases (7-Day Average)", "New Cases per Million (7-Day Average)")
           ), 
           # add selectize input to choose up to 6 countries to look at
           selectizeInput( 
             inputId = "countries",
             label = "Choose up to 6 Countries:",
             choices = covid_pop_combined_df$Country_Region,
             select = c("China", "Colombia", "Germany", "Nigeria", "US"),
             multiple = TRUE,
             options = list(maxItems = 6)
           ), 
           # choose a date to focus on
           dateInput( 
             inputId = "date",
             label = "Choose a Date:",
             min = "2020-01-22",
             max = "2020-10-31",
             value = "2020-07-23"
           ) 
    ), 
    # second column: line plot
    column(width=8, 
           plotOutput("plot") 
    ) 
  ), 
  # second row
  fluidRow(
    # first column: data table 
    column(width=4,
           dataTableOutput("table") 
    ), 
    # second column: map
    column(width=8,
           plotOutput("map") 
    ) 
  ) 
)


#################################### Define server ####################################
server <- function(input, output) {
  
  # reactive df, just countries from selectize
  plot_df <- reactive(covid_pop_combined_df %>% 
                        filter(Country_Region %in% input$countries))
  
  # reactive expression for y-axis label based on radio button choice
  yaxis <- reactive(
    if(input$variable=="Wk_Rolling_Average"){
      paste0("New Cases (7-Day Average)")
    }
    else if(input$variable=="newcases_7dayavg_permill"){
      paste0("New Cases per Million (7-Day Average)")
    }
  )
  
  # line plot
  ### y-axis changes based on variable selected in radio button
  ### vertical line changes based on date selected in date input
  ### countries change based on countries selected in selectize
  output$plot = renderPlot({
    ggplot(plot_df(), 
           aes_string(x="Date", y=input$variable, color="Country_Region")) + 
      geom_line() +
      geom_vline(xintercept = input$date) +
      ylab(yaxis()) +
      ggtitle("New COVID-19 Cases over Time")
  }) 
  
  # data table output just includes selected countries on selected date
  table_df <- reactive(plot_df() %>% 
                         filter(Date==input$date) %>% 
                         select(c(Country_Region, New_Cases, new_cases_per_mill)) %>%
                         rename("Country" = "Country_Region", "New Cases" = "New_Cases", "New Cases per Million" = "new_cases_per_mill"))
  output$table = renderDataTable(table_df())
  
  # combined data frame on chosen data, recoded country names, joined to map data                                
  covid_pop_filtered_df <- reactive(covid_pop_combined_df %>% filter(Date==input$date) %>% 
                    mutate(region=recode(Combined_Key, !!!list)))
  covid_pop_filtered_map_df <- reactive(left_join(covid_pop_filtered_df(), world_map_df, by="region"))
  
  # update the title of the map based on radio button choice
  unit <- reactive(
    if(input$variable=="Wk_Rolling_Average"){
      paste0("(Total, 7-Day Average)")
    }
    else if(input$variable=="newcases_7dayavg_permill"){
      paste0("(Per Million, 7-Day Average)")
    }
  )
  
  # map title, reactive based on chosen variable and date
  title <- reactive(paste0("New Global COVID-19 Cases on ", input$date, " ", unit()))
  
  # limits of shading on map, reactive based on variable chosen
  limits <- reactive(
    if(input$variable=="Wk_Rolling_Average"){
      c(min_wk_rolling_avg, max_wk_rolling_avg)
    }
    else if(input$variable=="newcases_7dayavg_permill"){
      c(min_newcases_7dayavg_permill, max_newcases_7dayavg_permill)
    }
  )
  
  # map output
  # reactive to date chosen
  # reactive to variable chosen
  # reactive title based on date and variable chosen
  # reactive limits based on variable chosen
  output$map = renderPlot({ 
    ggplot(covid_pop_filtered_map_df(), aes(x = long, y = lat, group = group)) +
      geom_polygon(aes_string(fill=input$variable), color = "white") + 
      theme(panel.grid.major = element_blank(), 
            panel.background = element_blank(),
            axis.title = element_blank(), 
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      ggtitle(title()) + 
      scale_fill_viridis_c(name=yaxis(), limits=limits())
  })
  
}


####################################### Run the application ##############################
shinyApp(ui = ui, server = server)