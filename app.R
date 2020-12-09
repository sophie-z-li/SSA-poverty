library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(ggthemes)
library(readxl)
library(ggrepel)
library(gt)
library(gtsummary)
library(broom.mixed)
library(broom)
library(leaflet)
library(maptools)
library(DT)
library(rstanarm)
theme_set(theme_bw())

# Here, I am reading in all of my data. Although I cleaned/altered some data
# in my `Gather.Rmd`, for the most part, I preferred loading my data directly
# into my `App.R`. This was because I frequently got new ideas for my Shiny App
# that required new columns/joining different data, so it ended up being more
# efficient for me to just edit data directly here, as opposed to constantly
# going back and creating new RDS/csv files.

region_poverty <- read_xlsx("raw_data/finaldata.xlsx", 1)
country_poverty <- read_xlsx("raw_data/finaldata.xlsx", 2)
country_gdp <- read_xlsx("raw_data/finaldata.xlsx", 3)

labor <- read_xlsx("raw_data/ssa_hfs.xlsx", 1)
income <- read_xlsx("raw_data/ssa_hfs.xlsx", 2)
food <- read_xlsx("raw_data/ssa_hfs.xlsx", 3)
education <- read_xlsx("raw_data/ssa_hfs.xlsx", 4)

# Some of the lines below go over the 80 character line, but my column names
# are naturally long (rip), and I kept the indentation for readability purposes.

# `indicator_data` refers to the World Development Indicators that I downloaded
# directly from the World Bank's public database. I used this for my model.

indicator_data <- read_csv("raw_data/indicator.csv", col_types = cols(
                          .default = col_double(),
                          country_code = col_character(),
                          cooling_degree_days_projected_change_in_number_of_degree_celsius = col_double(),
                          heat_index_35_projected_change_in_days = col_logical(),
                          maximum_5_day_rainfall_25_year_return_level_projected_change_in_mm = col_double(),
                          mean_drought_index_projected_change_unitless = col_double(),
                          people_using_safely_managed_sanitation_services_percent_of_population = col_double()))

# The `finaldata` I read in below was provided by my supervisor Jose Motes. I
# used this dataset primarily for my Regional and Country-level tabs.

finaldata_old <- read_xlsx("raw_data/finaldata.xlsx", 3) %>%
  mutate(pov_increase = `2020_new` - `2019_new`,
         pov_increase_BA = `2020_BA` - `2019_BA`,
         pov_increase_MPO =`2020_MPO` - `2019_MPO`)

# I read in population data and then joined it with my `finaldata` tibble in
# order to calculate the estimated new poor in Sub-Saharan Africa based on the
# projected increase in poverty.

population <- read_xls("raw_data/population.xls", 1) %>%
  clean_names() %>%
  select(country_code, x2019) %>%
  rename(population = x2019)

finaldata <- left_join(finaldata_old, population, 
                       by = c("code" = "country_code")) %>%
  mutate(new_poor = pov_increase*population)

# Here, I am loading my leaflet information. This was a wild ride LOL.

data("wrld_simpl")
world_simple_custom <- wrld_simpl

world_simple_custom@data <- left_join(world_simple_custom@data, finaldata, 
                                      by = c("ISO3" = "code")) %>%
  clean_names()


# Define UI ----
ui <- fluidPage(
  
  # I explored with a lot of themes before finally settling on "flatly."
  # For the longest of times, I actually had "slate," but unfortunately, the
  # plot renderings didn't go well with a dark theme.
    
    theme = shinytheme("flatly"),
    
    titlePanel(
        h1("COVID-19's Impact on Poverty in Sub-Saharan Africa", 
           align = "center"),
        
        # My tab title kept on showing HTML code, so I add the below line in
        # order to rename the tab to a more suitable moniker.
        
        tags$head(HTML("<title>COVID-19's Impact on Poverty in 
                       Sub-Saharan Africa</title>"))),
    
    # I used a navlistPanel because I wanted to be edgy in having my navigation
    # interface on the left side of my Shiny App, compared to literally every
    # other Shiny App I've seen, which always has the navigation up top.
    
    navlistPanel(
        "Background",
        
        # Here, I give a brief introduction on what my project is about. The
        # words are a copy-and-paste from the PDF document that I am writing
        # with the World Bank's SSA team to update poverty numbers post-Covid-19
        # outbreak.
        
        tabPanel("Introduction",
                 br(),
                 HTML('<center><img src="Africa.png" width="400"></center>'),
                 br(),
                 br(),
                 h3("How much will poverty rise in Sub-Saharan Africa in 
                    2020?"),
                 p("Since its initial outbreak, the COVID-19 pandemic has 
                 brought about a sharp reduction in global economy activity. 
                 For Sub-Saharan Africa (SSA), daily infections rates have 
                 declined since mid-July, although recently, there has been a 
                 small spike in new cases as countries continue to conduct more 
                 testing. While the officially reported infections in SSA remain 
                 relatively low compared to other regions, the data may be 
                 misleading given that testing capacity is limited in many 
                 countries, and we may still be in the intermediate stages of 
                 the pandemic in the region. Areas with weak healthcare 
                 infrastructure could easily be overwhelmed by a rapidly 
                 expanding pandemic, the effects of which will be aggravated by
                 the global economic downturn as well as government-mandated
                 lockdowns in the region."),
                 h3("The Analysis"),
                 p("This dashboard presents estimates of the increase in poverty
                 and as an update from a", a("previous World Bank note", 
             href = "https://openknowledge.worldbank.org/handle/10986/33765")," 
                 which was published in May 2020. Updated estimates based on 
                 the October 2020 World Economic Outlook vintage suggest that 
                 COVID-19 will have a more severe impact on poverty rates 
                 than originally projected. Additionally, data from 
                 high-frequency phone surveys indicate that the pandemic 
                 has created severe disturbances in the labor market, 
                 household income, food security, and educational attainment
                 within the region.")),
        
        # Here, I give a brief summary about the methodology used by the World
        # Bank in calculating poverty, as well as the three types of poverty
        # estimates that are referenced in this project.
        
        tabPanel("Methodology",
                 br(),
                 HTML('<center><img src="worldbank.jpg" width="400"></center>'),
                 h3("Calculating Poverty"),
                 p("Poverty projections were created by running a pre-existing
                 Stata do-file, which was provided by the World Bank's Poverty
                 and Equity Team. The do-file took the
                 following four inputs:"),
                 HTML("<ul><li>Poverty rate per country at the year 2018 (World 
                     Bank Data)</li><li>GDP per capita growth 2019-2021 (World 
                     Economic Outlook Data)</li><li>Welfare aggregate per 
                     country at 2018 (World Bank Data)</li><li>Poverty line at 
                     1.9 PPP (World Bank Data)</li></ul>"),
                 p("The methodology used was identical to the May 2020 World 
                 Bank note. We first defined shock to GDP as the difference 
                 between the two most recent GDP projections from the IMF’s 
                 World Economic Outlook (WEO). In this case, it was the October
                 2019 and October 2020 vintages. We then simulated the change
                 in poverty rates by adjusting the welfare of households from a
                 database of household surveys, forecasting the welfare 
                 aggregate of each country by the growth of the GDP per capita
                 assuming a “distributionally neutral” impact in welfare. 
                 The difference in lined-up poverty rates between the two 
                 vintages is an approximate estimate of the effect of the 
                 crisis on poverty. For more information, please refer to", 
                 a("the May 2020 note.", 
              href = "https://openknowledge.worldbank.org/handle/10986/33765")),
                 h3("Poverty Projections"),
                 p("This dashboard utilizes three different poverty projections.
                   The first two are POVCAL estimates, which are calculated
                   using the above methology. The baseline POVCAL estimate 
                   depicts
                   pre-COVID poverty projections based on
                   the October 2019 WEO vintage. The new POVCAL estimate depicts
                   the updated poverty projections post-outbreak. using the
                   October 2020 WEO vintage."),
                 p("This dashboard also utilizes the World Bank’s latest Macro 
                 Poverty Outlook (MPO) projections, which analyzes 
                 country-specific poverty developments. MPOs projections are 
                 based on GDP-poverty elasticities for the latest available 
                 household surveys used to measure poverty. Although MPO 
                 projections also follow a distributional neutral approach, 
                 the magnitude of the impact in welfare aggregate depends on 
                 the welfare and GDP per capita elasticity chosen by the poverty
                 economist in charge of each country."),
                 tags$i("*Unless MPO is specifically stated, mentions of poverty
                   projections in this dashboard to poverty projections will
                   be in reference to POVCAL estimates (in accordance with World
                   Bank practices.)"),
                 p(tags$i("**While I gathered the above inputs, ultimately, the
                   finalized do-file was run by my project supervisor, Jose
                   Montes, due to my not having possession of certain 
                   proprietary data."))),
        
        # Here, I finally start talking about my data, yay!
        
        "Data",
        tabPanel("Country-Level Data",
            h3("Examining Poverty by Country"),
            p("Below is an interactive map. Click on a country in the SSA region
            to explore."),
            leafletOutput("SSAmap"),
            
            # In creating my leaflet, I suffered a minor "NA" legend crisis.
            # Adding the below HTML tag helped resolve it to some extent.
            
            tags$style(type="text/css", "div.info.legend.leaflet-control br 
                       {clear: both;}"),
        br(),
        dataTableOutput("pov_change")
        ),
        
        # Starting out, I was super nervous because Shiny looked like a complex
        # behemoth, and I wasn't even sure where to start in terms of making it
        # interactive (or even making the app in general). So I do have a bit of
        # a soft spot for the "Regional Data" because it was the first time I
        # was able to make part of my Shiny App interactive.
        
        tabPanel("Regional Data",
                 mainPanel(
                   h3("Comparing Poverty Differences By Region"),
                   p("This interactive feature allows you to visualize changes
                   in poverty over time in different SSA regions. In the 
                   drop-down menu below, select the region of interest."),
                   p("The", tags$b("Eastern/Central region (AFE)"), "includes 
                   Angola, Botswana, 
                   Burundi, Comoros, the Democratic Republic of the Congo, 
                   Eswantini, Ethiopia, Kenya, Lesotho, Madagascar, Malawi, 
                   Mauritius, Mozambique, Namibia, Rwanda, São Tomé and 
                   Príncipe, Seychelles, South Africa, South Sudan, Sudan,
                   Tanzania, Uganda, Zambia, and Zimbabwe."),
                   p("The", tags$b("Western/Southern region (AFW)"), "includes 
                   Benin, Burkina Faso,
                   Cameroon, Cabo Verde, Central African Republic, Chad, Côte
                   d'Ivoire, Gabon, Ghana, Guinea, Guinea-Bissau, Liberia, Mali,
                   Mauritania, Niger, Nigeria, Republic of Congo, Senegal,
                   Sierra Leone, The Gambia, and Togo."),
                   br()),
                 
                 mainPanel(
                   selectInput("select_region",
                               "Select Region",
                               c("Sub-Saharan" = "SSA", 
                                 "Eastern/Central" = "AFE", 
                                 "Western/Southern" = "AFW")),
                   br(),
                   plotOutput("PlotsRegional", width = 700),
                   br(),
                   p("The pandemic is projected to have a slightly higher 
                   impact on poverty levels in Western/Central Africa than 
                   Eastern/Southern Africa. Poverty is expected to increase 
                   around 3 percentage points for Western/Central Africa, 
                   effectively wiping out four years of progress in reducing 
                   poverty. Poverty is also estimated to increase by almost 
                   2 percentage points for Eastern/Southern Africa, 
                   negating over three years of progress. However, overall 
                   poverty rates are still lower in AFW compared to AFE. The 
                   poverty rate in AFW is projected to be around 37 percent for
                   2020, while the poverty rate in AFE is estimated to be 
                   between 44 percent for the same year. The difference in 
                   poverty rates between the two regions is a stark contrast to 
                   10 years ago, when the two regions’ poverty rates were both 
                   around 47 percent. It appears that countries in the AFW 
                   region have overall been more effective at reducing poverty
                   compared to AFE countries, which may explain why AFW 
                   countries’ poverty rates were slightly more impacted by the 
                   pandemic."),
                   br(),
                   br())),
        
        # My supervisor was pretty impressed by the graphs I made for this, and 
        # I chuckle with glee because I made it with R and not Excel LOL.
        
        tabPanel("High-Frequency Phone Surveys",
                 mainPanel(
                 h3("COVID-19 High Frequency Monitoring"),
                 p("The following data was pulled from the World Bank's",
                 a("COVID-19 High-Frequency Monitoring", 
               href = "https://openknowledge.worldbank.org/handle/10986/33765"),
                 "beta. Preliminary data from high-frequency 
                   phone surveys indicate major disruptions to labor markets, 
                   household income, food security, and educational attainment 
                   within the SSA region."),
                 br(),
                 selectInput("select_indicator",
                             "Select Indicator",
                             c("Labor" = "labor", 
                               "Income" = "income", 
                               "Food" = "food",
                               "Education" = "education")),
                 plotOutput("PlotsIndicator"),
                 br(),
                 textOutput("PlotsIndicatorText"),
                 br())),
        
        # Below is my model. Coding my model stressed me out the most, but I'm
        # very excited about the final product. It was also here that I self-
        # coronated myself as the Style Emperor.
        
        tabPanel("Model",
                 h3("Predicting Poverty"),
                 p(
                     "The variables below were taken from the World Bank's 
                     Environment and Social Government database. Below, you
                     can construct a predictive model regressing poverty at 
                     either national poverty lines or the international
                     poverty line, using up to three covariates with/without
                     interaction terms. For further information on the 
                     variables, you may consult the", tags$b("Glossary"), "on 
                     the leftside pane, which provides definitions directly 
                     from the World Bank."
                 ),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                       selectInput("select_definition",
                                   "Glossary",
                                   c("Access to Electricity" = "electricity", 
                                     "Adult Literacy Rate" = "literacy", 
                                     "Annual GDP Growth" = "gdp",
                                     "Child Employment" = "child",
                                     "Gini Index" = "gini", 
                                     "Government Effectiveness Estimate" = 
                                       "goveff",
                                     "Life Expectancy at Birth" = "life",
                                     "Prevalence of Undernourishment" = 
                                       "nourishment",
                                     "Primary School Enrollment" = "school")),
                       
                       # I load a cute little glossary here for viewers to
                       # reference the definition of various indicators.
                       
                       textOutput("VariableDefinition"),
                       br(),
                       
                       # Here, I ask the user to select the X1 covariate.
                       
                       selectInput(
                             "varOI_x",
                             "X1 variable:",
       choices = c(
        "Access to Electricity" = "access_to_electricity_percent_of_population",
        "Adult Literacy" = 
          "literacy_rate_adult_total_percent_of_people_ages_15_and_above",
        "Annual GDP Growth" = 
          "gdp_growth_annual_percent",
        "Child Employment" = 
          "children_in_employment_total_percent_of_children_ages_7_14",
        "Gini Index" ="gini_index_world_bank_estimate",
        "Government Effectiveness" = "government_effectiveness_estimate",
        "Life Expectancy at Birth" = "life_expectancy_at_birth_total_years",
        "Prevalence of Undernourishment" = 
          "prevalence_of_undernourishment_percent_of_population",
        "Primary School Enrollment" = "school_enrollment_primary_percent_gross"
),
selected = "government_effectiveness_estimate"),

# Here, I ask the user to select the X2 covariate.

selectInput(
  "varOI_x2",
  "X2 variable:",
  choices = c(
    "Access to Electricity" = "access_to_electricity_percent_of_population",
    "Adult Literacy" = 
      "literacy_rate_adult_total_percent_of_people_ages_15_and_above",
    "Annual GDP Growth" = "gdp_growth_annual_percent",
    "Child Employment" = 
      "children_in_employment_total_percent_of_children_ages_7_14",
    "Gini Index" ="gini_index_world_bank_estimate",
    "Government Effectiveness" = "government_effectiveness_estimate",
    "Life Expectancy at Birth" = "life_expectancy_at_birth_total_years",
    "Prevalence of Undernourishment" = 
      "prevalence_of_undernourishment_percent_of_population",
    "Primary School Enrollment" = "school_enrollment_primary_percent_gross",
    "None" = "None" 
  ),
  selected = "None"),

# Here, I ask the user to select the X3 covariate.

selectInput(
  "varOI_x3",
  "X3 variable:",
  choices = c(
    "Access to Electricity" = "access_to_electricity_percent_of_population",
    "Adult Literacy" = 
      "literacy_rate_adult_total_percent_of_people_ages_15_and_above",
    "Annual GDP Growth" = "gdp_growth_annual_percent",
    "Child Employment" =
      "children_in_employment_total_percent_of_children_ages_7_14",
    "Gini Index" ="gini_index_world_bank_estimate",
    "Government Effectiveness" = "government_effectiveness_estimate",
    "Life Expectancy at Birth" = "life_expectancy_at_birth_total_years",
    "Prevalence of Undernourishment" = 
      "prevalence_of_undernourishment_percent_of_population",
    "Primary School Enrollment" = "school_enrollment_primary_percent_gross",
    "None" = "None" 
  ),
  selected = "None"),

# Here, I ask the user to select the Y1 output variable.

selectInput(
    "varOI_y",
    "Y variable:",
    choices = c(
      "Poverty Rate at National Poverty Lines" = 
      "poverty_headcount_ratio_at_national_poverty_lines_percent_of_population",
      "Poverty Rate at International Poverty Line ($1.90)" = 
      "poverty_headcount_ratio_at_1_90_a_day_2011_ppp_percent_of_population"),
    selected = 
     "poverty_headcount_ratio_at_national_poverty_lines_percent_of_population"),

# Here, I ask the user to select the model they wish to use. Side note: radio
# buttons are cute.

radioButtons("type", "Model Type:",
             c("Linear Model" = "toggleLinear",
               "Multivariate Regression" = "toggleMulti",
               "Multivariate Regression w/ X1 & X2 Interaction" = 
                 "toggleMultiinteraction",
               "Multivariate Regression w/ X1, X2 & X3 Interaction" = 
                 "toggleMultiinteraction3"),
             selected = "toggleLinear")),

mainPanel(
    
    # Here, I load a scatterplot with a line of best fit. Credit goes to Wyatt
    # for this idea. I actually borrowed it from his Gov 50 Shiny App, so want
    # to give credit where credit is due.
    
    plotOutput("poverty_regression", height = 500),
    br(),
    
    # Here, I load my regression table.
    
    gt_output(outputId = "RegSum"),
    
    # Wyatt had this in his original Shiny App. I'm afraid to take it out
    # because of app erroring.
    
    tags$style(
        type = "text/css",
        ".shiny-output-error {display: none;}",
        ".shiny-output-error:before {display: none;}")))),

# Here is where I flex about myself and my non-existent street-cred.

        "Others",
tabPanel("About",
         HTML('<center><img src="headshot.jpg", height = "30%", 
              width="30%"></center>'),
         br(),
           h3("About Me"),
           p("My name is Sophie, and I am a junior at Harvard College
           studying Philosophy. As a native Houstonian, I love all
           things barbeque and am the proud owner of two miniature
           cacti. You can find the link to my Github", 
             a("here", href ="https://github.com/sophie-z-li")),
         h3("About this Project"),
         p("For my final project, I partnered with the
           World Bank Group's Global Poverty and Equity Practice to analyze
           COVID-19's impact on poverty in the sub-Saharan Africa region."),
         p("This dashboard updates", a("previously published estimates", 
           href = "https://openknowledge.worldbank.org/handle/10986/33765"),"
           of COVID-19’s effect on poverty levels in Sub-Saharan Africa 
           (SSA), using GDP growth projections from the October 2020 
           World Economic Outlook (WEO) database. Prior
           projections were created based on April 2020 WEO numbers. In total,
           this project used data from both the WEO as well as a variety of
           World Bank datasets, including the Environment and Social Governance
           database (ESG), World Development Indicators, World Population, 
           COVID-19 High-Frequency Phone Survey results, Macro-Poverty Outlooks,
           as well as proprietary bank data."),
         br()),
        tabPanel("PDF",
                 tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                             src="SSA_note_draft.pdf"))
    )
)
# Define server logic ----
server <- function(input, output) {
    
    # Below is my interactive component for regional poverty. I have if-else
    # statements that can handle four different choices.
    
    output$PlotsRegional <- renderPlot({
        if(input$select_region == "SSA") {
        region_poverty %>%
            filter(year >= 2010) %>%
            filter(region == input$select_region) %>%
            mutate(poverty = round(poverty, 1)) %>%
            ggplot(aes(x = year, y = poverty, color = type)) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            geom_text_repel(aes(label = poverty), size = 3.5, 
                            color = "black", segment.color = "transparent") +
            labs(title = "Changes in Poverty in the Overall Sub-Saharan Region",
                 x = "Year",
                 y = "Poverty Rate (%)") + 
            ylim(32, 48) +
            scale_color_manual(values = 
                                 c("darkgreen", "darkorange2", "royalblue3"),
                               name = "Estimates",
                               breaks = c("baseline", "new", "mpo"),
                               labels = c("Pre-Pandemic", 
                                          "Post-Pandemic (POVCAL)", 
                                          "Post-Pandemic (MPO)")) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
        }
        else{
            if(input$select_region == "AFE"){
                region_poverty %>%
                    filter(year >= 2010) %>%
                    filter(region == input$select_region) %>%
                    mutate(poverty = round(poverty, 1)) %>%
                    ggplot(aes(x = year, y = poverty, color = type)) +
                    geom_line(size = 1) +
                    geom_point(size = 2) +
                    geom_text_repel(aes(label = poverty), size = 3.5, 
                          color = "black", segment.color = "transparent") +
                    labs(title = "Changes in Poverty in Eastern/Central Africa",
                         x = "Year",
                         y = "Poverty Rate (%)") + 
                    ylim(32, 48) +
                    scale_color_manual(values = 
                                   c("darkgreen", "darkorange2", "royalblue3"),
                                   name = "Estimates",
                                   breaks = c("baseline", "new", "mpo"),
                                   labels = c("Pre-Pandemic", 
                                              "Post-Pandemic (POVCAL)", 
                                              "Post-Pandemic (MPO)")) +
                    theme_bw() +
                    theme(plot.title = element_text(hjust = 0.5))    
            }
        
        else{
            if(input$select_region == "AFW") {
                region_poverty %>%
                    filter(year >= 2010) %>%
                    filter(region == input$select_region) %>%
                    mutate(poverty = round(poverty, 1)) %>%
                    ggplot(aes(x = year, y = poverty, 
                               color = type, label = poverty)) +
                    geom_line(size = 1) +
                    geom_point(size = 2) +
                    geom_text_repel(aes(label = poverty), size = 3.5, 
                          color = "black", segment.color = "transparent") +
                    labs(title = 
                           "Changes in Poverty in Western/Southern Africa",
                         x = "Year",
                         y = "Poverty Rate (%)") + 
                    ylim(32, 48) +
                    scale_color_manual(values = 
                                   c("darkgreen", "darkorange2", "royalblue3"),
                                   name = "Estimates",
                                   breaks = c("baseline", "new", "mpo"),
                                   labels = c("Pre-Pandemic", 
                                              "Post-Pandemic (POVCAL)", 
                                              "Post-Pandemic (MPO)")) +
                    theme_bw() +
                    theme(plot.title = element_text(hjust = 0.5))
            }
        }}
        
    })
    
    # Below is my data table for the country-specific tab. Coding this took a
    # long time, and I still can't figure out how to add commas to the digits
    # rip.
    
    output$pov_change <- renderDataTable(world_simple_custom@data %>%
                                     filter(iso3 %in% c("AGO", "BDI", "BEN", 
                                     "BFA", "BWA", "CAF", "CIV", 
                                     "CMR", "COD", "COM", "CPV", "ETH", "GAB", 
                                     "GHA", "GIN", "GMB", "GNB", "KEN",
                                     "LBR", "LSO", "MDG", "MLI", "MOZ", "MRT", 
                                     "MUS", "MWI", "NAM", "NER", "NGA",
                                     "RWA", "SDN", "SEN", "SLE", "SSD", "STP", 
                                     "SWZ", "SYC", "TCD", "TGO", "TZA", 
                                     "UGA", "ZAF", "ZMB", "ZWE")) %>%
                                     select(name, 
                                            population, 
                                            pov_increase, 
                                            new_poor) %>%
                                     mutate(pov_increase = 
                                              round(pov_increase, 2)) %>%
                                     mutate(new_poor = round(new_poor, 0)),
                                     colnames = c("Country", "Population", 
                                     "Poverty Increase % (POVCAL)", 
                                     "Estimated # of New Poor"),
                                     options = list(pageLength = 10))
    
    # Here is where I made my Leaflets. As you can see, they are a cool red
    # color that vary in shade based on the percentage of poverty increase.
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = world_simple_custom$pov_increase)
    
    output$SSAmap <- renderLeaflet({
      leaflet(world_simple_custom) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = .5,
                    color = ~pal(pov_increase),
                    popup = paste(
                      world_simple_custom$name,
                      "<br>",
                      "Increase in Poverty:",
                      world_simple_custom$pov_increase,
                      "<br>",
                      "Population:",
                      finaldata$population
                    )) %>%
        setView(lng = 6.6111, lat = 20.9394, zoom = 3) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = world_simple_custom$pov_increase,
          title = "% change in poverty, 2019-2020",
          opacity = 1,
          labFormat = labelFormat(digits = 0))
    })
    
    # Here are my plots and text for high-frequency phone surveys. Some of the
    # labs run over the 80 character line because they had to.
    
    output$PlotsIndicator <- renderPlot({
        if(input$select_indicator == "labor") {
            ggplot(labor, aes(y = poverty_increase, x = working_stop, 
                              color = country)) +
                geom_point() +
            
            # Let's be real, using `geom_text_repel` over vanilla `geom_text`
            # is the ultimate flex.
            
                geom_text_repel(aes(label = country, vjust = 2), 
                          size = 3.5, color = "black", 
                          segment.color = "transparent") +
                labs(x = "% of Responders Who Reported Having Stopped Working Since the COVID-19 Outbreak",
                     y = "Increase in Poverty (%)") +
                scale_color_discrete("Country") +
                xlim(0, 100) +
                ylim(0, 5) +
                theme_bw() +
                theme(legend.position = "none")
        }
        else{
            if(input$select_indicator == "income"){
                ggplot(income, aes(y = poverty_increase, 
                                   x = decrease_total_income, 
                                   color = Country)) +
                    geom_point() +
                    geom_text(aes(label = Country, vjust = 2), 
                              size = 3.5, color = "black") +
                    labs(x = "% of Responders Who Reported a Decrease in Total Income",
                         y = "Increase in Poverty (%)") +
                    theme_bw() +
                    theme(legend.position = "none") +
                    xlim(0, 100) +
                    ylim(0, 5)    
            }
            
            else{
                if(input$select_indicator == "food") {
                    ggplot(food, aes(y = poverty_increase, x = skip_meal, 
                                     color = country)) +
                        geom_point() +
                        geom_text_repel(aes(label = country, vjust = -1), 
                                  size = 3.5, color = "black", 
                                  segment.color = "transparent") +
                        labs(x = "Percentage of Responders Who Reported Skipping a Meal in the Past 30 Days",
                             y = "Increase in Poverty (%)") +
                        theme_bw() +
                        theme(legend.position = "none") +
                        xlim(0, 100) +
                        ylim(-1, 5) +
                        geom_hline(yintercept = 0, linetype = "dashed")
                }
                else{
                    if(input$select_indicator == "education"){
                        ggplot(education, aes(y = poverty_increase, 
                                              x = education, 
                                              color = Country)) +
                            geom_point() +
                            geom_text_repel(aes(label = Country, vjust = -1), 
                                            size = 3.5, color = "black", 
                                            segment.color = "transparent") +
                            labs(x = "% of Responders Who Reported Their School-Aged Children Were Still Engaged in any Educational Learning",
                                 y = "Increase in Poverty (%)") +
                            scale_color_discrete("Country") +
                            theme_bw() +
                            theme(legend.position = "none") +
                            xlim(0, 100) +
                            ylim(-1, 5) +
                            geom_hline(yintercept = 0, linetype = "dashed")
                    }}
            }}
        
    })
    
    # These texts correspond with the above plots based on which World Bank
    # High-Frequency phone survey indicator is selected.
    
    output$PlotsIndicatorText <- renderText({
        if(input$select_indicator == "labor") {
            "The labor market in the SSA region has been negatively 
            impacted by the COVID-19 outbreak. Over 29 percent of 
            respondents reported losing their job after the outbreak 
            (for the 11 surveyed countries with this indicator). In 
            both Gabon and Kenya, 61 percent of respondents reported 
            that they had stopped working since the COVID-19 outbreak. 
            In general, employed respondents were more likely to be 
            involved with farming activities than non-farm 
            enterprises."
        }
        
        else {
            if(input$select_indicator == "income") {
                "Income has sharply decreased in all surveyed SSA 
                countries, particularly for non-farming enterprises. In 
                Gabon, Malawi, and Zambia over 60 percent of households 
                reported a decrease in total income since the pandemic. 
                For wage income specifically, over 24 percent of households, 
                with available information, reported a decrease. In half of 
                these countries, over 53 percent of households reported a 
                decrease in wage income, with Malawi being the highest at 
                86 percent. The impact on income was more severe for non-farm
                family businesses than farm businesses, although for both 
                sectors, at least 24 percent of households in all surveyed 
                countries reported an income decrease. For the majority of 
                countries that were surveyed on remittances, over 58 percent 
                of households reported a decrease in remittances."
            }
        
        else {
            if(input$select_indicator == "food") {
                "The pandemic has created damaging consequences on food 
                   security for people who live in SSA countries. For half of 
                   the 10 countries that were surveyed on this indicator, over 
                   64 percent of respondents reported skipping a meal in the 
                   last 30 days due to lack of money or resources. Food 
                   security levels did not appear to correlate with the 
                   industry sector that a person worked in (the four categories
                   were agriculture, commerce, mining/manufacturing, and other 
                   services). There also did not seem to be a link between food 
                   security and urban/rural residents."
            }
            else{
                if(input$select_indicator == "education") {
                    "The pandemic has led to a significant decrease in 
                   school-aged children’s educational engagement. Before the 
                   pandemic, over 71 percent of all households with school-aged 
                   children of the 11 surveyed countries (aside from Gabon) had
                   children enrolled in primary and/or secondary schools. 
                   However, since school closures, less than 24 percent of 
                   households with school-aged children in all countries had 
                   children who had completed any school assignments. School 
                   closures have led to a disproportionate impact on different 
                   SSA countries in terms of non-school educational engagement. 
                   In wealthier countries, school-aged children have been able 
                   to engage in alternative educational activities more than 
                   school-aged children in poorer countries. In general, 
                   school-aged children in urban areas were more likely to be 
                   engaged in learning activities compared to those in rural 
                   areas. This is likely due to rural areas having less access 
                   to electricity, and by extension, the scarcity of 
                   electricity-dependent learning resources such as computers, 
                   radios, and internet all pose problems for educational 
                   attainment for school-aged children in rural areas."
                }
            }
        }}
    })
    
    # Here is where I added the variable definitions for the Glossary on my
    # Model page.
    
    output$VariableDefinition <- renderText({
      if(input$select_definition == "electricity") {
        "The percentage of a country's population with access 
        to electricity. Electrification data are collected from 
        industry, national surveys and international sources."
      }
      
      else {
        if(input$select_definition == "literacy") {
          "The percentage of people ages 15 and above who can 
          both read and write with understanding a short simple 
          statement about their everyday life."
        }
        
        else {
          if(input$select_definition == "gdp") {
            "The annual percentage growth rate of GDP at 
            market prices based on constant local currency. Aggregates
            are based on constant 2010 U.S. dollars. GDP is the sum of
            gross value added by all resident producers in the economy
            plus any product taxes and minus any subsidies not included
            in the value of the products. It is calculated without 
            making deductions for depreciation of fabricated assets 
            or for depletion and degradation of natural resources.
"
          }
          
          else{
            if(input$select_definition == "child") {
              "The percentage of children between the ages of
              7 and 14 who work."
              
            }
            
            else{
              if(input$select_definition == "gini") {
                "Measurement of inequality, specifically, the extent to which 
                     the distribution of income (or, in some cases, consumption
                     expenditure) among individuals or households within an 
                     economy deviates from a perfectly equal distribution."
                
              }
              else{
                if(input$select_definition == "goveff") {
                  "Captures perceptions of the quality of public services, the 
                  quality of the civil service and the degree of its 
                  independence from political pressures, the quality of 
                  policy formulation and implementation, and the 
                  credibility of the government's commitment to such 
                  policies. Estimate gives the country's score on the 
                  aggregate indicator, in units of a standard normal 
                  distribution, i.e. ranging from approximately -2.5 to 2.5."
                  
                }
                else{
                  if(input$select_definition == "life") {
                    "The number of years a newborn infant would live 
                     if prevailing patterns of mortality at the time of its 
                     birth were to stay the same throughout its life."
                    
                  }
                  
                  else{
                    if(input$select_definition == "nourishment") {
                    "Percentage of population below minimum level
                     of dietary energy consumption (also referred to as 
                     prevalence of undernourishment) shows the percentage 
                     of the population whose food intake is insufficient to 
                     meet dietary energy requirements continuously. Data 
                     showing as 5 may signify a prevalence of 
                     undernourishment below 5%."
                      
                    }
                    
                    else{
                      if(input$select_definition == "school") {
                     "The ratio of children of official school age 
                     who are enrolled in school to the population of the 
                     corresponding official school age."
                        
                      }
                    }
                  }
                }
              }
          }
        }}
    }})
    
    # Code below displays a scatterplot and line of best fit when the X1 and
    # Y variables are selected.
    
  output$poverty_regression <- renderPlot({
      p = indicator_data %>%
          ggplot(aes_string(x = input$varOI_x, y = input$varOI_y)) +
          geom_point() +
          theme_classic() +
          geom_jitter() +
          labs(title = "Scatterplot Depicting Relationship Between the X1 and Y Variables",
               caption = "Source: World Bank Group")
        
        # Here, I'm adding axis labels that correspond to what is selected.
        
      if (input$varOI_x == "government_effectiveness_estimate")
            p <- p + xlab("Degree of Government Effectiveness")
      if (input$varOI_x == 
          "literacy_rate_adult_total_percent_of_people_ages_15_and_above")
            p <- p + xlab("Adult Literacy (%):")
      if (input$varOI_x == 
          "prevalence_of_undernourishment_percent_of_population")
            p <- p + xlab("Prevalence of Undernourishment (%)")
      if (input$varOI_x == 
          "access_to_electricity_percent_of_population")
        p <- p + xlab("Access to Electricity (%)")
      if (input$varOI_x == 
          "gdp_growth_annual_percent")
        p <- p + xlab("Annual GDP Growth (%)")
      if (input$varOI_x == 
          "children_in_employment_total_percent_of_children_ages_7_14")
        p <- p + xlab("Child Employment (%)")
      if (input$varOI_x == 
          "gini_index_world_bank_estimate")
        p <- p + xlab("Gini Index")
      if (input$varOI_x == 
          "school_enrollment_primary_percent_gross")
        p <- p + xlab("Primary School Enrollment")
                
      if (input$varOI_y == 
          "poverty_headcount_ratio_at_1_90_a_day_2011_ppp_percent_of_population")
                    p <- p + ylab("Poverty Rate at $1.90 a day (2011 PPP, %)")
        if (input$varOI_y == 
           "poverty_headcount_ratio_at_national_poverty_lines_percent_of_population")
                    p <- p + ylab("Poverty Rate at National Poverty Lines (%)")
                
                        
        # Here is a trendline for if the user selects Liner model. Thanks Wyatt
        # for the inspiration :))))
                        
        if (input$type == "toggleLinear")
            p <- p + geom_smooth(method = "lm",
                 se = TRUE,
                 formula = y ~ x)
                 p
    })
    
    # Here, I render all of my regression outputs, which correlate with
    # what the user selects for co-variates and model types.
    
    output$RegSum <- render_gt({
        if(input$type == "toggleLinear")
            pov_model <-
                stan_glm(data = indicator_data,
                         family = gaussian,
                         formula = paste(input$varOI_y, " ~ ", input$varOI_x),
                         refresh = 0)
        
        if (input$type == "toggleMulti") {
          
          if(input$varOI_x3 == "None") {
            pov_model <-
                stan_glm(data = indicator_data,
                         family = gaussian,
                         formula = paste(input$varOI_y, " ~ ", 
                                         input$varOI_x, "+", input$varOI_x2),
                         refresh = 0
                ) }
        
          if(input$varOI_x3 != "None") {
            pov_model <-
              stan_glm(data = indicator_data,
                       family = gaussian,
                       formula = paste(input$varOI_y, " ~ ", 
                                       input$varOI_x, "+", 
                                       input$varOI_x2, "+", 
                                       input$varOI_x3),
                       refresh = 0
              ) }}
        
        if (input$type == "toggleMultiinteraction") {
          
          if(input$varOI_x3 == "None") {
            pov_model <-
              stan_glm(data = indicator_data,
                       family = gaussian,
                       formula = paste(input$varOI_y, " ~ ", 
                                       input$varOI_x, "*", 
                                       input$varOI_x2),
                       refresh = 0
              ) }
          
          if(input$varOI_x3 != "None") {
            pov_model <-
              stan_glm(data = indicator_data,
                       family = gaussian,
                       formula = paste(input$varOI_y, " ~ ", 
                                       input$varOI_x, "*", 
                                       input$varOI_x2, "+", 
                                       input$varOI_x3),
                       refresh = 0
              ) }}
        
        if (input$type == "toggleMultiinteraction3") {

            pov_model <-
              stan_glm(data = indicator_data,
                       family = gaussian,
                       formula = paste(input$varOI_y, " ~ ", 
                                       input$varOI_x, "*", 
                                       input$varOI_x2, "*", 
                                       input$varOI_x3),
                       refresh = 0
              ) }
        
        # Here, I print the results of my model in a regression table.
        
        tbl_regression(pov_model,
                       intercept = FALSE) %>%
          as_gt() %>%
          tab_header(title = "Regression Table",
                     subtitle = "Predicted Value of Y given the selected X 
                     covariates") %>%
          tab_source_note(md("Source: https://data.worldbank.org"))
        
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)