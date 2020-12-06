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

region_poverty <- read_xlsx("raw_data/finaldata.xlsx", 1)
country_poverty <- read_xlsx("raw_data/finaldata.xlsx", 2)
country_gdp <- read_xlsx("raw_data/finaldata.xlsx", 3)

labor <- read_xlsx("raw_data/ssa_hfs.xlsx", 1)
income <- read_xlsx("raw_data/ssa_hfs.xlsx", 2)
food <- read_xlsx("raw_data/ssa_hfs.xlsx", 3)
education <- read_xlsx("raw_data/ssa_hfs.xlsx", 4)

indicator_data <- read_csv("raw_data/indicator.csv")

finaldata <- read_xlsx("raw_data/finaldata.xlsx", 3) %>%
  mutate(pov_increase = `2020_new` - `2019_new`,
         pov_increase_BA = `2020_BA` - `2019_BA`,
         pov_increase_MPO =`2020_MPO` - `2019_MPO`)

population <- read_xls("raw_data/population.xls", 1) %>%
  clean_names() %>%
  select(country_code, x2019) %>%
  rename(population = x2019)

finaldata <- left_join(finaldata, population, by = c("code" = "country_code")) %>%
  mutate(new_poor = pov_increase*population)

# Loading leaflet data

data("wrld_simpl")
world_simple_custom <- wrld_simpl

world_simple_custom@data <- left_join(world_simple_custom@data, finaldata, 
                                      by = c("ISO3" = "code")) %>%
  clean_names()


# Define UI ----
ui <- fluidPage(
    
    theme = shinytheme("flatly"),
    
    titlePanel(
        h1("Analyzing COVID-19's Impact on Poverty in Sub-Saharan Africa", 
           align = "center"),
        tags$head(HTML("<title>Analyzing COVID-19's Impact on Poverty in 
                       Sub-Saharan Africa</title>"))),
    
    navlistPanel(
        "Background",
        tabPanel("Introduction",
                 br(),
                 HTML('<center><img src="Africa.png" width="400"></center>'),
                 br(),
                 br(),
                 h3("How much will poverty rise in Sub-Saharan Africa in 2020?"),
                 p("Since its initial outbreak, the COVID-19 pandemic has brought about 
                 a sharp reduction in global economy activity. For Sub-Saharan 
                 Africa (SSA), daily infections rates have declined since 
                 mid-July, although recently, there has been a small spike in 
                 new cases as countries continue to conduct more testing. While 
                 the officially reported infections in SSA remain relatively 
                 low compared to other regions, the data may be misleading given
                 that testing capacity is limited in many countries, and we may 
                 still be in the intermediate stages of the pandemic in the 
                 region. Areas with weak healthcare infrastructure could easily
                 be overwhelmed by a rapidly expanding pandemic, the effects of 
                 which will be aggravated by the global economic downturn as 
                 well as government-mandated lockdowns in the region."),
                 h3("The Analysis"),
                 p("This dashboard presents estimates of the increase in poverty
                 and as an update from a", a("previous World Bank note", 
                 href = "https://openknowledge.worldbank.org/handle/10986/33765")," 
                 which was 
                 published in May 2020. Updated estimates based on the October 
                 2020 World Economic Outlook vintage suggest that COVID-19 will
                 have a more severe impact on poverty rates than originally 
                 projected. Additionally, data from high-frequency phone surveys
                 indicate that the pandemic has created severe disturbances in
                 the labor market, household income, food security, and 
                 educational attainment within the region.")),
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
                 database of household surveys, forecasting the welfare aggregate
                 of each country by the growth of the GDP per capita assuming a
                 “distributionally neutral” impact in welfare. The difference in
                 lined-up poverty rates between the two vintages is a rough 
                 estimate of the effect of the crisis on poverty. For more 
                 information, please refer to", a("the May 2020 note.", 
                 href = "https://openknowledge.worldbank.org/handle/10986/33765")),
                 h3("Poverty Projections"),
                 p("This dashboard utilizes three different poverty projections.
                   The first two are POVCAL estimates, which are calculated
                   using the above methology. The baseline POVCAL estimate depicts
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
                 p("*Unless MPO is specifically stated, mentions of poverty
                   projections in this dashboard to poverty projections will
                   be in reference to POVCAL estimates (in accordance with World
                   Bank practices.)"),
                 p("**While I gathered the above inputs, ultimately, the
                   finalized do-file was run by my project supervisor, Jose
                   Montes, due to my not having possession of certain proprietary
                   data.")
                 ),
        "Data",
        tabPanel("Country-Level Data",
            h3("Examining Poverty by Country"),
            p("Click on a country below to see the change in poverty
              rate (POVCAL estimates)."),
            leafletOutput("SSAmap"),
            tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
        br(),
        dataTableOutput("pov_change")
        ),
        tabPanel("Regional Data",
                 mainPanel(
                   h3("Comparing Poverty Differences By Region"),
                   p("This interactive feature allows you to visualize changes
                   in poverty over time in different SSA regions. In the 
                   drop-down menu below, select the region of interest."),
                   p("The Eastern/Central region (AFE) includes Angola, Botswana, 
                   Burundi, Comoros, the Democratic Republic of the Congo, 
                   Eswantini, Ethiopia, Kenya, Lesotho, Madagascar, Malawi, 
                   Mauritius, Mozambique, Namibia, Rwanda, São Tomé and Príncipe,
                  Seychelles, South Africa, South Sudan, Sudan, Tanzania,
                   Uganda, Zambia, and Zimbabwe."),
                   p("The Western/Southern region (AFW) includes Benin, Burkina Faso,
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
        tabPanel("Model",
                 h3("Predicting Poverty"),
                 p(
                     "This project's dataset was taken from the World Bank. The
                     variables below were derived from the Environment and
                     Social Government database. Below are the definitions for
                     all available covariates (quoted directly from the World
                     Bank."
                 ),
                 tags$ul(
                     tags$li(
                       tags$b("Access to Electricity (%)"), 
                       "is the percentage of a country's population with access 
                       to electricity. Electrification data are collected from 
                       industry, national surveys and international sources.)."
                     ),
                     tags$li(
                     tags$b("Adult Literacy Rate (%)"), 
                     "is the percentage of people ages 15 and above who can 
                         both read and write with understanding a short simple 
                         statement about their everyday life."
                   ),
                   tags$li(
                     tags$b("Annual GDP Growth"), 
                     "refers to the annual percentage growth rate of GDP at 
                     market prices based on constant local currency. Aggregates
                     are based on constant 2010 U.S. dollars. GDP is the sum of
                     gross value added by all resident producers in the economy
                     plus any product taxes and minus any subsidies not included
                     in the value of the products. It is calculated without 
                     making deductions for depreciation of fabricated assets 
                     or for depletion and degradation of natural resources."
                   ),
                   tags$li(
                     tags$b("Child Employment"), 
                     "refers to the percentage of children between the ages of
                     7 and 14 who work."
                   ),
                   tags$li(
                     tags$b("Gini Index"), 
                     "measures inequality, specifically, the extent to which 
                     the distribution of income (or, in some cases, consumption
                     expenditure) among individuals or households within an 
                     economy deviates from a perfectly equal distribution."
                   ),
                   tags$li(
                     tags$b("Government Effectiveness Estimate"), "captures 
                     perceptions of the quality of public services, the 
                     quality of the civil service and the degree of its 
                     independence from political pressures, the quality of 
                     policy formulation and implementation, and the 
                     credibility of the government's commitment to such 
                     policies. Estimate gives the country's score on the 
                     aggregate indicator, in units of a standard normal 
                     distribution, i.e. ranging from approximately -2.5 to 2.5."
                     ),
                   tags$li(
                     tags$b("Life Expectancy at Birth"), 
                     "refers to the number of years a newborn infant would live 
                     if prevailing patterns of mortality at the time of its 
                     birth were to stay the same throughout its life."
                   ),
                   tags$li(
                     tags$b("Prevalence of Undernourishment (as % of 
                     population)"), "refers to Population below minimum level
                     of dietary energy consumption (also referred to as 
                     prevalence of undernourishment) shows the percentage 
                     of the population whose food intake is insufficient to 
                     meet dietary energy requirements continuously. Data 
                     showing as 5 may signify a prevalence of 
                     undernourishment below 5%. (source: World Bank)."),
                   tags$li(
                     tags$b("Primary School Enrollment (% net)"), 
                     "refers to the ratio of children of official school age 
                     who are enrolled in school to the population of the 
                     corresponding official school age."
                   )),
                 p(
                     "blah, blah, blah, say something cool about this model"
                 ),
                 sidebarLayout(
                     sidebarPanel(
                         h4("Construct the Model:"),
                         selectInput(
                             "varOI_x",
                             "X1 variable:",
                             choices = c(
                                 "Access to Electricity" = "access_to_electricity_percent_of_population",
                                 "Adult Literacy" = "literacy_rate_adult_total_percent_of_people_ages_15_and_above",
                                 "Annual GDP Growth" = "gdp_growth_annual_percent",
                                 "Child Employment" = "children_in_employment_total_percent_of_children_ages_7_14",
                                 "Gini Index" ="gini_index_world_bank_estimate",
                                 "Government Effectiveness" = "government_effectiveness_estimate",
                                 "Life Expectancy at Birth" = "life_expectancy_at_birth_total_years",
                                 "Prevalence of Undernourishment" = "prevalence_of_undernourishment_percent_of_population",
                                 "Primary School Enrollment" = "school_enrollment_primary_percent_gross"
),
selected = "government_effectiveness_estimate"


),

# Select X2 Variable

selectInput(
  "varOI_x2",
  "X2 variable:",
  choices = c(
    "Access to Electricity" = "access_to_electricity_percent_of_population",
    "Adult Literacy" = "literacy_rate_adult_total_percent_of_people_ages_15_and_above",
    "Annual GDP Growth" = "gdp_growth_annual_percent",
    "Child Employment" = "children_in_employment_total_percent_of_children_ages_7_14",
    "Gini Index" ="gini_index_world_bank_estimate",
    "Government Effectiveness" = "government_effectiveness_estimate",
    "Life Expectancy at Birth" = "life_expectancy_at_birth_total_years",
    "Prevalence of Undernourishment" = "prevalence_of_undernourishment_percent_of_population",
    "Primary School Enrollment" = "school_enrollment_primary_percent_gross",
    "None" = "None" 
  ),
  selected = "None"
  
  
),

# Select X3 variable

selectInput(
  "varOI_x3",
  "X3 variable:",
  choices = c(
    "Access to Electricity" = "access_to_electricity_percent_of_population",
    "Adult Literacy" = "literacy_rate_adult_total_percent_of_people_ages_15_and_above",
    "Annual GDP Growth" = "gdp_growth_annual_percent",
    "Child Employment" = "children_in_employment_total_percent_of_children_ages_7_14",
    "Gini Index" ="gini_index_world_bank_estimate",
    "Government Effectiveness" = "government_effectiveness_estimate",
    "Life Expectancy at Birth" = "life_expectancy_at_birth_total_years",
    "Prevalence of Undernourishment" = "prevalence_of_undernourishment_percent_of_population",
    "Primary School Enrollment" = "school_enrollment_primary_percent_gross",
    "None" = "None" 
  ),
  selected = "None"
  
  
),

# Select Y variable(s) for model.

selectInput(
    "varOI_y",
    "Y variable:",
    choices = c(
        "Poverty Rate at International Poverty Line ($1.90)" = "poverty_headcount_ratio_at_1_90_a_day_2011_ppp_percent_of_population",
        "Poverty Rate at National Poverty Lines" = "poverty_headcount_ratio_at_national_poverty_lines_percent_of_population"),
    selected = "poverty_headcount_ratio_at_1_90_a_day_2011_ppp_percent_of_population"),

# Select Model

radioButtons("type", "Model Type:",
             c("Linear Model" = "toggleLinear",
               "Multivariate Regression" = "toggleMulti",
               "Multivariate Regression w/ X1 & X2 Interaction" = "toggleMultiinteraction",
               "Multivariate Regression w/ X1, X2 & X3 Interaction" = "toggleMultiinteraction3"),
             selected = "toggleLinear")

),

mainPanel(
    
    # Output scatterplot with line of best fit based on model.
    
    plotOutput("poverty_regression", height = 500),
    br(),
    
    # Output summary of regression output. I would like to present this in a nicer format over the next 10 days.
    
    gt_output(outputId = "RegSum"),
    
    tags$style(
        type = "text/css",
        ".shiny-output-error {display: none;}",
        ".shiny-output-error:before {display: none;}"
    )
)
                 )
        ),


        "Other Resources",
tabPanel("About",
         br(),
         br(),
         h3("About Me"),
         p("My name is Sophie, and I am a junior at Harvard College
                   studying Philosophy. As a native Houstonian, I love all
                   things barbeque and am the proud owner of two miniature
                   cacti. You can find the link to my Github", 
           a("here", href ="https://github.com/sophie-z-li")),
         br(),
         h3("About this Project"),
         p("For my final project, I partnered with the
                   World Bank Group's Global Poverty and Equity Practice to analyze
                   COVID-19's impact on poverty in the sub-Saharan Africa
                   region."),
         p("This dashboard updates", a("previously published estimates", 
                                       href = "https://openknowledge.worldbank.org/handle/10986/33765"),"
                   of COVID-19’s effect on poverty levels in Sub-Saharan Africa 
                   (SSA), using GDP growth projections from the October 2020 
                   World Economic Outlook (WEO) database. Prior
                   projections were created based on April 2020 WEO numbers.")),
        tabPanel("PDF",
                 tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                             src="SSA_note_draft.pdf"))
    )
)
# Define server logic ----
server <- function(input, output) {
    
    # Below is my interactive component for regional poverty
    
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
            scale_color_discrete(name = "Estimates",
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
                    scale_color_discrete(name = "Estimates",
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
                    ggplot(aes(x = year, y = poverty, color = type, label = poverty)) +
                    geom_line(size = 1) +
                    geom_point(size = 2) +
                    geom_text_repel(aes(label = poverty), size = 3.5, 
                          color = "black", segment.color = "transparent") +
                    labs(title = "Changes in Poverty in Western/Southern Africa",
                         x = "Year",
                         y = "Poverty Rate (%)") + 
                    ylim(32, 48) +
                    scale_color_discrete(name = "Estimates",
                                         labels = c("Pre-Pandemic", 
                                                    "Post-Pandemic (POVCAL)", 
                                                    "Post-Pandemic (MPO)")) +
                    theme_bw() +
                    theme(plot.title = element_text(hjust = 0.5))
            }
        }}
        
    })
    
output$pov_change <- renderDataTable(world_simple_custom@data %>%
                                     filter(iso3 %in% c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", 
                                     "CMR", "COD", "COM", "CPV", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "KEN",
                                     "LBR", "LSO", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "NAM", "NER", "NGA",
                                     "RWA", "SDN", "SEN", "SLE", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO", "TZA", 
                                     "UGA", "ZAF", "ZMB", "ZWE")) %>%
                                     select(name, population, pov_increase, new_poor) %>%
                                     mutate(pov_increase = round(pov_increase, 2)) %>%
                                     mutate(new_poor = round(new_poor, 0)),
                                     # formatCurrency(c("population"),currency = "", interval = 3, mark = ","),
                                     colnames = c("Country", "Population", 
                                     "Poverty Increase % (POVCAL)", "Estimated # of New Poor"),
                                     options = list(
  pageLength = 10)
)
    
    # Leaflets
    
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
    
    # Here are my plots and text for high-frequency phone surveys
    
    output$PlotsIndicator <- renderPlot({
        if(input$select_indicator == "labor") {
            ggplot(labor, aes(x = poverty_increase, y = working_stop, color = country)) +
                geom_point() +
                geom_text(aes(label = country, vjust = 2), size = 3.5, color = "black") +
                labs(y = "Percentage of Responders Who Reported Having \n Stopped 
                     Working Since the COVID-19 Outbreak",
                     x = "Increase in Poverty (percentage points)") +
                xlim(0, 5) +
                ylim(0, 100) +
                scale_color_discrete("Country") +
                theme_bw() +
                theme(legend.position = "none")
        }
        else{
            if(input$select_indicator == "income"){
                ggplot(income, aes(x = poverty_increase, 
                                   y = decrease_total_income, color = Country)) +
                    geom_point() +
                    geom_text(aes(label = Country, vjust = 2), size = 3.5, color = "black") +
                    labs(y = "Percentage of Responders Who Reported \n a Decrease in Total Income",
                         x = "Increase in Poverty (percentage points)") +
                    theme_bw() +
                    theme(legend.position = "none") +
                    ylim(0, 100) +
                    xlim(0, 5)    
            }
            
            else{
                if(input$select_indicator == "food") {
                    ggplot(food, aes(x = poverty_increase, y = skip_meal, color = country)) +
                        geom_point() +
                        geom_text(aes(label = country, vjust = 2), size = 3.5, color = "black") +
                        labs(y = "Percentage of Responders Who Reported \n Skipping a Meal in the Past 30 Days",
                             x = "Increase in Poverty (percentage points)") +
                        theme_bw() +
                        theme(legend.position = "none") +
                        ylim(0, 100) +
                        xlim(-1, 5) +
                        geom_vline(xintercept = 0, linetype = "dashed")
                }
                else{
                    if(input$select_indicator == "education"){
                        ggplot(education, aes(x = poverty_increase, y = education, color = Country)) +
                            geom_point() +
                            geom_text_repel(aes(label = Country, vjust = 2), size = 3.5, color = "black", segment.color = "transparent") +
                            labs(y = "Percentage of Responders Who Reported Their \n School-Aged Children Were Still Engaged \n in any Educational Learning",
                                 x = "Increase in Poverty (percentage points)") +
                            scale_color_discrete("Country") +
                            theme_bw() +
                            theme(legend.position = "none") +
                            ylim(0, 100) +
                            xlim(-1, 5) +
                            geom_vline(xintercept = 0, linetype = "dashed")
                    }
                }
            }}
        
    })
    
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

    output$PlotLabor <- renderPlot({
        ggplot(labor, aes(x = poverty_increase, y = working_stop, color = country)) +
            geom_point() +
            geom_text(aes(label = country, vjust = 2), size = 3.5, color = "black") +
            labs(y = "Percentage of Responders Who Reported Having \nStopped Working Since the COVID-19 Outbreak",
                 x = "Increase in Poverty (percentage points)") +
            xlim(0, 5) +
            ylim(0, 100) +
            scale_color_discrete("Country") +
            theme_bw() +
            theme(legend.position = "none")
        
    })
    
    output$PlotIncome <- renderPlot({
        ggplot(income, aes(x = poverty_increase, y = decrease_total_income, color = Country)) +
            geom_point() +
            geom_text(aes(label = Country, vjust = 2), size = 3.5, color = "black") +
            labs(y = "Percentage of Responders Who Reported \n a Decrease in Total Income",
                 x = "Increase in Poverty (percentage points)") +
            theme_bw() +
            theme(legend.position = "none") +
            ylim(0, 100) +
            xlim(0, 5)
        
    })
    
    output$PlotFood <- renderPlot({
        ggplot(food, aes(x = poverty_increase, y = skip_meal, color = country)) +
            geom_point() +
            geom_text(aes(label = country, vjust = 2), size = 3.5, color = "black") +
            labs(y = "Percentage of Responders Who Reported \n Skipping a Meal in the Past 30 Days",
                 x = "Increase in Poverty (percentage points)") +
            theme_bw() +
            theme(legend.position = "none") +
            ylim(0, 100) +
            xlim(-1, 5) +
            geom_vline(xintercept = 0, linetype = "dashed")
        
    })
    
    output$PlotEducation <- renderPlot({
        ggplot(education, aes(x = poverty_increase, y = education, color = Country)) +
            geom_point() +
            geom_text_repel(aes(label = Country, vjust = 2), size = 3.5, color = "black", segment.color = "transparent") +
            labs(y = "Percentage of Responders Who Reported Their \n School-Aged Children Were Still Engaged \n in any Educational Learning",
                 x = "Increase in Poverty (percentage points)") +
            scale_color_discrete("Country") +
            theme_bw() +
            theme(legend.position = "none") +
            ylim(0, 100) +
            xlim(-1, 5) +
            geom_vline(xintercept = 0, linetype = "dashed")
        
    })
    
    # Render plot that displays a scatterplot and line of best fit when 1 X and 1
    # Y variable are selected.
    
    output$poverty_regression <- renderPlot({
        p = indicator_data %>%
            ggplot(aes_string(x = input$varOI_x, y = input$varOI_y)) +
            geom_point() +
            theme_classic() +
            geom_jitter() +
            labs(title = "Scatterplot depicting relationship between X1 and Y variables",
                 caption = "Sources: World Bank Group")
        
        # Add appropriate axis labels based on variables selected.
        
        if (input$varOI_x == "government_effectiveness_estimate")
            p <- p + xlab("Degree of Government Effectiveness")
        if (input$varOI_x == "literacy_rate_adult_total_percent_of_people_ages_15_and_above")
            p <- p + xlab("Adult Literacy (%):")
        if (input$varOI_x == "prevalence_of_undernourishment_percent_of_population")
            p <- p + xlab("Prevalence of Undernourishment (%)")
                
                if (input$varOI_y == "poverty_headcount_ratio_at_1_90_a_day_2011_ppp_percent_of_population")
                    p <- p + ylab("Poverty Rate at $1.90 a day (2011 PPP, %)")
                if (input$varOI_y == "poverty_headcount_ratio_at_national_poverty_lines_percent_of_population")
                    p <- p + ylab("Poverty Rate at National Poverty Lines (%)")
                
                        
                        # Add appropriate trendline, based on user selection. 
                        
                        if (input$type == "toggleLinear")
                            p <- p + geom_smooth(method = "lm",
                                                 se = TRUE,
                                                 formula = y ~ x)
                        p
    })
    
    # Render regression output. First, choose the appropriate model, based on
    # user input, and calculate using the selected X and Y variables.
    
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
                         formula = paste(input$varOI_y, " ~ ", input$varOI_x, "+", input$varOI_x2),
                         refresh = 0
                ) }
        
          if(input$varOI_x3 != "None") {
            pov_model <-
              stan_glm(data = indicator_data,
                       family = gaussian,
                       formula = paste(input$varOI_y, " ~ ", input$varOI_x, "+", input$varOI_x2, "+", input$varOI_x3),
                       refresh = 0
              ) }}
        
        if (input$type == "toggleMultiinteraction") {
          
          if(input$varOI_x3 == "None") {
            pov_model <-
              stan_glm(data = indicator_data,
                       family = gaussian,
                       formula = paste(input$varOI_y, " ~ ", input$varOI_x, "*", input$varOI_x2),
                       refresh = 0
              ) }
          
          if(input$varOI_x3 != "None") {
            pov_model <-
              stan_glm(data = indicator_data,
                       family = gaussian,
                       formula = paste(input$varOI_y, " ~ ", input$varOI_x, "*", input$varOI_x2, "+", input$varOI_x3),
                       refresh = 0
              ) }}
        
        if (input$type == "toggleMultiinteraction3") {

            pov_model <-
              stan_glm(data = indicator_data,
                       family = gaussian,
                       formula = paste(input$varOI_y, " ~ ", input$varOI_x, "*", input$varOI_x2, "*", input$varOI_x3),
                       refresh = 0
              ) }
        
        # Print a summary of the model. 
        
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