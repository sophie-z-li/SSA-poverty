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
  mutate(pov_increase = `2020_new` - `2019_new`)

# Loading leaflet data

data("wrld_simpl")
world_simple_custom <- wrld_simpl

world_simple_custom@data <- left_join(world_simple_custom@data, finaldata, 
                                      by = c("ISO3" = "code")) %>%
  clean_names() 


# Define UI ----
ui <- fluidPage(
    
    theme = shinytheme("slate"),
    
    titlePanel(
        h1("Analyzing COVID-19's Impact on Poverty in Sub-Saharan Africa", 
           align = "center"),
        tags$head(HTML("<title>Analyzing COVID-19's Impact on Poverty in 
                       Sub-Saharan Africa</title>"))),
    
    navlistPanel(
        "About",
        tabPanel("Background",
                 br(),
                 HTML('<center><img src="Africa.png" width="400"></center>'),
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
        tabPanel("Methodology",
                 br(),
                 h3("Calculating Poverty"),
                 p("Poverty projections were created by running a pre-existing
                 Stata do-file on updated WEO numbers. While I originally planned
                 on running the do-file to derive the numbers myself, a lack of 
                 access to some proprietary data at the World Bank derailed this 
                 plan. What ultimately happened was I gathered all the new 
                 information I could access to update this do-file and sent it to 
                 my project supervisor Jose Montes. Jose then added the final
                 necessary data, ran the do-file, and then sent the outputted 
                 projections back to me for visualization and analysis. Below 
                 is a brief explanation of how poverty was mathematically 
                 calculated within the do-file."),
                 br(),
                 h5("Inputs"),
                 HTML("<ul><li>Poverty rate per country at the year 2018 (World 
                     Bank Data)</li><li>GDP per capita growth 2019-2021 (World 
                     Economic Outlook Data)</li><li>Welfare aggregate per 
                     country at 2018 (World Bank Data)</li><li>Poverty line at 
                     1.9 PPP (World Bank Data)</li></ul>"),
                 br(),
                 h5("Formulas"),
                 HTML("<ul><li>Welfare Estimation</li></ul>"),
                 p(HTML(paste0("welfare",tags$sub("year"), "= welfare", 
                 tags$sub("year-1"), "* (1 + growthGDPpc",
                 tags$sub("year"), ")"))),
                 HTML("<ul><li>Poverty Estimation</li></ul>"),
                 p(HTML(paste0("Poverty",tags$sub("year"), "="))),
                 p(HTML(paste0("0 if welfare",tags$sub("year"), "> 1.9"))), 
                 p(HTML(paste0("1 if welfare",tags$sub("year"), "≤ 1.9"))),
                 HTML("<ul><li>Regional Poverty Rate Estimation</li></ul>"),
                 p(HTML(paste0("(PovertyRate",tags$sub("country1"), 
                 "*Population",tags$sub("countryn"), "+ PovertyRate",
                 tags$sub("countryn", "* Population",tags$sub("countryn"), 
                 "/Σ (n = 1 to n, for population)"))))
                 ),
        "Data",
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
                 textOutput("PlotsRegionalText"),
                 br(),
                 br())
                 
                  ),
        tabPanel("Country-Level Data",
            h2("Examining Poverty by Country"),
            p("Click on the countries below to see change in poverty"),
            tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff;
                    }
### ADD THIS HERE ###
                    .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}

###To change text and background color of the `Search` box ###
                    .dataTables_filter input {
                            color: #0E334A;
                            background-color: #0E334A
                           }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #000000;
                     }
                    
                    input, button, select, textarea {
    font-family: inherit;
    font-size: inherit;
    line-height: inherit;
    color: black;
                    }
                    
                    a.paginate_button {
    font-family: inherit;
    font-size: inherit;
    line-height: inherit;
    color: white;
                    }

                   "
                            
                            
                            
            )
        ),
            dataTableOutput("pov_change"),
        br(),
          leafletOutput("SSAmap")
        ),
        tabPanel("High-Frequency Phone Surveys",
                 mainPanel(
                 br(),
                 h3("COVID-19 High Frequency Monitoring"),
                 p("The following data was pulled from the World Bank's
                   COVID-19 High-Frequency Monitoring beta (link to
                   external dashboard). Preliminary data from high-frequency 
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
        "Model",
        tabPanel("Regression",
                 h2("Predicting Poverty"),
                 p(
                     "This project's dataset was taken from the World Bank, blah,
                     blah, blah, add more stuff here."
                 ),
                 tags$ul(
                     tags$li(
                         tags$b("Government Effectiveness Estimate"), "captures 
                         perceptions of the quality of public services, the 
                         quality of the civil service and the degree of its 
                         independence from political pressures, the quality of 
                         policy formulation and implementation, and the 
                         credibility of the government's commitment to such 
                         policies. Estimate gives the country's score on the 
                         aggregate indicator, in units of a standard normal 
                         distribution, i.e. ranging from approximately -2.5 to 
                         2.5.(source: World Bank Group)."
                     ),
                     tags$li(
                         tags$b("Adult Literacy Rate (as % of total population"), 
                         "is the percentage of people ages 15 and above who can 
                         both read and write with understanding a short simple 
                         statement about their everyday life. (source: World Bank)."
                     ),
                     tags$li(
                         tags$b("Prevalence of Undernourishment (as % of 
                         population"), "refers to Population below minimum level
                         of dietary energy consumption (also referred to as 
                         prevalence of undernourishment) shows the percentage 
                         of the population whose food intake is insufficient to 
                         meet dietary energy requirements continuously. Data 
                         showing as 5 may signify a prevalence of 
                         undernourishment below 5%. (source: World Bank)."
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
                                 "Government Effectiveness" = "government_effectiveness_estimate",
                                 "Adult Literacy" = "literacy_rate_adult_total_percent_of_people_ages_15_and_above",
                                 "Prevalence of Undernourishment" = "prevalence_of_undernourishment_percent_of_population"
),
selected = "government_effectiveness_estimate"


),

# Select X2 Variable

selectInput(
  "varOI_x2",
  "X2 variable:",
  choices = c(
    "Government Effectiveness" = "government_effectiveness_estimate",
    "Adult Literacy" = "literacy_rate_adult_total_percent_of_people_ages_15_and_above",
    "Prevalence of Undernourishment" = "prevalence_of_undernourishment_percent_of_population",
    "None" = "None" 
  ),
  selected = "None"
  
  
),

# Select X3 variable

selectInput(
  "varOI_x3",
  "X3 variable:",
  choices = c(
    "Government Effectiveness" = "government_effectiveness_estimate",
    "Adult Literacy" = "literacy_rate_adult_total_percent_of_people_ages_15_and_above",
    "Prevalence of Undernourishment" = "prevalence_of_undernourishment_percent_of_population",
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
               "Multivariate Regression w/ Interaction" = "toggleMultiinteraction"),
             selected = "toggleLinear")

# checkboxInput("toggleLinear", label = "Linear Model", value = TRUE),
# checkboxInput("toggleMulti", label = "Multivariate Regression", value = FALSE),
# checkboxInput("toggleMultiinteraction", label = "Multivariate Regression w/ Interaction", value = FALSE),

# Dependent variable slider inputs, used to predict the number of
# water conflict events in a hypothetical basin with the given inputs
# over a 50 year period.

),

mainPanel(
    
    # Output scatterplot with line of best fit based on model.
    
    plotOutput("poverty_regression", height = 500),
    br(),
    
    # Output summary of regression output. I would like to present this in a nicer format over the next 10 days.
    
    gt_output(outputId = "RegSum"),
    
    # Use CSS styling to hide all error messages. This is necessary
    # because the scatterplot displays an error if more than 1 X variable
    # is selected. I ran many tests and could not find a reason why this
    # would be a problem (i.e. where displaying error results would be
    # necessary).
    
    tags$style(
        type = "text/css",
        ".shiny-output-error {display: none;}",
        ".shiny-output-error:before {display: none;}"
    )
)
                 )
        ),


        tabPanel("Posterior Distribution",
                 sidebarLayout(
                   sidebarPanel(
                     h4("Predict Poverty:"),
                     sliderInput(
                       "government_effectiveness_estimate_pred",
                       "Degree of Government Effectiveness:",
                       min = -2.5,
                       max = 2.5,
                       value = 0
                     ),
                     sliderInput(
                       "literacy_rate_adult_total_percent_of_people_ages_15_and_above_pred",
                       "Adult Literacy (%):",
                       min = 0,
                       max = 100,
                       value = 0
                     ),
                     sliderInput(
                       "prevalence_of_undernourishment_percent_of_population_pred",
                       "Prevalence of Undernourishment (%):",
                       min = 0,
                       max = 100,
                       value = 0
                     )
                   ),
                   mainPanel(
                     p(
                       "Now, let's take the model above and predict the level of poverty for a 
        hypothetical country in sub-Saharan Africa
        with the inputs given at left.
          Three numbers are provided. 'Fit' is the model's best guess for the 
        number of conflict events. 'Lwr' and 'upr' provide the range of values 
        within which we are 95% confident that the true number of conflict 
        events lies."
                     ),
                     
                     # Output predict() results. 
                     
                     plotOutput("predictionplot")
                   )
                 )),
        "Other Resources",
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
            ggplot(aes(x = year, y = poverty, color = type)) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            labs(title = "Changes in Poverty in the Overall sub-Saharan Region",
                 x = "Year",
                 y = "Poverty Rate") + 
            ylim(32, 48) +
            scale_color_discrete(name = "Estimates",
                                labels = c("Pre-Pandemic", 
                                           "Post-Pandemic (POVCAL)", 
                                           "Post-Pandemic (MPO)")) +
            theme_bw() +
            theme(panel.border =element_rect(color = "black", fill = NA, 
                                             size =3),
                  plot.title = element_text(hjust = 0.5))
        }
        else{
            if(input$select_region == "AFE"){
                region_poverty %>%
                    filter(year >= 2010) %>%
                    filter(region == input$select_region) %>%
                    ggplot(aes(x = year, y = poverty, color = type)) +
                    geom_line(size = 1) +
                    geom_point(size = 2) +
                    labs(title = "Changes in Poverty in Eastern/Central Africa",
                         x = "Year",
                         y = "Poverty Rate") + 
                    ylim(32, 48) +
                    scale_color_discrete(name = "Estimates",
                                         labels = c("Pre-Pandemic", 
                                                    "Post-Pandemic (POVCAL)", 
                                                    "Post-Pandemic (MPO)")) +
                    theme_bw() +
                    theme(panel.border =element_rect(color = "black", fill = NA, 
                                                     size =3),
                          plot.title = element_text(hjust = 0.5))    
            }
        
        else{
            if(input$select_region == "AFW") {
                region_poverty %>%
                    filter(year >= 2010) %>%
                    filter(region == input$select_region) %>%
                    ggplot(aes(x = year, y = poverty, color = type)) +
                    geom_line(size = 1) +
                    geom_point(size = 2) +
                    labs(title = "Changes in Poverty in Western/Southern Africa",
                         x = "Year",
                         y = "Poverty Rate") + 
                    ylim(32, 48) +
                    scale_color_discrete(name = "Estimates",
                                         labels = c("Pre-Pandemic", 
                                                    "Post-Pandemic (POVCAL)", 
                                                    "Post-Pandemic (MPO)")) +
                    theme_bw() +
                    theme(panel.border =element_rect(color = "black", fill = NA, 
                                                     size =3),
                          plot.title = element_text(hjust = 0.5))
            }
        }}
        
    })
    
output$pov_change <- renderDataTable(world_simple_custom@data %>%
                                     filter(iso3 %in% c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", 
                                     "CMR", "COD", "COM", "CPV", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "KEN",
                                     "LBR", "LSO", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "NAM", "NER", "NGA",
                                     "RWA", "SDN", "SEN", "SLE", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO", "TZA", 
                                     "UGA", "ZAF", "ZMB", "ZWE")) %>%
                                     select(name, pov_increase)
                                       , options = list(
  pageLength = 5)
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
                      world_simple_custom$pov_increase
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
            labs(title = "Predicting Poverty",
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
        
        if (input$type == "toggleMultiinteraction")
          pov_model <-
            stan_glm(data = indicator_data,
                     family = gaussian,
                     formula = paste(input$varOI_y, " ~ ", input$varOI_x, "*", input$varOI_x2),
                     refresh = 0
            )
        
        # Print a summary of the model. 
        
        tbl_regression(pov_model,
                       intercept = FALSE) %>%
          as_gt() %>%
          tab_header(title = "Regression of Poverty Headcount Ratio at National 
             Poverty Lines") %>%
          tab_source_note(md("Source: https://data.worldbank.org"))
        
    })
    
    # Generate a predicted number of conflicted events, using the same models
    # constructed above and user inputs of independent variables.
    
    output$predictionplot <- renderPlot({

      new_obs <- tibble(government_effectiveness_estimate = input$government_effectiveness_estimate_pred,
                        prevalence_of_undernourishment_percent_of_population = input$literacy_rate_adult_total_percent_of_people_ages_15_and_above_pred,
                        literacy_rate_adult_total_percent_of_people_ages_15_and_above = input$prevalence_of_undernourishment_percent_of_population_pred)
      
      posterior_predict(pov_model, newdata = new_obs) %>%
        as_tibble() %>%
        mutate_all(as.numeric) %>%
        ggplot(aes(x = `1`)) +
        geom_histogram()
                })
        
        # Set independent variable slider values equal to the input names
        # required by the above models. Note that the slider names could not be
        # the same as the input names, or else Shiny threw an error.
        
        sliderValues <- reactive({
            data.frame(
                government_effectiveness_estimate = input$government_effectiveness_estimate_pred,
                literacy_rate_adult_total_percent_of_people_ages_15_and_above = input$literacy_rate_adult_total_percent_of_people_ages_15_and_above_pred,
                prevalence_of_undernourishment_percent_of_population = input$prevalence_of_undernourishment_percent_of_population_pred

            )
        })
        
        # Generate prediction, with a 95% confidence interval. 
        
        prediction <- reactive({
            predict(lmsum(),
                    newdata = sliderValues(),
                    interval = "confidence")
        })
        
        # Print prediction.
        
        print(prediction())
    }#)
    
# }

# Run the app ----
shinyApp(ui = ui, server = server)