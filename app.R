library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(ggthemes)
library(readxl)

region_poverty <- read_xlsx("raw_data/finaldata.xlsx", 1)
country_poverty <- read_xlsx("raw_data/finaldata.xlsx", 2)
country_gdp <- read_xlsx("raw_data/finaldata.xlsx", 2)

# Define UI ----
ui <- fluidPage(
    
    theme = shinytheme("slate"),
    
    titlePanel(
        h1("Analyzing COVID-19's Impact on Poverty in Sub-Saharan Africa", 
           align = "center")),
    
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
                   cacti. You can find the link to my Github", a("here", href ="https://github.com/sophie-z-li")),
                 br(),
                 h3("About this Project"),
                 p("For my final project, I partnered with the
                   World Bank Group's Global Poverty and Equity Practice to analyze
                   COVID-19's impact on poverty in the sub-Saharan Africa
                   region."),
                 br(),
                 HTML('<center><img src="worldbank.jpg" width="200"></center>'),
                 br(),
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
                 HTML('<left><img src="formulas.png" width="400"></left>')),
        "Data",
        tabPanel("Regional Data",
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
                 br(),
                 
                 # LMAO HOW DO I SET A REGION VECTOR!!!!!
                 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("select_region",
                                     "Select Region",
                                     c("Sub-Saharan" = "SSA", 
                                       "Eastern/Central" = "AFE", 
                                       "Western/Southern" = "AFW"))
                     ),
                     mainPanel(
                         plotOutput("Plot2", width = 800)
                     )
                 ),
                 br(),
                 p("The ongoing pandemic is expected to drastically slow GDP 
                   growth in SSA by almost 7 percentage points compared to 
                   pre-pandemic forecasts for 2020, which is 2 percentage points
                   higher than the April 2020 projections. The new GDP also 
                   estimates a significant increase in the total number of 
                   additional poor due to the crisis, from 17.7 to 26.6 million,
                   according to the international poverty line of $1.90 per day 
                   in 2011 PPP. Additionally, the estimates suggest a slightly 
                   larger increase in the poverty rate, by 2.4 instead of the 
                   previously estimated 1.6 percentage points. By region, the 
                   poverty rate is expected to increase around 3 percentage 
                   points for Western/Central Africa and by almost 2 percentage
                   points for Eastern/Southern Africa.")),
        tabPanel("Country-Level Data"),
        tabPanel("High-Frequency Phone Surveys",
                 p("Preliminary data from high-frequency phone surveys 
                   indicate major disruptions to labor markets, household 
                   income, food security, and educational attainment within 
                   the SSA region.")),
        "Model",
        tabPanel("Poverty-Related Indicators"),
        "Other Resources",
        tabPanel("PDF",
                 tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                             src="SSA_note_draft.pdf"))
    )
)
# Define server logic ----
server <- function(input, output) {
    
    # CHANGE STUFF HERE
    
    output$Plot2 <- renderPlot({
        region_poverty %>%
            filter(year >= 2010) %>%
            filter(region == input$select_region) %>%
            ggplot(aes(x = year, y = poverty, color = type)) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            labs(x = "Year",
                 y = "Poverty Rate") + 
            ggtitle(input$select_region) +
            ylim(32, 48) +
            scale_color_discrete(name = "Estimates",
                                labels = c("Pre-Pandemic", 
                                           "Post-Pandemic (POVCAL)", 
                                           "Post-Pandemic (MPO)")) +
            theme_bw() +
            theme(panel.border =element_rect(color = "black", fill = NA, 
                                             size =3),
                  plot.title = element_text(hjust = 0.5))
        
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)