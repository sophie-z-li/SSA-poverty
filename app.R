library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(ggthemes)
library(readxl)
library(ggrepel)
library(gganimate)
theme_set(theme_bw())

region_poverty <- read_xlsx("raw_data/finaldata.xlsx", 1)
country_poverty <- read_xlsx("raw_data/finaldata.xlsx", 2)
country_gdp <- read_xlsx("raw_data/finaldata.xlsx", 3)

labor <- read_xlsx("raw_data/ssa_hfs.xlsx", 1)
income <- read_xlsx("raw_data/ssa_hfs.xlsx", 2)
food <- read_xlsx("raw_data/ssa_hfs.xlsx", 3)
education <- read_xlsx("raw_data/ssa_hfs.xlsx", 4)



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
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("select_region",
                                     "Select Region",
                                     c("Sub-Saharan" = "SSA", 
                                       "Eastern/Central" = "AFE", 
                                       "Western/Southern" = "AFW"))
                     ),
                     mainPanel(
                         plotOutput("PlotsRegional", width = 800),
                         textOutput("PlotsRegionalText")
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
                 br(),
                 h3("COVID-19 High Frequency Monitoring"),
                 p("The following data was pulled from the World Bank's
                   COVID-19 High-Frequency Monitoring beta (link to
                   external dashboard). Preliminary data from high-frequency 
                   phone surveys indicate major disruptions to labor markets, 
                   household income, food security, and educational attainment 
                   within the SSA region."),
                 br(),
                 h4("1. Labor"),
                 p("The labor market in the SSA region has been negatively 
                    impacted by the COVID-19 outbreak. Over 29 percent of 
                    respondents reported losing their job after the outbreak 
                    (for the 11 surveyed countries with this indicator). In 
                    both Gabon and Kenya, 61 percent of respondents reported 
                    that they had stopped working since the COVID-19 outbreak. 
                    In general, employed respondents were more likely to be 
                    involved with farming activities than non-farm 
                    enterprises."),
                 h5("Figure 1a. Scatterplot depicting relationship between 
                    poverty rate increases and labor disruption by country."),
                 fluidRow(
                     column(1, align = "center",
                     imageOutput("PlotLabor", width = 800)
                     )
                 ),
                 br(),
                 h4("2. Income"),
                 p("Income has sharply decreased in all surveyed SSA 
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
                   of households reported a decrease in remittances."),
                 h5("Figure 2a. Scatterplot depicting relationship between 
                    poverty rate increases and food security by country."),
                 fluidRow(
                     column(1, align = "center",
                     imageOutput("PlotIncome", width = 800)
                     )
                 ),
                 br(),
                 h4("3. Food"),
                 p("The pandemic has created damaging consequences on food 
                   security for people who live in SSA countries. For half of 
                   the 10 countries that were surveyed on this indicator, over 
                   64 percent of respondents reported skipping a meal in the 
                   last 30 days due to lack of money or resources. Food 
                   security levels did not appear to correlate with the 
                   industry sector that a person worked in (the four categories
                   were agriculture, commerce, mining/manufacturing, and other 
                   services). There also did not seem to be a link between food 
                   security and urban/rural residents. "),
                 h5("Figure 3a. Scatterplot depicting relationship between 
                    poverty rate increases and income disruption by country."),
                 fluidRow(
                     column(1, align = "center",
                     imageOutput("PlotFood", width = 800)
                     )
                 ),
                 br(),
                 h4("4. Education"),
                 p("The pandemic has led to a significant decrease in 
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
                   attainment for school-aged children in rural areas."),
                 h5("Figure 4a. Scatterplot depicting relationship between 
                    poverty rate increases and school-aged children's 
                    educational engagement by country."),
                 fluidRow(
                     column(1, align = "center",
                     imageOutput("PlotEducation", width = 800)
                     )
                 )),
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
    
    # Below is my interactive component for regional poverty
    
    output$PlotsRegional <- renderPlot({
        if(input$select_region == "SSA") {
        region_poverty %>%
            filter(year >= 2010) %>%
            filter(region == input$select_region) %>%
            ggplot(aes(x = year, y = poverty, color = type)) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            labs(title = "This is the first plot",
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
                    labs(title = "This is the second plot",
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
                    labs(title = "This is the third plot",
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
    
    output$PlotsRegionalText <- renderText({
        if(input$select_region == "SSA") {
            "The ongoing pandemic is expected to drastically slow GDP 
            growth in SSA by almost 7 percentage points compared to 
            pre-pandemic forecasts for 2020, which is 2 percentage points
            higher than the April 2020 projections. The new GDP also 
            estimates a significant increase in the total number of 
            additional poor due to the crisis, from 17.7 to 26.6 million,
            according to the international poverty line of $1.90 per day 
            in 2011 PPP. Additionally, the estimates suggest a slightly 
            larger increase in the poverty rate, by 2.4 instead of the 
            previously estimated 1.6 percentage points."
        }
        
        else {
            if(input$select_region == "AFE") {
                "By region, the poverty rate is expected to increase by almost 
                2 percentage points for Eastern/Southern Africa."
            }
        
        else {
            if(input$select_region == "AFW") {
                "By region, the poverty rate is expected to increase around 3 
                percentage points for Western/Central Africa."
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
    
    
}

# Run the app ----
shinyApp(ui = ui, server = server)