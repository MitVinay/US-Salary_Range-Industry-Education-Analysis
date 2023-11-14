
devtools::install_github("davidsjoberg/ggsankey")

## Library
library(shiny)
library(shinyjs)
library(dplyr)
library(highcharter)
library(stringr)
library(tidyverse)
library(ggsankey)
library(plotly)
library(shinyWidgets)
library(corrplot)

# Reading the Files
df1 <- read_csv("cd_name.csv")
df_finance <- read_csv("final_finance2.csv")
df_education <- read_csv("final_educationv3.csv")
df_edu_finance <- read.csv("question_2_merge_eduction and incomeV2.csv")
industry <- read.csv("final_industry.csv")
df_state_industry <- read_csv("state_industry_final.csv")
df_state_industry_bar <- read_csv("state_industry_bar.csv")
finance <- df_finance
education <- df_education


# Downlaod function replacement( converting json to be for high charter)
## Not using in-built function as downloading data online was resitricted
mapdata <- readLines("state_map.js", warn = FALSE, encoding = "UTF-8")
mapdata[1] <- gsub(".* = ", "", mapdata[1])
mapdata <- paste(mapdata, collapse = "\n")
mapdata <- stringr::str_remove(mapdata, ";$")
mapdata <- jsonlite::fromJSON(mapdata, simplifyVector = FALSE)
state_maps <- mapdata


# State Map
mapdata <- readLines("cd_map.js", warn = FALSE, encoding = "UTF-8")
mapdata[1] <- gsub(".* = ", "", mapdata[1])
mapdata <- paste(mapdata, collapse = "\n")
mapdata <- stringr::str_remove(mapdata, ";$")
mapdata <- jsonlite::fromJSON(mapdata, simplifyVector = FALSE)
cd_maps <- mapdata

# Ordering Income Range
income_order <- c(
  "Less than 5k",
  "5k-9k",
  "10k-14k",
  "15k-19k",
  "20k-24k",
  "25k-34k",	"35k-49k", "50k-74k",	"75k-99k",	"100k-149k",	"150k or more")


# Using categorical data to display in the map for cd
my.cols <- data.frame(
  interval = c(1),
  scale = 1:7,
  industry = c("Educationalservices healthcare socialassistance",
                    "Professional services",
                    "Arts entertainment and foodservices",
                    "Manufacturing" ,
                    "Construction",
                    "Transportation warehousing utilities",
                    "Public administration"
  ),
hexes = c( '#E0EBF4','#42B5C4', '#6CAFD6','#BFD4E6', "#53BA97","#007D90","#81D2BF"))


# Using categorical data to display in the map for state
my.cols2 <- data.frame(
  interval = c(1),
  scale = 1:2,
  gender = c("Female", "Male"),
  hexes = c( "#F4C2C2","#6CAFD6"))

# Defining color for categorical data
stops2 <- data.frame(
  name = my.cols2$gender,
  scaled = 1:2,
  from = 0:1/1,
  color = toupper(my.cols2$hexes),
  stringsAsFactors = FALSE)

# Defining color for categorical data
stops <- data.frame(
  name = my.cols$industry,
  scaled = 1:7,
  from = 0:6/6,
  color = toupper(my.cols$hexes),
  stringsAsFactors = FALSE)


# Coloring Map based on legend
df <- merge(df1, stops, by = "scaled")

stops <- list_parse(stops) 

stops2 <- list_parse(stops2) 

#################### Computation for Education Qualification ######################

# # Reading column names of the Variable
finance_vars <- colnames(df_finance)[-c(1, 2)]  # Exclude 'Year' and 'cd' columns
education_vars <- colnames(df_education)[-c(1, 2)]


# Totaling the population as the sum of individual country is not matching up
df_education$Total_IPopulation <- rowSums(df_education[education_vars])
df_finance$Total_FPopulation <- rowSums(df_finance[finance_vars])


# Normalising the data frame
normalized_finance_data <- df_finance %>%
  mutate(across(all_of(finance_vars), ~ . / Total_FPopulation))

normalized_education_data <- df_education %>%
  mutate(across(all_of(education_vars), ~ . / Total_IPopulation))

# Joining the data frame with Year and Cd column
merged_data <- merge(normalized_finance_data, normalized_education_data, by = c("Year", "cd"))

merged_data1 <- merged_data %>% select(-Total_FPopulation, -Total_IPopulation) %>% filter(Year == 2021)%>%
  select(-Year, -cd)

correlation_matrix <- cor(merged_data1)

submatrix <- correlation_matrix[finance_vars, education_vars]
label_color <- "black"
transposed_matrix <- t(submatrix)

# Ui server
ui <- shiny::fluidPage(
  fluidRow(column(12, br())),
  
  fluidRow( # Heading
    column(12, h1("Impact of Industry and Educational Qualification on Salary Range in the United States",
                  align = "center",
                  style = "color: #31473A; font-family: 'Georgia', serif;
                  font-weight: bold;
                  padding: 10px; background-color: white;") #https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  )
), br(),
br(),
# Introduction
fluidRow(column(12, p("Many people dream of living a prosperous life in the
                      competitive employment market in the United States, where
                      some people have it tough and others do very well. But in the USA,
                      it\'s common to need a high income in order to live a good life.
                      One crucial issue applies, regardless of whether you\'re a college student
                      choosing your courses of study or an employed person considering a career change.",
                      style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;")
)),  br(),

fluidRow(column(12, p("First, let\'s look into the salary-based population distribution.
                      Apparently, there are about 21,499,786 people who make 150k or more amount of money.
                      The left-skewed distribution shown by the bar plot emphasises the substantial
                      proportion of the population that falls into the high-income category
                      The main objective is to show that a significant proportion of
                      the population is in higher pay groups, implying that it is not unrealistic to aim to be in such ranges.
                      ",
                      style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;")
                )),
# Pretty Radio Button
fluidRow(
  column(width = 3, style = "text-align: center;",
         prettyRadioButtons(
           inputId = "year_filter",
           label = "Year",
           icon = icon("dollar", lib = "font-awesome", style = "color: steelblue;"),
           choices = c("2019", "2020", "2021"),
           animation = "jelly",
           status = "primary",
           fill = TRUE,
           bigger = TRUE,
           thick = TRUE,
           outline = TRUE,
           width = "100%",
           selected = "2021"
         )
         
  ), # Salary Distribution Plot
  column(8,
         plotlyOutput("salarybarPlot"))
),br(),

fluidRow(
  column(
    12, 
    div(
      style = "text-align: justify; font-size: 17px; color: #1c1c1c; font-family: Times;",
      p(
        "There are several factors like Industry type, Education and Gender influencing the income in U.S.,
        Let's uncover the effect of 
        these factors below."
      ),
    )
  )
),
fluidRow(
  column(12, h1(" Industry Type",
                align = "center",
                style = "color: #31473A; font-family: 'Georgia', serif;
                  font-weight: bold; font-size: 24px;
                  padding: 10px; background-color: white;") #https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  )
),
fluidRow(column(12, p("Following our analysis, It is clear from our data that your industry association has 
                      a big impact on the range of salaries you are most likely to be in.
                      An accompanying Sankey graphic helps visualise the complex link between
                      pay income and industry type. It is noteworthy that a significant proportion
                      of the workforce working in the fields of education, healthcare, and social
                      assistance is often paid in higher wage ranges. In addition, the professional
                      services industry is closely linked to the income bracket of $150,000 and higher. 
                      Essentially, this means that workers in the professional services sector are more
                      likely to make above $150,000 in compensation. Note: The black bar on some lengend in below 
                      Sankey diagram shows
                      that the attribute is to the right side. (Use the filter button in the left
                      corner of the visualisation to customise it to your interests and narrow it down
                      to the industries you're interested in)",
                      style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;")
)),

# Radio Buttton for year
  fluidRow(
    column(width = 12, style = "text-align: center;",
           div(style = "margin-right: 20px; display: inline-block;", "         "),
           div(style = "display: inline-block;"),
      prettyRadioButtons(
        inputId = "year",
        label = "Year",
        icon = icon("globe", lib = "font-awesome", style = "color: steelblue;"),
        choices = c("2019", "2020", "2021"),
        animation = "jelly",
        status = "primary",
        inline = TRUE,
        fill = TRUE,
        bigger = TRUE,
        thick = TRUE,
        outline = TRUE,
        width = "100%",
        selected = "2020"
      )
    
    )
  ),
  
  fluidRow( style = "background-color: white;",
            column(
              12, br())
            
  ),fluidRow( style = "background-color: white;",
              column(3, p("Click below",
                          style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:14px;")
              )
  ),
  
# Side Drop Down Syntax
  fluidRow( style = "background-color: white;",
            column(
              1,
              dropdown(
                
                tags$h3("List of Input"),
                
                pickerInput(inputId = "industry",
                            label = "Industry Type",
                            choices = list("All",
                                           "Educationalservices healthcare socialassistance",
                                           "Professional services",
                                           "Arts entertainment and foodservices",
                                           "Manufacturing",
                                           "Construction",
                                           "Transportation warehousing utilities",
                                           "Public administration"),
                            options = list(`style` = "btn-info"),
                            selected = "All", multiple = TRUE),
                
                style = "unite", 
                icon = icon("chart-bar", lib = "font-awesome", style = "color: steelblue;"),
                status = "primary", width = "300px",
                animate = animateOptions(
                  enter = animations$fading_entrances$fadeInLeftBig,
                  exit = animations$fading_exits$fadeOutRightBig
                )
              )
            )
  ),
  fluidRow( style = "background-color: white;",
            column(
              12, br())
            
  ),
# Plotting Sankey Diagram
  fluidRow( style = "background-color: white;",
            column(
              12,
              plotlyOutput("sankeyPlot")
            )
  ),br(),
fluidRow(column(12, p("As observed earlier, People working in the education, healthcare, and social 
                      assistance sectors typically fall into a variety of income brackets. 
                      Why is this phenomenon?. After a more thorough investigation 
                      and upscaling granularity to the congressional district level,
                      Every count on the stacked bar plot represents
                      the number of congressional districts .The stacked bar chart below illustrates that
                      individuals working in Education, Healthcare, and Social Assistance, across diverse
                      salary brackets, are associated with different congressional districts.
                      It becomes clear that your workplace location affects your
                      salary range even within a particular industry.",
                      style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;")
)), #Plotting Stacked bar plot for salary grouping Indstry
  fluidRow( style = "background-color: white;",
            column(
              12,
              plotlyOutput("inustrybar")
            )
  ), # syntax for side drop down icon
fluidRow(column(12, p("The map below serves as an excellent representation,
                      illustrating the ideal locations to work in specific 
                      industries for achieving a particular salary bracket. Hovering the cursor over the map
                      reveals that Oregon's congressional district level 2 pays between $50,000 and $74,000 in
                      the fields of education, healthcare, and social assistance, but California's congressional
                      district level 4 pays more than $150,000. (To remove certain pay ranges, use the filter on
                      the right.)",
                      style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;")
)),fluidRow( style = "background-color: white;",
             column(3, p("Click below",
                         style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:14px;")
             )
),
fluidRow( style = "background-color: white;",
          column(
            1,
            dropdown(
              
              tags$h3("List of Input"),
              
              pickerInput(inputId = "salary",
                          label = "Salary Range",
                          choices = list("All","150k and more", "100k-149k", "50k-74k"),
                          options = list(`style` = "btn-info"),
                          selected = "All"),
              
              style = "unite", icon = icon("th-list", lib = "font-awesome", style = "color: steelblue;"),
              status = "primary", width = "300px",
              animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
              )
            )
          )
),
# Plotting congressional district level map
fluidRow( style = "background-color: white;",
          column(
            12, shinyjs::useShinyjs(),
            highcharter::highchartOutput(width = "100%", height = 550, outputId = "industry_map_usa"),
            shiny::uiOutput(outputId = "ui")
          )
),

fluidRow( style = "background-color: white;",
          column(
            12, br())
          
),

fluidRow(
  column(12, h1(" Educational Qualification",
                align = "center",
                style = "color: #31473A; font-family: 'Georgia', serif;
                  font-weight: bold; font-size: 24px;
                  padding: 10px; background-color: white;") #https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  )
),
fluidRow( style = "background-color: white;",
          column(
            12,
            p("There is evidence of a correlation between education level and income type
              in the correlation matrix. As one\'s education increases, especially after earning 
              a bachelor\'s degree or more, the correlation with higher income groups grows
              This suggests that there is a positive correlation between income and education level.
              For instance, there is an almost perfect positive correlation between the income range
              of $150,000 or more and the term \"bachelor\'s degree or higher\" Furthermore, obtaining 
              a \"bachelor\'s degree or higher\" is negatively correlated with falling into a lower
              income category, indicating that individuals are less likely to do so after completing 
              their higher education. Conversely, there is a clear positive association between less 
              education and lower income levels. For example, there is a substantial correlation 
              between earning between $15,000 and $74,999 and having a high school diploma.
              Furthermore, there is a marginally correlation between those earning between $5,000
              and $35,000 and those without a high school education. Note: The correlation coefficient's
              magnitude is shown by the circle's radius.",
              style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;")
          )
),# Plotting Correlation matrix
fluidRow( style = "background-color: white;",
          column(
            12,
            plotOutput("correlation_matrix")
          )
),
fluidRow( style = "background-color: white;",
          column(3, p("Click below",
            style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:14px;")
          )
), # Slider input for scatter plot 
fluidRow( style = "background-color: white;",
          column(
            1,
            dropdown(
              
              tags$h3("List of Input"),
              
              pickerInput(inputId = "finance_var",
                          label = "Choose Income Range",
                          choices = c("Less than 5k",
                                      "5k-9k",
                                      "10k-14k",
                                      "15k-19k",
                                      "20k-24k",
                                      "25k-34k",
                                      "35k-49k",
                                      "50k-74k",
                                      "75k-99k",
                                      "100k-149k",
                                      "150k or more"),
                          options = list(`style` = "btn-info"),
                          selected = "150k or more", multiple = FALSE),
              
              pickerInput(inputId = "education_var",
                          label = "Choose Educational Qualification",
                          choices = c("Bachelors degree or higher",
                                      "High school or some degree",
                                      "Less than high school graduate"),
                          options = list(`style` = "btn-info"),
                          selected = "Bachelors degree or higher", multiple = FALSE),
              
              style = "unite", 
              icon = icon("chart-bar", lib = "font-awesome", style = "color: steelblue;"),
              status = "primary", width = "300px",
              animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
              )
            )
          )
),
# plotting scatter plot
fluidRow( style = "background-color: white;",
          column(
            12,
            plotlyOutput("scatter_plot")
          )
),br(),
fluidRow(
  column(12, h1(" Gender Distribution",
                align = "center",
                style = "color: #31473A; font-family: 'Georgia', serif;
                  font-weight: bold; font-size: 24px;
                  padding: 10px; background-color: white;") #https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  )
),
fluidRow( style = "background-color: white;",
            column(
              12,
              p("As previously mentioned, finding the industry that offers 
                the best salaries is only the first step. A closer look at the
                data reveals an intriguing feature: the distribution of genders across 
                different businesses. The map illustrates a clear distinction, the information
                sector tends towards male domination whereas industries like educational services, healthcare, 
                and social assistance are controlled by women. This finding is significant because it 
                implies that there may be distinct opportunities when you join an industry where the 
                gender distribution is similar to your own. The pie chart below shows the gender distribution of U.S.
                working individuals. ",
                style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;")
            )
),
br(), # Radio button for year
fluidRow(
  column(width = 12, style = "text-align: center;",
         div(style = "margin-right: 20px; display: inline-block;", "         "),
         div(style = "display: inline-block;"),
         prettyRadioButtons(
           inputId = "gender_year",
           label = "Year",
           icon = icon("globe", lib = "font-awesome", style = "color: steelblue;"),
           choices = c("2019", "2020", "2021"),
           animation = "jelly",
           status = "primary",
           inline = TRUE,
           fill = TRUE,
           bigger = TRUE,
           thick = TRUE,
           outline = TRUE,
           width = "100%",
           selected = "2020"
         )
         
  )
),br(),

# Show a plot of the generated distribution
fluidRow(column(12,
                plotlyOutput("gender_pie"))
),
fluidRow( style = "background-color: white;",
          column(
            12,
            p("Click on the state to visulise the exact Gender distribution of the state",
              style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;")
          )
), # Drop dowbnb for industry
fluidRow( style = "background-color: white;",
          column(3, pickerInput(inputId = "bar_industry",

          label = "Industry Type",
          choices = list("Agriculture",
                         "Construction",
                         "Manufacturing",
                         "Wholesale trade",
                         "Retail trade",
                         "Information",
                         "Finance insurance realestate",
                         "Professional  services",
                         "Educationalservices healthcare socialassistance",
                         "Arts entertainment and foodservices","Otherservices","Public administration"), 
          selected = "Finance insurance realestate"),
          options = list(`style` = "btn-info"),
          selected = "All", multiple = TRUE),
    column(
      9,
            highcharter::highchartOutput(width = "100%", height = 550, outputId = "gender_map_usa")
          )),
# Plotting stack barplot
fluidRow( style = "background-color: white;",
          column(
            12,plotlyOutput("gender_bar")
          )),
br(),
fluidRow(
  column(12, h1("Conclusion",
                align = "center",
                style = "color: #31473A; font-family: 'Georgia', serif;
                  font-weight: bold; font-size: 24px;
                  padding: 10px; background-color: white;") #https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  )
),
fluidRow( style = "background-color: white;",
          column(
            12,
            p("We can state with confidence, based on the 
            detailed visualisations, that choosing the appropriate industry and
            obtaining better education have a substantial association with reaching greater pay range.
            But it's not just about breaking into a field; it's also important to work at the best possible
            location. The difficulty is not just getting into the field but also overcoming gender domination,
            which might make it harder to break into it. Therefore to guarantee a greater income, it is necessary to
              emphasise the location's equal significance at the same time, Gender also becomes an important 
              consideration for 
              certain businesses.  ",
              style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;")
          )
), # Plotting data resources
fluidRow(
  column(12, h1("Data Resources",
                align = "center",
                style = "color: #31473A; font-family: 'Georgia', serif;
                  font-weight: bold; font-size: 24px;
                  padding: 10px; background-color: white;") #https://stackoverflow.com/questions/51298177/how-to-centre-the-titlepanel-in-shiny
  )
),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:19px;",
          column(12,a("A. U.S. Map Data - Non - Tabular Data (JS File)", href = "https://code.highcharts.com/mapdata/countries/us/us-all.js"))), 
br(),

fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:19px;",
         column(12,a("B. Industry Population Data - Tabular Data", href = " https://data.census.gov/table?q=industry&g=010XX00US$8600000&kd=ACSST5Y2021.S2404"))),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;",
          column(12,p("1. Industry file 1: rows 33120 X columns 542, Year: 2019 "))),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;",
          column(12,p("2. Industry file 2: rows 33120 X columns 542, Year: 2020 "))),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;",
          column(12,p("3. Industry file 3: rows 33120 X columns 542, Year: 2021 "))),
br(),

fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:19px;",
          column(12,a("C. Finance Population Data - Tabular Data", href = " https://data.census.gov/table?q=Financial+Characteris<cs&g=010XX00US$8600000&<d=ACSST5Y202 1.S2503"))),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;",
          column(12,p("1. Finance file 1: rows 33120 X columns 1106, Year: 2019 "))),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;",
          column(12,p("2. Finance file 2: rows 33120 X columns 1106, Year: 2020 "))),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;",
          column(12,p("3. Finance file 3: rows 33120 X columns 1106, Year: 2021 "))),
br(),

fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:19px;",
          column(12,a("D. Education Population Data - Tabular Data", href = "https://data.census.gov/table?q=Educakonal+Aeainment&g=010XX00US$8600000&kd=ACSST5Y2021.S1501"))),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;",
          column(12,p("1. Education file 1: rows 33120 X columns 1106, Year: 2020 "))),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;",
          column(12,p("2. Education file 2: rows 33120 X columns 1106, Year: 2021 "))),
br(),


fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:19px;",
          column(12,a("E. Zip code - Tabular Data 1", href = "https://github.com/OpenSourceActivismTech/us-zipcodes-congress/blob/master/zccd.csv"))),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;",
          column(12,p("1. Zip code file 1: rows 41924 X columns 4 "))),
br(),


fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:19px;",
          column(12,a("F. Zip code - Tabular Data 2", href = "https://www.unitedstateszipcodes.org/zip-code-database/"))),
fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:17px;",
          column(12,p("1. Zip code file 2: rows 42735 X columns 14 "))),
br(),

fluidRow( style ="color:#1c1c1c; font-family:'times'; text-align: justify;font-size:13px;",
          column(12,p("Note: Click on the heading to download the data.")))
)

# Server 
server <- function(input, output, session) {
  
### Salary  Bar plot
  filtered_data_1 <- reactive({
    df_finance %>%
      filter(Year == input$year_filter)
  })
  
  # Reshape and summarize the data
  summarized_data <- reactive({
    filtered_data_1() %>%
      gather(key = "Income", value = "Value", -Year, -cd) %>% 
      group_by(Year, Income) %>%
      summarise(total = sum(Value), .groups = "drop") %>%
      mutate(Income = factor(Income, levels = income_order))
  })
  
  
  # Salary Plot
  output$salarybarPlot <- renderPlotly({
    

      ggplot(summarized_data() %>% filter(!is.na(Income)), aes(x = Income,
                                    y = total), hoverinfo="text", 
             text = ~paste0("mpg: ", "\nwt: ")) +
        geom_bar(stat = "identity", position = "dodge", fill = "lightsteelblue", color = "steelblue", alpha = 0.7 )+
        labs(
          title = paste(input$year_filter),
          x = "Income Range",
          y = "Total Value",
          fill = "Year"
        ) +
        theme_minimal() + 
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1 ),
          plot.background = element_rect(fill = "white"),  # Set the background color to white
          panel.background = element_rect(fill = "white"),  # Set the panel background color to white
        ) +
        scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "k")) 
      
  })
  
  
 ## Syntax for plotting map
  
  filter_df <- reactive({
    if (input$salary == "All"){
      df  %>%
        filter(year == input$year)
    }else{
      df  %>%
        filter(year == input$year & Income == input$salary)
    }
    
  })
  

  # Sankey Diagram filter
  
  filter_df_sankey <- reactive({
    if ("All" %in% input$industry || is.null(input$industry) ){
      df  %>%
        filter(year == input$year)
    }
    else{
      df  %>%
        filter(year == input$year & Industry %in% input$industry)
    }
    
  })
  
  df_san1 <- reactive({filter_df_sankey() %>%
      make_long(Industry,Income) %>% mutate(types = factor(node))
  })
  
   # plotting map
  usa_state_map <- shiny::reactive({
    highcharter::highchart() %>%
      highcharter::hc_add_series_map(
        map = cd_maps,
        df = filter_df(),
        joinBy = c("hc-key", "cd_name"),
        value = "from",
        borderColor = "black",
        showInLegend = TRUE
      ) %>%hc_plotOptions(series = list(allAreas = TRUE))%>%
      hc_colorAxis(dataClasses = stops) %>%
      hc_legend(align = 'right',verticalAlign = "bottom",x = -10,y = 10) %>%
      highcharter::hc_tooltip(
        headerFormat = "<b>{point.key}</b><br>",
        pointFormat = "State: {point.State}<br>Industry Type: {point.Industry}<br>Income Range: {point.Income}<br>",
        useHTML = TRUE
      ) 
  })
  
  # 
  output$industry_map_usa <- highcharter::renderHighchart({ usa_state_map() })
  
  ## If state clicked, add button to go back to state map
  output$ui <- shiny::renderUI({
    if (!is.null(input$interaction)) {
      shiny::actionButton(
        inputId = "geo_button",
        label = "Return to USA Map"
      )
    }
  })
  custom_palette <- c('#939C9E','#BFD4B6','#BFD4E6','#BFD4B6', 
                      '#42B5C4','#E0EBF4', '#6CAFD6','#BFD4E6', "#53BA97","#007D90","#81D2BF")
  
  # Sankey Diagram
  output$sankeyPlot <- renderPlotly({
    ggplot(df_san1(), aes(
      x = x,
      next_x = next_x,
      node = node,
      next_node = next_node,
      fill = types,
      label = node
    )) +
      geom_sankey(
        flow.alpha = 0.5,
        node.color = "black",
        show.legend = FALSE
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank()
      ) +
      scale_fill_manual(values = custom_palette) +  # Use your custom color palette
      labs(title = paste("Association between Industry Type and Income Range in", input$year)) +
      labs(fill = 'Nodes')
  })
  
  # Stacked Barplot
  output$inustrybar <- renderPlotly({
    ggplot(filter_df_sankey(), aes(x = Income, fill = Industry)) +
      geom_bar( position = "stack", color = "black") +
      labs(title = paste("Distribution of Industry wise Income Ranges in", input$year),
           x = "Income Range",
           y = "Total Value",
           fill = "Industry Type") +
      scale_fill_manual(values = c('#6CAFD6', '#42B5C4', '#E0EBF4','#BFD4E6', "#53BA97","#007D90","#81D2BF")) +
      theme_bw() +
      theme(
        legend.position = "right",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank()
      ) 
    
  }) 

    #Correlation Matrix
    output$correlation_matrix <- renderPlot({corrplot(transposed_matrix,
                                                      tl.col = label_color,
                                                      method = "circle"
    )
    })
    
    # Scatter plot
    df_edu_finance_filter <- reactive({
      df_edu_finance %>% filter(Income == input$finance_var & education_range == input$education_var)
    })
    
    
    output$scatter_plot <- renderPlotly({
      ggplot(df_edu_finance_filter(), aes(x = income_value	, y = education_value)) +
        geom_point(shape = 21, color = "black", fill = "steelblue", size = 2) +
        stat_smooth(method = lm, color = "black", fill = "lightsteelblue", alpha = 0.3) +
        labs(title = paste(input$education_var ,"/", input$finance_var),
             x = "Income Range Normalised Population",
             y = "Education Qualification Normalised Population",
             fill = "Industry Type") +
        theme_minimal() +  # Set the theme to minimal
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.background = element_rect(fill = "white", color = NA),  # Set the background color to white
          panel.background = element_rect(fill = "white"),
        )
      
    })
# PIE CHART
    
    df_industry <- reactive({industry %>% filter(Year == input$gender_year) %>%
        select( Male, Female) %>%
        gather(key = "Gender", value = "total") %>%
        group_by(Gender) %>%
        summarise(Total = sum(total))  %>%
        ungroup() %>%
        mutate(Percentage = (Total / sum(Total))) %>% 
        arrange(Percentage) %>%
        mutate(labels = scales::percent(Percentage)) })
    
    # Pie plot
    output$gender_pie <- renderPlotly({ # https://stackoverflow.com/questions/62021769/plotly-r-pie-chart-how-to-fixate-the-color-assignment-color-per-group
      plot_ly(df_industry(), labels = ~Gender, values = ~Total, sort = F, 
              marker = list(colors = c("1" = "#F4C2C2", 
                                       "2" = "#6CAFD6"
              ))) %>%
        add_pie(hole = 0.3) %>%
        layout(legend = list(orientation = 'h'), margin = list(l = 0 , r = 0, t = 0, b = 100, pad = 1),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))})
    

    
    df_map2_gender <- reactive({
      df_state_industry %>%
        filter(Year == input$gender_year & Industry == input$bar_industry)
    }) 
    
    # gender distribution map plot
    usa_state_map2 <- shiny::reactive({
      highcharter::highchart() %>%
        highcharter::hc_add_series_map(
          map = state_maps,
          df = df_map2_gender(),
          joinBy = c("postal-code", "state_code"),
          value = "Values",
          borderColor = "black",
          dataLabels = list(enabled = TRUE, format = "{point.State_name}"),
          showInLegend = TRUE
          # color = "industry_type"
        ) %>%hc_plotOptions(series = list(allAreas = TRUE))%>%
        hc_colorAxis(dataClasses = stops2) %>%
        hc_legend(align = 'right',verticalAlign = "bottom",x = -10,y = 10) %>%
        highcharter::hc_tooltip(
          headerFormat = "<b>{point.key}</b><br>",
          pointFormat = "Gender: {point.Gender}<br>",
          useHTML = TRUE
        )%>%
      highcharter::hc_plotOptions(
        series = list(
          allowPointSelect = TRUE,
          events = list(
            click = htmlwidgets::JS(
              "function(event) {
                 Shiny.setInputValue(
                   'interaction', 
                   event.point.state_code, 
                   {priority: 'event'}
                 );
              }"
            )
          )
        )
      ) 
    })
    
    output$gender_map_usa <- highcharter::renderHighchart({ usa_state_map2() })
    
    check_df1 <- reactive({ 
    df_state_industry_bar %>%
      filter(Year == input$gender_year & state_code == input$interaction)
    })
    
  

    # link interaction on click the state of the U.S. MAP
    shiny::observeEvent(
      eventExpr = input$interaction,
      handlerExpr = {
        output$gender_bar <- renderPlotly({

          check_df1() %>%
            mutate(
              Industry = factor(Industry),
              Gender = factor(Gender, levels = c("Male", "Female"))
            ) %>% 
            group_by(Industry, Gender) %>%
            summarise(n = sum(Values, na.rm = TRUE)) %>%
            mutate(pct = prop.table(n)) %>%
            ggplot(aes(pct, x = Industry, fill = Gender)) +
            geom_col(
              color = "black", # Border color of the bars
              size = 0.5       # Border size of the bars
            ) +
            geom_text(
              aes(label = scales::percent(pct, accuracy = .1)),
              position = position_stack(vjust = 0.5),
              color = "black", # Text color
              size = 3          # Text size
            ) +
            scale_fill_manual(
              values = c("#F4C2C2", "#6CAFD6"),  # Custom colors for Male and Female
              name = "Gender"
            )+
            theme_bw() +
            theme(
              legend.position = "right",
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              plot.background = element_blank(),
              panel.border = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1 )
                  
                
            )
        })
        
      }
    )
  
}


shiny::shinyApp(ui = ui, server = server)