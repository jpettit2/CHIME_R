
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Minimal CHIME App."),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(style = "overflow-y:scroll; max-height: 700px; position:relative;",width =2,
            
            #Inputs
            numericInput("i_hosp",label = "Currently Hospitalized COVID-19 Patients",
                         value = 6,min = 1,max = 10000),
            
            numericInput("i_doubling",label = "Doubling time before social distancing (days)",
                         value = 7,min = 1,max = 30),
            
            numericInput("i_rel_contact",label = "Social distancing (% reduction in social contact)",
                         value = 0,min = 0,max = 100),
            
            numericInput("i_hosp_rate",label = "Hospitalization (% of total infections)",
                         value = 6,min = 1,max = 100),
            
            numericInput("i_icu_rate",label = "ICU (% total infections)",
                         value = 3,min = 1,max = 100),
            
            numericInput("i_vent_rate",label = "Ventilated %(total infections)",
                         value = 1,min = 1,max = 100),
            
            numericInput("i_hosp_los",label = "Average COVID Hospital LOS",
                         value = 10,min = 1,max = 30),
            
            numericInput("i_icu_los",label = "Average COVID ICU LOS",
                         value = 9,min = 1,max = 20),
            
            numericInput("i_vent_los",label = "Average COVID Vent. LOS",
                         value = 8,min = 1,max = 20),
            
            numericInput("i_market_share",label = "Hospital Market Share (%)",
                         value = 14,min = 1,max = 100),
            
            numericInput("i_S",label = "Regional Population",
                         value = 3200000,min = 1,max = 10000000),
            
            numericInput("i_inf",label = "Currently Known Regional Infections",
                         value = 40,min = 1,max = 1000000),
            
            numericInput("i_hosp_cap",label = "Hospital Capacity",
                         value = 800,min = 1,max = 1000),
            
            numericInput("i_icu_cap",label = "ICU Capacity",
                         value = 150,min = 1,max = 150),
            
            numericInput("i_vent_cap",label = "Vent. Capacity",
                         value = 200,min = 1,max = 200)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            
            tabsetPanel(type = "pills",
                        
                        tabPanel("About",
                                 fluidPage(
                                     fluidRow(includeMarkdown("About.md"))
                                 )
                        ),
                        
                        tabPanel("Plots",
                                 fluidPage(
                                     fluidRow(
                                         column(width= 12,
                                                sliderInput("i_n_days",
                                                            label = "Number of days for forecast",
                                                            value = 100,min = 1,max = 365,step = 1)
                                                )
                                         ),
                            
                            
                            fluidRow(column(12,
                                            plotlyOutput(outputId = "plot_census"  ))
                            ),
                            fluidRow(column(12,
                                            plotlyOutput(outputId = "plot_admits"  )
                            )
                            ))),
                        
                        tabPanel("Tabular Data",
                                 fluidPage(
                                     fluidRow(column(6, downloadButton("report", "Download Forecast Data"))),
                                     fluidRow(column(12,   dataTableOutput("forecast")
                                     )
                                     )
                                 ))
            )
            
    
            
        )
    )
))
