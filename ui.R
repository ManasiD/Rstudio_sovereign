library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

shinyUI(fluidPage(
  #   titlePanel("Credit Universe"),
  fluidRow(
    tabsetPanel(position = "right", id = "inTabset", 
                tabPanel("PLOT", 
                         
                         column(2,
                                wellPanel(
                                  h6(
                                    
                                    selectInput("genre", "Select Index/Portfolio",
                                                c("All", "Advanced Economies", "Emerging Market","Developing Countries")),
                                    selectInput("rate", "Select Credit Rating",
                                                c("All","AAA","AA","A","BBB","BB","B","CCC","CC")),
                                    
                                    
                                    tags$small(paste0( "AAA=1   
                                                       AA=2   
                                                       A=3   BBB=4   BB=5   B=6  CCC=7   CC=8   C=9")),
                                    
                                    
                                    sliderInput("rank", "Moody's Rating:" ,0, 9, width = '80%', value = c(0, 9)),
                                    #                sliderInput("ranks", "S&P Rating:" ,0, 9, width = '40%', value = c(0, 9)),
                                    #          
                                    
                                    sliderInput("spread5", "Spread5:" , 1, 6000, width = '80%', value = c(1, 6000)),
                                    
                                    #sliderInput("spread10", "Spread10:" , 1, 6000, width = '80%', value = c(1, 6000)),
                                    
                                    selectInput("region", "Select Region:" , c("All","EUROPE","CW of independent states",
                                                                               "LATIN AMERICA & CARIB","NEAR EAST","ASIA", "AFRICA","NORTHERN AMERICA","OCEANIA","AFRICA")),
                                    
                                    #                selectInput("subregion", "Select SUB-Region:" , c("All","ANZ","ASIA", "SUB-SAHARAN AFRICA","NORTHERN AFRICA","Greater China",
                                    #                                                           "RCIS","India & SAARC","WESTERN EUROPE","EASTERN EUROPE","OCEANIA","AFRICA")),
                                    #                
                                    #                selectInput("subregion", "Select SUB-Region:" , c("All")),
                                    
                                    selectizeInput("yvar", "Y-axis variable", axis_vars, selected = "E"),
                                    selectizeInput("xvar", "X-axis variable", axis_vars, selected = "K")
                                    ###              tags$small(paste0(
                                    ###                "Note: The Tomato Meter is the proportion of positive reviews",
                                    ###                " (as judged by the Rotten Tomatoes staff), and the Numeric rating is",
                                    ###                " a normalized 1-10 score of those reviews which have star ratings",
                                    ###                " (for example, 3 out of 4 stars)."
                                    ###              ))
                                    
                                    )  )),
                         
                         
                         column(6,
                                
                                
                                fluidRow(
                                  radioButtons("year" , "Year:" ,  c("2015" = 2015, "2020" = 2020), selected = "2015", inline =1)
                                ),
                                
                                ggvisOutput("plot1")), 
                                
                         
                         column(3, 
                                ggvisOutput("histoy")),
                         
                         column(2,
                                textOutput("nothing")),
                         
                         column(12,  
                                wellPanel(
                                  span("Number of data points:",
                                       textOutput("n_movies")
                                       
                                       
                                  )))
                                ),
                tabPanel(position = "right","TABLE",
                         
                         #      textOutput("m_variables"),
                         
                         checkboxGroupInput('showcols', ' ', (axis_vars2),selected = (axis_vars2), inline =1),
                         
                         tabsetPanel(
                           tabPanel('Sovereign',
                                    dataTableOutput("m_movies")))
                         
                         
                         #tableOutput("m_movies")
                         
                ),
                #tabPanel("Crime HeatMap",  column(6, plotOutput("crimeHeatMap",width="750px",height="1000px")))
                tabPanel(position = "right","HEAT MAP",
                       
                         pre(textOutput("HM_text")),
                                plotOutput("HMPlot",width="1000px",height="1000px")
                                #ggvisOutput("HMPlot")
                                
                         
                ))
    
  )))                                                                                                                                                 