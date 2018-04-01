library(shiny)
library(shinythemes)

# UI Setup
shinyUI(navbarPage("Retirement Planner",
    
  tabPanel("Home",
    
    #Output Window
    tabsetPanel(
    
      tabPanel("Plot",
        plotOutput("OutPlot", width = "100%", height = "400px")
      ),
      
      tabPanel("Table",
        DT::dataTableOutput("OutTable")
      )
    ),
  
    # Input window
    tabsetPanel(
      
      tabPanel("Profile",
                       
        column(3, wellPanel(
          sliderInput("Ages",
            label = "Current Age and Expected Age at Retirement",
            min = 20, max = 80, value = c(25, 67)),
          numericInput("LifeExp",
            label = "Life Expectancy*",
            value = 50)
        )),
        
        column(3, wellPanel(
          numericInput("CurSal",
            label = "Current Annual Salary ($)",
            value = 50000),
          sliderInput("ExpIncr",
            label = "Expected Annual Salary Increase",
            min = 0, max = 10, value = 2, post = "%")
        )),
        
        column(3, wellPanel(
          sliderInput("ContrRate",
            label = "Salary Contribution**",
            min = 0, max = 100, value = 10, post = "%"),
          sliderInput("PostSal",
            label = "Retirement Income Needed",
            min = 0, max = 100, value = 85, post = "%")
        )),
        
        column(3, wellPanel(
          selectInput("TaxStat",
            label = "Tax Filing Status",
            c("Single" = "TaxSing",
              "Married filing jointly" = "TaxMarJoint",
              "Married filing separately" = "TaxMarSep",
              "Head of household" = "TaxHead")),
          radioButtons("TaxType",
            label = "Contribution Tax",
            c("Pretax" = "Pretax", "Post-tax" = "Posttax"))
        )),
        
        column(12,
          helpText("*See Resources for Life Expectancy"),
          helpText("**Include Employer Match")
        )        
      ),
          
      tabPanel("Savings",
               
        column(3, wellPanel(
          numericInput("CurRetSav",
            label = "Current Account Savings ($)",
            value = 5000),
          numericInput("SocSec",
            label = "Monthly Social Security Income ($)",
            value = 0)
        )),
        
        column(3, wellPanel(
          sliderInput("EquityAllo",
            label = "Equity Allocation",
            min = 0, max = 100, value = 60, step = 5, post = "%"),
          sliderInput("EquityIncr",
            label = "Expected Return on Equity",
            min = 0, max = 20, value = 8, step = 1, post = "%")
        )),
        
        column(3, wellPanel(
          sliderInput("BondAllo",
            label = "Bond Allocation",
            min = 0, max = 100, value = 20, step = 5, post = "%"),
          sliderInput("BondIncr",
            label = "Expected Return on Bonds",
            min = 0, max = 20, value = 4, step = 1, post = "%")
        )),
        
        column(3, wellPanel(
          sliderInput("AltAllo",
            label = "Alternative Allocation",
            min = 0, max = 100, value = 20, step = 5, post = "%"),
          sliderInput("AltIncr",
            label = "Expected Return on Alternatives",
            min = 0, max = 20, value = 4, step = 1, post = "%")
        ))        
      )      
    )
  ),
  
  tabPanel("Resources",
           
    headerPanel("Life Expectancy"),
    DT::dataTableOutput("LifeExpTable"),
    helpText("Source: Social Security Administration, 2011"),
    
    headerPanel("Tax Brackets"),
    DT::dataTableOutput("TaxTable"),
    helpText("Source: bankrate.com, 2014")    
  ),
  
  tabPanel("About",
    
    column(3, wellPanel(
     p("Retirement Calculator - Version 1.0"),
     p("Developed by Sterling Cutler @ 
       https://github.com/sterling19"),
     p("Special thanks to Ricky Pickering.")
    ))
  ),
  
  # Style Settings
  theme = shinytheme("cosmo"),
  
  tags$head(
    tags$style(HTML(
      ".shiny-output-error-validation {color: red}"
      ))
    )  
))