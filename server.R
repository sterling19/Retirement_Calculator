library(DT)
library(magrittr)
library(ggplot2)
library(scales)

# Import data tables
LE.df <- read.delim("LE.txt")
names(LE.df) <- c("Age", "Male Life Expectancy", 
                  "Female Life Expectancty")
TB.df <- read.delim("TB.txt")
names(TB.df) <- c("Tax rate", "Single filers", "Married filing 
                  jointly", "Married filing separately",
                  "Head of household")

# Server Setup
shinyServer(function(input, output){
    
  OutputFunction <- reactive({
    
    # Validations
    validate(need(
      input$LifeExp >= (input$Ages[2] - input$Ages[1]),
      "Let's assume you won't die before retirement."))
    
    validate(need(
      input$EquityAllo + input$BondAllo + input$AltAllo == 100,
      "Portfolio allocation must sum to 100%")) 
    
    # Temp input variables
    TaxStat <- input$TaxStat
    TaxType <- input$TaxType
    SocSec <- input$SocSec
    CurSal <- input$CurSal
    RetAssets <- input$CurRetSav
    TaxRate <- 0 
    
    # Set Range Variables
    Yrs2Ret <- input$Ages[2] - input$Ages[1]
    YrsinRet <- input$LifeExp - Yrs2Ret
    WhenRet <- length(1: Yrs2Ret)
    
    # Dataframe with Pre-Allocated Space
    RetDF <- data.frame(
      Year = numeric(Yrs2Ret + YrsinRet),
      Assets = numeric(Yrs2Ret + YrsinRet),
      Status = character(Yrs2Ret + YrsinRet),
      stringsAsFactors = F)
        
    # Tax Bracket Function
    CheckTax <- function(Sal, TaxStat){
      if (TaxStat == "TaxSing") {
        if (Sal <= 9075) {TaxRate <- 0.1}
        else if (Sal <= 36900) {TaxRate <- 0.15}
        else if (Sal <= 89350) {TaxRate <- 0.25}
        else if (Sal <= 186350) {TaxRate <- 0.28}
        else if (Sal <= 405100) {TaxRate <- 0.33}
        else if (Sal <= 406750) {TaxRate <- 0.35}
        else {TaxRate <- 0.396}
      } else if (TaxStat == "TaxMarJoint") {
        if (Sal <= 18150) {TaxRate <- 0.1}
        else if (Sal <= 73800) {TaxRate <- 0.15}
        else if (Sal <= 148850) {TaxRate <- 0.25}
        else if (Sal <= 226850) {TaxRate <- 0.28}
        else if (Sal <= 405100) {TaxRate <- 0.33}
        else if (Sal <= 457600) {TaxRate <- 0.35}
        else {TaxRate <- 0.396}
      } else if (TaxStat == "TaxMarSep") {
        if (Sal <= 9075){TaxRate <- 0.1}
        else if (Sal <= 36900) {TaxRate <- 0.15}
        else if (Sal <= 74425) {TaxRate <- 0.25}
        else if (Sal <= 113425) {TaxRate <- 0.28}
        else if (Sal <= 202550) {TaxRate <- 0.33}
        else if (Sal <= 228800) {TaxRate <- 0.35}
        else {TaxRate <- 0.396}
      } else if (TaxStat == "TaxHead") {
        if (Sal <= 12950) {TaxRate <- 0.1}
        else if (Sal <= 49400) {TaxRate <- 0.15}
        else if (Sal <= 127550) {TaxRate <- 0.25}
        else if (Sal <= 206600) {TaxRate <- 0.28}
        else if (Sal <= 405100) {TaxRate <- 0.33}
        else if (Sal <= 432200) {TaxRate <- 0.35}
        else {TaxRate <- 0.396}
      }
      
      return(TaxRate)
    }
    
    # Before Retirement
    for (n in 1:Yrs2Ret) {
      
      # Set Dynamic Variables
      TaxRate <- CheckTax(CurSal, TaxStat)
      SalGrow <- CurSal * (1 + (input$ExpIncr/100))
      Contr <- CurSal * (input$ContrRate/100)
      
      EquityROI <- (RetAssets * (input$EquityAllo/100) * 
        (1 + (input$EquityIncr/100))) - 
        (RetAssets * (input$EquityAllo/100))
      
      BondROI <- (RetAssets * (input$BondAllo/100) * 
        (1 + (input$BondIncr/100))) -
        (RetAssets * (input$BondAllo/100))
      
      AltROI <- (RetAssets * (input$AltAllo/100) * 
        (1 + (input$AltIncr/100))) -
        (RetAssets * (input$AltAllo/100))
      
      # Add contributions/asset growth and deduct taxes
      RetAssets <- RetAssets + Contr + EquityROI + BondROI 
        + AltROI
      if (TaxType == "Pretax")
      {RetAssets <- RetAssets - (TaxRate * Contr)}
      
      # Grow Salary
      CurSal <- SalGrow
      
      # Append to dataframe
      RetDF$Year[n] <- n
      RetDF$Assets[n] <- RetAssets
      RetDF$Status[n] <- "Employed"
    }
    
    # After Retirement
    for (n in WhenRet: (length(1:Yrs2Ret) + YrsinRet)) {
      
      # Set Dynamic Variables
      Wthdrwl <- CurSal * (input$PostSal/100)
      TaxRate <- CheckTax(Wthdrwl, TaxStat)
      
      # Add SS income/asset growth and deduct taxes
      RetAssets <- RetAssets - Wthdrwl + SocSec + EquityROI + 
        BondROI + AltROI
      if (TaxType == "Posttax")
      {RetAssets <- RetAssets - (TaxRate * Wthdrwl)}
      
      # Append to dataframe
      RetDF$Year[n] <- n
      RetDF$Assets[n] <- RetAssets
      RetDF$Status[n] <- "Retired"
    }
    
    return(RetDF)
  })
  
  RetLine <- reactive({
    Yrs2Ret <- input$Ages[2] - input$Ages[1]
    WhenRet <- length(1: Yrs2Ret)
    return(WhenRet)
  })

  # Plot Output
  output$OutPlot <- renderPlot({
    
    RetDF <- OutputFunction()
    WhenRet <- RetLine()

    p <- qplot(x = RetDF$Year, y = RetDF$Assets, geom = "smooth",
               method = "loess", xlab = "Years", ylab = "Assets")
    
    p <- p + scale_y_continuous(labels = dollar) +
      geom_hline(aes(yintercept = 0), colour = "red") +
      geom_vline(aes(xintercept = WhenRet)) +
      annotate("text", x = (WhenRet + 2), y = RetDF$Assets[WhenRet],
        label = "Retirement") + theme_classic()
      
    print(p)  
  })
  
  # Data tables
  output$LifeExpTable <- DT::renderDataTable({
    datatable(LE.df, rownames = F, options = list(
      dom = "ft", language = list(search = "Enter age:"),
      columnDefs = list(
        list(className = "dt-center", targets = c(0:2))
      )
    ))
  })
  
  output$TaxTable <- DT::renderDataTable({
    datatable(TB.df, rownames = F, options = list(
      dom = "t", ordering = F, columnDefs = list(
        list(className = "dt-center", targets = c(0:4))
      )
    ))
  })
  
  output$OutTable <- DT::renderDataTable({
    
    RetDF <- OutputFunction()
    
    datatable(RetDF, rownames = F, options = list(
      dom = "ft", language = list(search = "Enter year:"),
      columnDefs = list(
        list(className = "dt-center", targets = c(0:2))
      )
    )) %>% formatCurrency("Assets")
    
  })
    
})