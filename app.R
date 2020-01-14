# alt + o : Code folded


# Libraries ---------------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(XLConnect)
library(reshape)
library(aws.s3)
load("Data.RData")
riverdata <<- riverdata


s3BucketName <- "data.riskassessment.newfoundland"
Sys.setenv("AWS_ACCESS_KEY_ID" = Id,
           "AWS_SECRET_ACCESS_KEY" = Key,
           "AWS_DEFAULT_REGION" = "eu-west-1")

s3load(object = "inputs.RData", bucket = s3BucketName)
inputs <<- inputs



# Choices ---------------------------------
# Enumerated list of the names of farm sites along with Total as nr. 1
Choices = setNames(c(1:(length(rownames(farmsites))+1)),c('Total',farmsites$SiteName))


# header ---------------------------------
header <- dashboardHeaderPlus(
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "fish",
  title = "Risk assessment"
)

# sidebar ---------------------------------
sidebar <- dashboardSidebar(sidebarMenu(
  ## sidebar Menu Map ---------------------------------
  menuItem("Map", tabName = "Map", icon = icon("map-marked")),
  ## sidebar Menu Plot ---------------------------------
  menuItem("Plot", tabName = "Plot", icon = icon("chart-bar")),
  ## sidebar Menu Table ---------------------------------
  menuItem("Table", tabName = "Table", icon = icon("table")),
  ## sidebar Menu Weibull ---------------------------------
  menuItem("Weibull", tabName = "Weibull", icon = icon("chart-line")),
  ## sidebar Menu Parameters ---------------------------------
  menuItem("Parameters", tabName = "Parameters", icon = icon("sliders-h"))
))

# body ---------------------------------
body <- dashboardBody(  tabItems(
  ## body Map tab ---------------------------------
  tabItem(tabName = "Map",
          h2("Map"),
          leafletOutput("mymap")),
  ## body Plot tab ---------------------------------
  tabItem(
    tabName = "Plot",
    h2("Plot"),
    plotOutput("myPlot1"),
    selectInput(
      "Graph",
      label = "Pick a farm site",
      choices = Choices,
      selected = 1
    ),
    fluidRow(
      column(4,radioButtons(
        "radio",
        label = "",
        choices = list("Amount" = 1, "Percentage" = 2),
        selected = 2
      )),
      column(4,radioButtons(
        "all",
        label = "",
        choices = list("Show only affected rivers" = 1, "Show all rivers" = 2),
        selected = 1
      ))
    )
  ),
  ## body Table tab ---------------------------------
  tabItem(
    tabName = "Table",
    h2("Table"),
    DT::dataTableOutput("table")
  ),
  ## body Weibull tab ---------------------------------
  tabItem(
    tabName = "Weibull",
    h2("Weibull Plot"),
    plotOutput("myPlot2"),
    selectInput(
      "Graph2",
      label = "Pick a farm site",
      choices = Choices,
      selected = 1
    )
  ),
  ## body Parameter tab ---------------------------------
  tabItem(
    tabName = "Parameters",
    fluidRow(
      column(
        3,
        h4(""),
        helpText(
          "Here you can change the values of the parameters which affect the model. "
        )
      ),
      column(
        3,
        h4("Weibull coefficients for early escapees:"),
        sliderInput(
          "beta",
          label = "beta",
          0,
          min = 1,
          max = 10,
          step = 0.1,
          value = inputs$beta
        ),
        sliderInput(
          "eta",
          label = "eta",
          0,
          min = 100,
          max = 1000,
          step = 10,
          value = inputs$eta
        )
      ),
      
      column(
        3,
        h4("Proportion of smolts that survive their stay at sea:"),
        sliderInput(
          "surv",
          label = NULL,
          0,
          min = 0,
          max = 1,
          step = 0.01,
          value = inputs$surv
        ),
        h4("Proportional fitness of farmed smolts vs. wild:"),
        sliderInput(
          "surv2",
          label = NULL,
          0,
          min = 0,
          max = 1,
          step = 0.01,
          value = inputs$surv2
        )
      ),
      
      column(
        3,
        h4("Amount of escapees per ton produced:"),
        sliderInput(
          "escS",
          label = "Early escapees:",
          0,
          min = 0,
          max = 10,
          step = 0.1,
          value = inputs$escS
        ),
        sliderInput(
          "esc",
          label = "Late escapees:",
          0,
          min = 0,
          max = 10,
          step = 0.1,
          value = inputs$esc
        )
      )
      
      
    ),
    
    fluidRow(
      column(
        3,
        h4("Homing parameter:"),
        sliderInput(
          "home",
          label = "Early escapees:",
          0,
          min = 0,
          max = 5,
          step = 0.05,
          value = inputs$home
        ),
        sliderInput(
          "home2",
          label = "Late escapees:",
          0,
          min = 0,
          max = 5,
          step = 0.05,
          value = inputs$home
        )
      ),
      
      column(
        3,
        h4("Weibull coefficients for late escapees:"),
        sliderInput(
          "beta2",
          label = "beta",
          0,
          min = 1,
          max = 10,
          step = 0.1,
          value = inputs$beta2
        ),
        sliderInput(
          "eta2",
          label = "eta",
          0,
          min = 100,
          max = 1000,
          step = 10,
          value = inputs$eta2
        )
      ),
      
      column(
        3,
        h4(
          "Proportion of late escapees that sexually mature and go up into rivers:"
        ),
        sliderInput(
          "sex",
          label = NULL,
          0,
          min = 0,
          max = 1,
          step = 0.01,
          value = inputs$sex
        )
      ),
      
      column(
        3,
        h4("Length of rearing period at sea for a farmed salmon:"),
        sliderInput(
          "cTime",
          label = NULL,
          0,
          min = 10,
          max = 20,
          step = 0.5,
          value = inputs$cTime
        ),
        h4("Dangerperiod in months:"),
        sliderInput(
          "critPer",
          label = NULL,
          0,
          min = 2,
          max = 12,
          step = 0.5,
          value = inputs$critPer
        )
      )
    )
  )
),
tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
))

# right sidebar ---------------------------------
rightsidebar = rightSidebar(
  ## rightsidebar Production ---------------------------------
  rightSidebarTabContent(
    id = 1,
    active = TRUE,
    title = "Yearly production of farmed salmon in thousands of tons in: ",
    icon = "circle",
    uiOutput("selectionsTons")
  ),
  ## rightsidebar Save ---------------------------------
  rightSidebarTabContent(
    id = 2,
    active = FALSE,
    title = "Here you can save current state of the app: ",
    icon = "save",
    actionButton("saver", "Save")
  )
)

# UI ---------------------------------
ui <- dashboardPagePlus(title = 'Newfoundland',
                        header,
                        sidebar,
                        body,
                        rightsidebar,
                        skin = 'blue'
)



# Server ---------------------------------
server <- function(input, output) {
  ## fastar ---------------------------------
  s3load(object = "inputs.RData", bucket = s3BucketName)
  inputs <<- inputs
  
  amountOfRivers <- nrow(riverdata)
  amountOfEldis <- nrow(farmsites)
  
  defZoom = 5
  mybins=c(0,4,10,100)
  mypalette = colorBin( palette=c("red3", "gold", "darkgreen"), domain = c(0:100), na.color="transparent", bins=mybins,reverse = T)
  ## Output UI controls ---------------------------------
  output$selectionsTons <- renderUI({
    tagList(
      lapply(c(1:length(rownames(farmsites))),
             function(x)
             {
               h5(farmsites[x,1])
               sliderInput(
                 paste("tonn",x,sep=""),
                 label = farmsites[x,1],
                 0,
                 min = 0,
                 max = 50,
                 step = 0.1,
                 value = eval(parse(text=paste('inputs$tonn',x,sep="")))
               )
             }
      )
    )
  })

  
  ## dataZoom ---------------------------------
  dataZoom <-
    reactive({
      # Tekur inn zoom-level til að stilla af stærðir
      zoom <- input$mymap_zoom
      if (is.null(zoom)) {
        zoom = defZoom
      }
      zoom
    })
  
  ## dataKviAdult ---------------------------------
  # Föll sem taka inn riverdataýsingarnar um eldisstaðina og reikna út Distributionuna á fiskunum
  dataKviAdult <- reactive({
    kviAdult <- data.frame(
      lapply(rownames(farmsites),
             function(x)
             {
               amount = eval(parse(text=paste('input$tonn',x,sep="")))
               if (is.null(amount)){
                 amount=0
               }
               amount = amount*1000 
               Distribution(
                 farmsites[x, "position"],
                 input$beta2,
                 input$eta2,
                 amount,
                 input$esc,
                 input$sex,
                 input$cTime,
                 input$critPer,
                 input$home2
               )
             }
      )
    )
    kviAdult
  })
  ## dataKviSmolt ---------------------------------
  dataKviSmolt <- reactive({
    kviSmolt <- data.frame(
      lapply(rownames(farmsites),
             function(x)
             {
               amount = eval(parse(text=paste('input$tonn',x,sep="")))
               if (is.null(amount)){
                 amount=0
               }
               amount = amount*1000
               smoltDistribution(
                 farmsites[x, "position"],
                 input$beta,
                 input$eta,
                 amount,
                 input$escS,
                 input$surv * input$surv2,
                 input$home
               )
             }
      )
    )
    kviSmolt
  })
  ## dataKviTotal ---------------------------------
  dataKviTotal <- reactive({
    kviTotal <- dataKviAdult() + dataKviSmolt()
    kviTotal
  })
  
  ## dataGraph ---------------------------------
  # Takes the input which chooses what farmsite to show
  dataGraph <- reactive({
    Graph <- input$Graph
    if (Graph == 1) {
      Graph = amountOfEldis+2
    }
    Graph
  })
  
  ## dataGraph2 ---------------------------------
  dataGraph2 <- reactive({
    Graph2 <- input$Graph2
    if (Graph2 == 1) {
      Graph2 = amountOfEldis+2
    }
    Graph2
  })
  
  
  ## dataKvi ---------------------------------
  #Fall sem setur upp data.frameið sem inniheldur allar riverdataýsingar, t.d. Distributionar, stock size o.fl
  dataKvi <- reactive({
    kvi <- data.frame(riverdata$Stock.Size,
                      dataKviTotal(),
                      row.names =  as.character(riverdata$RiverName))

    kvi$"Farmed salmons" <- rowSums(kvi[, 2:(amountOfEldis+1)])
    colnames(kvi) <-
      c(
        "Wild salmons",
        lapply(farmsites$SiteName,function(x) paste('Farmed salmons from',x)),
        "Farmed salmons"
      )
    kvi$"Percentage of total" = round(kvi$"Farmed salmons" / (kvi$"Wild salmons"+kvi$"Farmed salmons"),
                                   digits = 4) * 100
    Total <- colSums(kvi)

    Total[amountOfEldis+3] = round(sum(kvi$"Farmed salmons") / (
      sum(kvi$"Farmed salmons") + sum(kvi$"Wild salmons")
    ), digits = 4)*100
    kvi <- rbind(kvi, Total)
    row.names(kvi) = c(as.character(riverdata$RiverName), "Total")
    kvi
  })
  
  ## dataKviMagn ---------------------------------
  
  # fall sem skilar vigri sem inniheldur magnið í öllum fjörðum
  dataKviMagn <- reactive({
    kviMagn <- c(lapply(
      rownames(farmsites),
      function(x){
        if(is.null(eval(parse(text = paste('input$tonn',x,sep = ""))))){
          0
        } else{ 
          eval(parse(text = paste('input$tonn',x,sep = "")))
        }
      }
    ))
    kviMagn
  })
  
  
  ## Output map ---------------------------------
  # Kortið teiknað upp og stillt´á Ísland
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 1, maxZoom = 15)) %>%
      addProviderTiles(providers$Hydda.Base) %>%
      setView(-56.126223719013126,48.56949991261743, zoom = 6) %>%
      addLegend( pal=mypalette, values=c(0,4,10,100), opacity=0.9, title = "Percent", position = "bottomright" )
    
    
  })
  ## Output map rivers ---------------------------------
  # Kökuritin teiknuð inn, proxy notað til að kortið sjálft endurstillist ekki einungis kökurnar
  observe({
    j <- 1
    leafletProxy("mymap") %>%
      clearGroup("river")
    for (i in riverdata$SizeCategory ) {
      if (i != 0) {
        leafletProxy("mymap") %>%
          addCircleMarkers(
            lng = riverdata[j, 2],
            lat = riverdata[j, 3],
            fillColor = mypalette(dataKvi()$'Percentage of total'[j]),
            fillOpacity = 1,
            stroke = TRUE,
            color = 'black',
            weight = 1,
            opacity = 1,
            radius = as.numeric(i)/2,
            group = "river",
            label = paste("River: ", riverdata$RiverName[j], "<br/>", "Stock size: ", round(dataKvi()$'Wild salmons'[j],digits = 2), "<br/>", "Farmed salmons: ", round(dataKvi()$'Farmed salmons'[j],digits = 2), sep="") %>%
              lapply(htmltools::HTML),
            labelOptions = labelOptions( style = list("font-weight" = "normal",  padding = "3px 8px"), textsize = "13px", direction = "auto")
          ) 
      }
      j <- j + 1
    }
  })
  
  ## Output map circles ---------------------------------
  # Teiknar upp reuðu hringina fyrir eldin
  observe({
    j <- 1
    leafletProxy("mymap") %>%
      clearGroup("eldi")
    for (i in dataKviMagn()) {
      if (i != 0) {
        leafletProxy("mymap") %>%
          addCircleMarkers(
            lng = farmsites[j, 3],
            lat = farmsites[j, 4],
            stroke = TRUE,
            fillColor = 'blue',
            fillOpacity = 1,
            color = 'black',
            weight = 1,
            radius = sqrt(i)*2,
            group = "eldi",
            label = farmsites[j,1],
            popup = paste(as.character(i), "thousand tons", sep = " ")
          )
      }
      j <- j + 1
    }
  })
  
  ## Output plot ---------------------------------
  # Teiknar upp súluritið, síar út tóm eldi og ár sem verða ekki fyrir áhrifum
  output$myPlot1 <- renderPlot({
    yHnit <- dataKvi()[1:amountOfRivers, as.integer(dataGraph())]
    yHnit <- yHnit[order(riverdata$"position")]
    xNofn <- riverdata$RiverName[order(riverdata$"position")]
    xNofn <- factor(xNofn, levels = unique(xNofn))
    if (input$all == 1) {
      influence <- which(yHnit != 0, arr.ind = T)
    }
    else {
      influence <- which(yHnit > -1, arr.ind = T)
    }
    if (length(influence) == 0) {
      influence = 0
    }
    if (input$radio == 1) {
      ggplot(data = dataKvi()[influence, ], aes(x = xNofn[influence], y = yHnit[influence])) + geom_bar(stat =
                                                                                              "identity") + xlab("Salmon river") + ylab("Amount of farmed salmons in river") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
    }
    else{
      amountOfVilltra <- dataKvi()[order(riverdata$"position"), 1]
      prosentur <- yHnit / (amountOfVilltra + yHnit) * 100
      ggplot(data = dataKvi()[influence, ], aes(x = xNofn[influence], y = prosentur[influence])) + geom_bar(stat =
                                                                                                  "identity", aes(fill = ifelse(prosentur[influence] < 4, "darkgreen", ifelse(prosentur[influence] < 10, "gold", "red3")))) + xlab("Salmon river") + ylab("Percentage of farmed salmons in river") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_hline(yintercept =  4, color = "gold") + geom_hline(yintercept =  10, color = "red3") +scale_fill_manual(values = c('darkgreen', 'gold','red3'))  + theme(legend.position =
                                                                                                                                                                                                                                                                                                                                                                                              "none")
    }
  })
  
  ## Output Weibull ---------------------------------
  # Teiknar weibull Distributionar Graphið og notar til þess weibb föllin
  output$myPlot2 <- renderPlot({
    if (as.integer(dataGraph2()) == amountOfEldis+2) {
      weibbS <- rep.int(0, amountOfRivers)
      weibbB <- rep.int(0, amountOfRivers)
      weibbSL <- rep.int(0, 2431)
      weibbBL <- rep.int(0, 2431)
      nums <- data.frame(c(-1215:1215))
      colnames(nums) = c('nums')
      for (i in c(1:amountOfEldis)) {
        weibbS = weibbS + weib(farmsites[i, 2], input$beta, input$eta)
        weibbB = weibbB + weib(farmsites[i, 2], input$beta2, input$eta2)
        weibbSL = weibbSL + weibL(farmsites[i, 2], input$beta, input$eta)
        weibbBL = weibbBL + weibL(farmsites[i, 2], input$beta2, input$eta2)
      }
      ggplot() + geom_point(data = riverdata,
                            aes(
                              x = position,
                              y = weibbS,
                              colour = "Early escapees"
                            )) +
        geom_line(data = nums, aes(
          x = nums,
          y = weibbSL,
          colour = "Early escapees"
        )) +
        geom_point(data = riverdata,
                   aes(
                     x = position,
                     y = weibbB,
                     colour = "Late escapees"
                   )) +
        geom_line(data = nums, aes(
          x = nums,
          y = weibbBL,
          colour = "Late escapees"
        )) +
        geom_point(data = riverdata,
                   aes(
                     x = position,
                     y = weibbB + weibbS,
                     colour = "Total escapees"
                   )) +
        geom_line(data = nums,
                  aes(
                    x = nums,
                    y = weibbBL + weibbSL,
                    colour = "Total escapees"
                  )) +
        xlab("River") + ylab("Percentage") + scale_x_discrete(breaks =
                                                                NULL)
    } else{
      stadsetning <- farmsites[as.integer(dataGraph2()) - 1, 2]
      nums <- data.frame(c(-1215:1215))
      colnames(nums) = c('nums')
      ggplot() + geom_point(data = riverdata,
                            aes(
                              x = position,
                              y = weib(stadsetning, input$beta, input$eta),
                              colour = "Early escapees"
                            )) +
        geom_line(data = nums, aes(
          x = nums,
          y = weibL(stadsetning, input$beta, input$eta),
          colour = "Early escapees"
        )) +
        geom_point(data = riverdata, aes(
          x = position,
          y = weib(stadsetning, input$beta2, input$eta2),
          colour = "Late escapees"
        )) +
        geom_line(data = nums, aes(
          x = nums,
          y = weibL(stadsetning, input$beta2, input$eta2),
          colour = "Late escapees"
        )) +
        geom_point(data = riverdata,
                   aes(
                     x = position,
                     y = weib(stadsetning, input$beta2, input$eta2) + weib(stadsetning, input$beta, input$eta),
                     colour = "Total escapees"
                   )) +
        geom_line(data = nums,
                  aes(
                    x = nums,
                    y = weibL(stadsetning, input$beta2, input$eta2) + weibL(stadsetning, input$beta, input$eta),
                    colour = "Total escapees"
                  )) +
        xlab("River") + ylab("Percentage") + scale_x_discrete(breaks =
                                                                NULL)
    }
  })
  
  ## Output table ---------------------------------
  # skrifar út töfluna
  output$table <- DT::renderDataTable({
    yHnit <- dataKvi()[1:(amountOfRivers+1), "Farmed salmons"]
    xHnit <- dataKvi()[(amountOfRivers+1), ]
    influenceX <- which(xHnit != 0, arr.ind = T)[, 2]
    if (sum(dataKvi()[1:amountOfRivers, "Farmed salmons"]) != 0) {
      influenceY <- which(yHnit != 0, arr.ind = T)
      df <- dataKvi()[influenceY, influenceX]
    } else{
      influenceY <- c(1:amountOfRivers)
      df  = data.frame(dataKvi()$"Wild salmons"[1:amountOfRivers], row.names = riverdata$RiverName)
      colnames(df) = "Wild salmons"
    }
    datatable(df,
              rownames = TRUE,
              filter = "none",
              width = "100%",
              options = list(scrollX = TRUE))%>%
              formatRound(TRUE, 1)
  })
  
 
  ## Distribution ---------------------------------
  Distribution <- function(arg1, beta, eta, annualProd, escapesPerTon, sexMat, seatime, crit, home){
    
    
    nums <- c(1:4200)
    hamark <- which.max((beta/eta)*((nums/eta)^(beta-1))*exp(-1*((nums/eta)^beta)))
    
    distance <- riverdata$position-arg1
    for(i in c(1:amountOfRivers)){
      if (distance[i] < -1215){
        distance[i] = 2430 + distance[i]
      }
      else if (distance[i] > 1215){
        distance[i] = - 2430 + distance[i]
      }
    }
    amendedDistance <- distance  + hamark
    amendedDistance[amountOfRivers+1] = hamark
    
    weibull <- ifelse(amendedDistance > 0, (beta/eta)*((amendedDistance/eta)^(beta-1))*exp(-1*((amendedDistance/eta)^beta)),0)
    heildarLikur <- sum(weibull)
    amountInRiver <- rep(0,amountOfRivers+1)
    for(i in c(1:amountOfRivers)){
      amountInRiver[i] = riverdata[i,"Stock.Size"]
    }
    amountInRiver[amountOfRivers+1] = annualProd*home
    weibullxStock <- weibull*amountInRiver/heildarLikur
    
    weibullxStockNormalized <- weibullxStock/sum(weibullxStock)
    
    FarmedFishInRiver <- annualProd*escapesPerTon*weibullxStockNormalized*15/18*sexMat*crit/seatime
    
    
    return(FarmedFishInRiver[1:amountOfRivers])
  }
  
  ## smoltDistribution ---------------------------------
  smoltDistribution <- function(arg1, beta, eta, annualProd, escapesPerTon,survival,home){
    
    nums <- c(1:4200)
    hamark <- which.max((beta/eta)*((nums/eta)^(beta-1))*exp(-1*((nums/eta)^beta)))
    
    distance <- riverdata$position-arg1
    for(i in c(1:amountOfRivers)){
      if (distance[i] < -1215){
        distance[i] = 2430 + distance[i]
      }
      else if (distance[i] > 1215){
        distance[i] = - 2430 + distance[i]
      }
    }
    amendedDistance <- distance  + hamark
    amendedDistance[amountOfRivers+1] = hamark
    
    weibull <- ifelse(amendedDistance > 0, (beta/eta)*((amendedDistance/eta)^(beta-1))*exp(-1*((amendedDistance/eta)^beta)),0)
    
    heildarLikur <- sum(weibull)
    
    amountInRiver <- rep(0,amountOfRivers+1)
    for(i in c(1:amountOfRivers)){
      amountInRiver[i] = riverdata[i,"Stock.Size"]
    }
    amountInRiver[amountOfRivers+1] = annualProd*home
    
    weibullxStock <- weibull*amountInRiver/heildarLikur
    
    weibullxStockNormalized <- weibullxStock/sum(weibullxStock)
    
    FarmedFishInRiver <- annualProd*escapesPerTon*weibullxStockNormalized*survival
    
    
    return(FarmedFishInRiver[1:amountOfRivers])
  }
  
  ## weib ---------------------------------
  weib<-function(arg1, beta, eta){
    
    nums <- c(1:4200)
    hamark <- which.max((beta/eta)*((nums/eta)^(beta-1))*exp(-1*((nums/eta)^beta)))
    
    
    distance <- riverdata$position-arg1
    for(i in c(1:amountOfRivers)){
      if (distance[i] < -1215){
        distance[i] = 2430 + distance[i]
      }
      else if (distance[i] > 1215){
        distance[i] = - 2430 + distance[i]
      }
    }
    
    amendedDistance <- distance  + hamark
    
    weibull <- ifelse(amendedDistance > 0, (beta/eta)*((amendedDistance/eta)^(beta-1))*exp(-1*((amendedDistance/eta)^beta)),0)
    return(weibull)
  }
  ## weib line ---------------------------------
  weibL <- function(arg1, beta, eta){
    
    nums <- c(1:4200)
    numsL <- c(-1215:1215)
    hamark <- which.max((beta/eta)*((nums/eta)^(beta-1))*exp(-1*((nums/eta)^beta)))
    
    
    distance <- numsL-arg1
    
    for(i in c(1:2431)){
      if (distance[i] < -1215){
        distance[i] = 2430 + distance[i]
      }
      else if (distance[i] > 1215){
        distance[i] = - 2430 + distance[i]
      }
    }
    
    amendedDistance <- distance  + hamark
    
    weibull <- ifelse(amendedDistance > 0, (beta/eta)*((amendedDistance/eta)^(beta-1))*exp(-1*((amendedDistance/eta)^beta)),0)
    return(weibull)
  }
  ## Save ------------------
  observeEvent(input$saver,{
    inputs <- reactiveValuesToList(input)
    s3save(inputs, object = "inputs.RData", bucket = s3BucketName)
  })
  
  
}

# runApp ---------------------------------
shinyApp(ui = ui, server = server)
