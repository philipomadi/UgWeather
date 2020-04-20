#load data
source('appPrep.R')

library(shiny)
library(htmltools)

library(raster)
library(dygraphs)
library(ggplot2)
library("formattable")
library(plotly)
library(viridis)
library(mapproj)

ui <- pageWithSidebar(
  
  titlePanel("Uganda Rainfall Analysis"),
  
  
  sidebarPanel(
    
    selectInput("yrs", strong("Select Rainfall Year"), c(1990:2018)),
    h3(textOutput(outputId = "yearRain", inline = TRUE), align="right"),
    plotOutput(outputId = "rainHistPlot", height = "150px"), 
    hr(),
    h4("Drought Risk Mapping"),
    sliderInput(inputId = "drydays_weight",
                label = "By Dry days (Zero Rain):",
                min = 1,
                max = 10,
                value = 5),
    sliderInput(inputId = "hotdays_weight",
                label = "By Hot Days (Max Temp > 29):",
                min = 1,
                max = 10,
                value = 5), 
    sliderInput(inputId = "veg_weight",
                label = "By Annual Rainfall Per SQKM:",
                min = 1,
                max = 10,
                value = 5), 
    hr(),
    tags$i(h6("Rainfall data from: ", tags$a(href="ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/ARC2", " ftp.cpc.ncep.noaa.gov"), " extrapolated with 0.05 degree res.")),
    tags$i(h6("Temperature data from: ", tags$a(href="ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_temp", "ftp.cdc.noaa.gov"), " extrapolated with 0.05 degree res.")),
    width=3
  ),
  
  mainPanel(
    
    
    dygraphOutput(outputId = "rain_ts", height='250px'),
    hr(),
    tabsetPanel(
      tabPanel("Rainfall Distribution", 
              br(), 
              plotOutput(outputId = "rainPlot", height="500px"),
              hr(),
              plotOutput(outputId = "rainboxplot") 
              ),
              
      tabPanel("Location Data",  formattableOutput(outputId = "distTable")),
      tabPanel("Drought Risk Mapping", plotlyOutput(outputId = "bubble_tab") ),
      tabPanel("Flood Risk Mapping", plotlyOutput(outputId = "bubble_tab2") )
      
    )
  
    
  )
  
)
  


server <- function(input,output){
  
  #show rain for selected year
  output$yearRain <- renderText({
    req(input$yrs)
    yrs <- as.numeric(input$yrs)
    yrData <- filter(arc2.by.year.summary, year %in% yrs)
    paste(yrs, "Avg Rain ", format(ceiling(as.numeric(yrData$means)), nsmall=0, big.mark=","), "ml") 
  })
  
  #plot showing total rainfall of previous 4 years 
  output$rainHistPlot <- renderPlot({
    
    req(input$yrs)
    
    yrs <- as.numeric(input$yrs)
    f_frm <- yrs-4
    plotData <- filter(arc2.by.year.summary, year %in% c(f_frm:yrs))
    
    ggplot(data=plotData, aes(year, means, group=1)) +
      geom_line(colour = "#408FA6", size=1) +
      geom_point()
    
  })
  
  
  #Display controls to Filter by location: District, county etc
  output$locationControls <- renderUI({
    
      if("Location" %in% input$datasrc){
        
        selectInput("district", strong("Select District(s) to Observe"), sort(unique(ubos_shp$DISTRICT)), multiple = TRUE)
        #selectInput("county", strong("County"), sort(unique(ubos_shp$COUNTY))),
        #selectInput("subcounty", strong("Sub County"), sort(unique(ubos_shp$SUBCOUNTY))),
        #selectInput("village", strong("Village"), sort(unique(ubos_shp$VILLAGE))),
        
      }
  })
  
  
  selectedYearDistrictData <- reactive({
    
    yrs <- as.character(input$yrs)
    ths.yr <- yrs[length(yrs)]
    
    dist.rf.tots <- filter(group_rf_tots_by_year, year == ths.yr)
    dist.rf.mean <- filter(group_rf_mean_by_year, year == ths.yr)
    dist.rf.zero <- filter(group_rf_zero_by_year, year == ths.yr)
    dist.tm.mean <- filter(group_tmax_mean_by_year, year == ths.yr)
    dist.tm.hots <- filter(group_tmax_hots_by_year, year == ths.yr)
    
    dist.rf.tots.yr <- gather(dist.rf.tots,year)
    dist.rf.mean.yr <- gather(dist.rf.mean,year)
    dist.rf.zero.yr <- gather(dist.rf.zero,year)
    dist.tm.mean.yr <- gather(dist.tm.mean,year)
    dist.tm.hots.yr <- gather(dist.tm.hots,year)
    
    rm(dist.rf.tots, dist.rf.mean, dist.rf.zero, dist.tm.mean, dist.tm.hots)
    
    names(dist.rf.tots.yr)<-c("DISTRICT","TOTAL_RAINFALL")
    names(dist.rf.mean.yr)<-c("DISTRICT","AVG_RAINFALL")
    names(dist.rf.zero.yr)<-c("DISTRICT","DRY_DAYS")
    names(dist.tm.mean.yr)<-c("DISTRICT","AVG_TMAX")
    names(dist.tm.hots.yr)<-c("DISTRICT","HOT_DAYS")
    
    rf.dist.yr <- left_join(ug_districts, dist.rf.tots.yr, by='DISTRICT') %>%  
      left_join(., dist.rf.mean.yr, by='DISTRICT') %>%  
      left_join(., dist.rf.zero.yr, by='DISTRICT') %>%   
      left_join(., dist.tm.mean.yr, by='DISTRICT') %>%   
      left_join(., dist.tm.hots.yr, by='DISTRICT') 
    
    rm(dist.rf.tots.yr, dist.rf.mean.yr, dist.rf.zero.yr, dist.tm.mean.yr, dist.tm.hots.yr)
    
    rf.dist.yr
    
  })
  
  
  #weather data by location (district) 
  output$distTable <- renderFormattable({
     
    rf.dist.yr <- selectedYearDistrictData()
    rf.dist.yr <- mutate(rf.dist.yr, RF_PER_SQKM=TOTAL_RAINFALL/AREA_SQKM)
    rf.dist.yr <- rf.dist.yr[,c(2,6,7,11,8,9,10)] 
    
     formattable(
       
       arrange(rf.dist.yr,desc(RF_PER_SQKM)), 
       align = c("l", rep("r", 7)),
       list(
         #`DISTRICT` = formatter("span", style = ~ formattable::style(color = "grey")),
         `TOTAL_RAINFALL` = color_bar("#9cc2e5"),
         `RF_PER_SQKM` = color_bar("#9cc2e5"),
         `HOT_DAYS` = color_tile("#ffffff", "#FA614B")
       )
       
     )
     
     
   })
  
  #plot seasonal distribution 
  output$rainPlot <- renderPlot({
    
    req(input$yrs)
    
    yrs <- as.character(input$yrs)
    ths.yr <- yrs[length(yrs)]
    
    #get matching cols from seasonal data
    cols.ths.yr <-  arc2.by.season[,grepl( ths.yr , names( arc2.by.season ))]
    colnames(cols.ths.yr) <- c("DJF","MAM","JJA","SON")
    cols.ths.yr <- cbind(arc2.by.season[,c("X","Y")],cols.ths.yr)
    
    par(mfrow=c(2,2), mai=c(0.2,0.2,0.5,0.2))
    djf <- rasterFromXYZ(cols.ths.yr[,c("X","Y","DJF")], digits=2)
    plot(djf, alpha=0.8, main="JFD", axes=FALSE, box=F)
    plot(ug_shp, add=T)
    
    mam <- rasterFromXYZ(cols.ths.yr[,c("X","Y","MAM")], digits=2)
    plot(mam, alpha=0.8, main="MAM", axes=FALSE, box=F, legend=F)
    plot(ug_shp, add=T)
    
    jja <- rasterFromXYZ(cols.ths.yr[,c("X","Y","JJA")], digits=2)
    plot(jja, alpha=0.8, main="JJA", axes=FALSE, box=F, legend=F)
    plot(ug_shp, add=T)
    
    son <- rasterFromXYZ(cols.ths.yr[,c("X","Y","SON")], digits=2)
    plot(son, alpha=0.8, main="SON", axes=FALSE, box=F, legend=F)
    plot(ug_shp, add=T)
    
    title(paste(ths.yr," Seasonal Rainfall Distribution") , line = -1, outer = TRUE)
    
    
    
  })
  
  #plot daily timeseries
  output$rain_ts <- renderDygraph({
    
    req(input$yrs)
    
    yrs <- as.character(input$yrs)
    last_yr <- yrs[length(yrs)]
    strt_yr <- paste(last_yr,"1","1", sep="-")
    end_yr <- paste(last_yr,"12","31", sep="-")
    
    dygraph(dy_ts, main = "Total daily rainfall") %>% dyRangeSelector() %>% dyShading(from=strt_yr, to=end_yr, color="#FFE6E6")
  
  })
  
  #rainfall monthly boxplot
  output$rainboxplot <- renderPlot({
    
    req(input$yrs)
    
    last_yr <- as.character(input$yrs)
    cn5B <-  arc2.by.month[,grepl( last_yr , names( arc2.by.month ))]

    colnames(cn5B) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    
    boxplot(cn5B, 
            main=paste(last_yr,"Monthly Rainfall Distribution"), 
            xlab="Month",
            ylab="Rainfall per 10Sq KM (ml)", 
            col="#75AADB" )
  })
  
  
  
  #drought risk mapping
  output$bubble_tab <- renderPlotly({
    
    wt.dry.days <- input$drydays_weight
    wt.hot.days <- input$hotdays_weight
    wt.veg <- input$veg_weight
    
    risk_data <- selectedYearDistrictData()
    risk_data <- mutate(risk_data, RF_PER_SQKM=TOTAL_RAINFALL/AREA_SQKM)
    risk_data <- mutate(risk_data, DRY_FN=DRY_DAYS-182)
    risk_data$DRY_FN <- ifelse(risk_data$DRY_FN < 0, 0, risk_data$DRY_FN)
    risk_data <- mutate(risk_data, HOT_FN=HOT_DAYS-182)
    risk_data$HOT_FN <- ifelse(risk_data$HOT_FN < 0, 0, risk_data$HOT_FN)
    risk_data <- mutate(risk_data, RAIN_FN=RF_PER_SQKM-10)
    risk_data$RAIN_FN <- ifelse(risk_data$RAIN_FN >= 0, 0, risk_data$RAIN_FN*-1)
    risk_data <- mutate(risk_data, RISK_FN=(wt.dry.days*DRY_FN-(10-wt.dry.days))+(wt.hot.days*HOT_FN-(10-wt.hot.days))+(wt.veg*RAIN_FN-(10-wt.veg)))
    risk_data$RISK_FN <- ifelse(risk_data$RISK_FN < 0, 0, risk_data$RISK_FN)
    
    risk_data <- risk_data[,c(2,3,4,15)] 
    
    plot_data=risk_data %>%
      arrange(RISK_FN) %>%
      mutate( remarks=paste("District: ", DISTRICT, "\n", "Risk Factor: ", round(RISK_FN), sep="")) %>%  
      
      ggplot() +
      geom_polygon(data = UG, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
      geom_point(aes(x=long, y=lat, size=RISK_FN, color=RISK_FN, text=remarks, alpha=RISK_FN) ) +
      scale_size_continuous(range=c(1,15)) +
      scale_color_viridis(option="inferno", trans="log" ) +
      scale_alpha_continuous(trans="log") +
      theme_void() +
      ylim(-2,5) +
      coord_map() +
      theme(legend.position = "none")
    
    ggplotly(plot_data, tooltip="text")
    
  })
  
  
}
shinyApp(ui=ui,server=server)
