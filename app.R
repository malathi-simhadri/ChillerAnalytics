library(shinydashboard)
library(plotly)
library(dplyr)
library(sqldf)
library(plyr)
library(readr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(DT)
library(dygraphs)
library(leaflet)
library(xts)
library(earth)
library(randomForest)
library(forecast)
library(data.table)
inp_data = read.csv("Chiller_metadata.csv")

data1<-read.csv("India.csv")

dbHeader <- dashboardHeader(titleWidth = "1px")
files<-list.files(pattern = "_data.csv")
c_param<-read.csv("ChillerParameters.csv")
c_param<-c_param[-(1:2),]
dbHeader$children[[2]]$children[[1]] <- tags$p("Chiller")

customHtml <- withTags({
  
  div(
      h3(style="width:1200px;height:68px;margin-top: 10px;margin-bottom: 10px;padding-top:0px;padding-bottom:0px;color:white;text-align: center;float:center;font-family:Arial Rounded MT Bold;font-size:200%","Chiller Data Analysis"),
      style="width:1500px;height:58px;margin-top: 0px;padding-top:10px;color:white;text-align: center;"
      
  )
})
dbHeader$children[[3]]$children[[3]] <- customHtml
dbHeader$Title<-"Chiller"

ui <- dashboardPage(
  
      dbHeader,
      dashboardSidebar(
      disable = T
      ),
      dashboardBody(
      tags$head(tags$style(HTML('
                /* logo */
                .skin-blue .main-header .logo {
                 background-color: #00465B;
                         }
                              
                /* logo when hovered */
                .skin-blue .main-header .logo:hover {
                 background-color: #00465B;
                         }
                              
                 /* navbar (rest of the header) */
                .skin-blue .main-header .navbar {
                 background-color: #00465B;
                         }  
                              
                  '))),
    
    tags$style(HTML('.nav-tabs {
                    background-color: #FFF;
                    }
                    
                    .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                    background-color: transparent;
                    border-color: transparent;
                    }
                    
                    .nav-tabs-custom .nav-tabs li.active {
                    background-color: #BFD738;
                    border-top-color: #FFF;
                    foreground-color: #FFF;
                    }
                    .box.box-solid.box-primary>.box-body {
                    color:#fff;
                    background-color:#00465B;
                    }
                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background-color:#00465B;
                    }
                    
                    .box.box-solid.box-primary{
                    border-bottom-color:#666666;
                    border-left-color:#666666;
                    border-right-color:#666666;
                    border-top-color:#666666;
                    }
                    
                    table.dataTable.tbody { background-color: #00465B;} 
                    .dataTables_length {
                    color: black;
                    }
                    tags$head(tags$style(HTML(".small-box {height: 30px}"))),
                    .dataTables_filter {
                    color: #FFF;
                    }
                    
                    ')),
    
    
    tabBox(
            title = "",width = "100%",side = "left",
                tabPanel( "Main                                    ", width = "50%",
                     fluidRow(
                                  
                                  valueBoxOutput("ChillerLoc", width=3),
                                  valueBoxOutput("Chillers", width=3),
                                  valueBoxOutput("ChillerMakes", width=3),
                                  valueBoxOutput("ChillerType", width=3)
                                # )
                            ),
                     
                    fluidRow(
                             box(status = "primary",solidHeader = TRUE,title="Development Centers", leafletOutput("mymap"),width=3),
                             box(status = "primary", solidHeader = TRUE,
                                           selectInput("param", "Choose an Option to display details:", c("ChillerLoc","Chillers","ChillerMakes", 
                                                       "ChillerType"), width = "100%"),
                                          plotlyOutput("plot1",height = "360px",width = "100%"), width = 6),
                             box(width = 3, status = "primary",solidHeader = TRUE,title="Chiller Schematic",uiOutput("img1"))
                            )
                     ),
            
            tabPanel(    "Correlation Analysis               ",
                         fluidRow(
                           box(status = "primary", solidHeader = F, width = 6,
                               selectInput("Loc1", "Choose a Location:",gsub(".csv","",files),selected = 1, width = "100%"),
                               box(status = "primary", solidHeader = F,width = 12,title = "Chiller Parameter Info.", dataTableOutput("cinfo"))
                           ),
                           
                           # box(status = "primary", solidHeader = F, width = 6,
                           
                           box(status = "primary", solidHeader = TRUE, width=6,   
                               plotOutput("plot41",height = "500px",width = "100%"))
                           
                           
                         )
                         
                         
                         
                         
                         
            ),
            
              
                tabPanel(    "Exploratory Analysis ",
                      fluidRow(
                               box(status = "primary", solidHeader = TRUE, width = 6,
                                            selectInput("Loc", "Choose a Chiller:",gsub(".csv","",files),selected = 1, width = "100%")),
                               box(status = "primary", solidHeader = TRUE, width = 6,
                                   selectInput("cparam", "Choose a Parameter:",c_param$Description,selected = 1, width = "100%")),
                               box(status = "primary", solidHeader = F, width = 12,
                                   dygraphOutput("plot_mp",width = "100%",height = "400"))
                   
                              )
                   
                     ),
            
      
             tabPanel(    "Diagnostics Model Analysis",
                   fluidRow(
                            box(status = "primary", solidHeader = TRUE, width = 6,height = "50",
                                          selectInput("Locm", "Choose a Location:",gsub(".csv","",files),selected = 1, width = "100%")),
                            box(status = "primary", solidHeader = TRUE, width = 6,height = "50" ,radioButtons("Model", "Select a Model::",
                                          c("Multivariate Linear Regression"="lm", "MARS"="earth","Random Forest"="rf"),inline = TRUE)),
                            box(status = "primary", solidHeader = FALSE, width = 6,
                                          dygraphOutput("plot_md1",width = "100%",height = "350")),
                            box(status = "primary", solidHeader = FALSE, width = 6,
                                          dygraphOutput("plot_md2",width = "100%",height = "350"))
                           
                          )
                   
                     ),
            
              tabPanel(    "Prognostics Model Analysis",
                    fluidRow(
                             box(status = "primary", solidHeader = TRUE, width = 4,height = "50",
                                           selectInput("Locmp", "Choose a Location:",gsub(".csv","",files),selected = 1, width = "100%")),
                             box(status = "primary", solidHeader = TRUE, width = 5,height = "50" ,radioButtons("Modelp", "Select a Model::", c(" Auto Arima"="Arima","Neural Networks Auto Regression"="nn", "Exponential Smoothing"="ETS"),inline = TRUE)),
                             box(status = "primary", solidHeader = TRUE, width = 3,height = "50" ,radioButtons("Modelpm", "Select a Parameter::", c("ikwtr", "CDWSTD","CHWSTD"),inline = TRUE)),
                             box(status = "primary", solidHeader = FALSE, width = 12,
                                          dygraphOutput("plot_mdp1",width = "100%",height = "350"))
                            )
                   
                      )

      
    )
    
     ))

#_______________________________________________________________________________________

server <- function(input, output, session) {
  
  
  output$cinfo <- renderDataTable({
    datatable(c_param[,c(2:ncol(c_param))],options = list(iDisplayLength = 6))
    
    
  })
  
  output$plot_mdp1 <- renderDygraph({ 
    withProgress({
    pm<-input$Modelpm
    ds1<-read.csv(paste0(input$Locmp,".csv")) 
    #print(head(ds1))
    g1c1<-ds1[(ds1$Val.instantikwtr<=0.6 )& (ds1$Val.condapproach<=4.5) & (ds1$Val.evapapproach<=4.5),]
    mp<-input$Modelp
 
    g1c1$date1 <- as.Date(g1c1$V11)
  
    #print(head(g1c1$date1))
    alldates <- data.table(date1=seq.Date(min(g1c1$date1), max(g1c1$date1), by="day"))
  
    dt <- merge(g1c1, alldates, by="date1", all=TRUE)
    dt$Val.condapproach<-na.approx(dt$Val.condapproach)
    dt$Val.evapapproach<-na.approx(dt$Val.evapapproach)
    dt$Val.instantikwtr<-na.approx(dt$Val.instantikwtr)
    
    gec1_c3_actual <- dt
    pointstoforecast<-30 
    sdate<- min(g1c1$date1)
    edate<- max(g1c1$date1)-pointstoforecast
    nonevntcdwdata<-subset(gec1_c3_actual,as.Date(gec1_c3_actual$date1)>=  sdate &as.Date(gec1_c3_actual$date1)<=  edate)
    
    actualdata<- dt #subset(gec1_c3_actual,gec1_c3_actual$X.1>=min(nonevntcdwdata$X.1) &gec1_c3_actual$X.1<max(nonevntcdwdata$X.1)+pointstoforecast)
    param<-nonevntcdwdata$Val.instantikwtr
    if(pm=="CDWSTD")
    {
      param<-nonevntcdwdata$Val.condapproach
      p<-"Val.condapproach"
    }
    if(pm=="CHWSTD")
    {
      param<-nonevntcdwdata$Val.evapapproach
      p<-"Val.evapapproach"
    }
    
    if(pm=="ikwtr")
    {
      param<-nonevntcdwdata$Val.instantikwtr
      p<-"Val.instantikwtr"
    } 
    if(mp=="Arima"){
      y<-auto.arima(param)}
    if(mp=="nn"){
       y<-nnetar(param)
      
    }
    if(mp=="ETS"){
       y<-ets(param)}
    fcastpoints<-as.data.frame(forecast(y,h=30))
    fcastpoints$date1<-seq.Date(edate, max(g1c1$date1)-1, by="day")
    finaldt <- merge(actualdata, fcastpoints, by="date1", all=TRUE)
    #print(head(finaldt))
    if(mp=="nn")
    {htmobj<-xts(finaldt[,c("date1",p,"Point Forecast")],as.POSIXct(finaldt$date1, format="%Y -%m -%d", tz="UTC", label="Date1"))}
    else {
      htmobj<-xts(finaldt[,c("date1",p,"Point Forecast","Lo 80","Hi 80","Lo 95","Hi 95")],as.POSIXct(finaldt$date1, format="%Y -%m -%d", tz="UTC", label="Date1"))
    }
    dygraph(htmobj,main= paste(mp, " on ", pm," for ",input$Locmp),ylab=pm ) %>%
      
      dyEvent("2014-8-26", "Chiller-1 condenser and evaporator STD is high", labelLoc = "bottom",color="red")  %>%
      dyEvent("2014-12-3", "Chiller-1 refrigerant shortage.", labelLoc = "bottom",color="red")  %>%
     
      dyOptions(axisLineWidth = 2,fillGraph = TRUE,drawGrid = FALSE)%>%
      dyRoller(rollPeriod = 1) %>%
      dyRangeSelector(height = 10) 
   })
  })
  
  
  output$plot_md1 <- renderDygraph({ 
    mdl<-input$Model
    #ds1<-read.csv(input$Locm)  
    ds1<-read.csv(paste0(input$Locm,".csv"))
    gec1_c1<-ds1[(ds1$Val.instantikwtr<=0.6 )& (ds1$Val.condapproach<=4.5) & (ds1$Val.evapapproach<=4.5),]
    if(mdl=="lm"){
      g1c1reg<-lm(Val.instantikwtr~Val.condsattemp+Val.evapsattemp+Val.compressordischargetemp+Val.lchwt
                  +Val.rchwt+Val.lcdwt
                  +Val.rcdwt
                  +Val.cteffectiveness
                  +Val.wetbulbtemp
                  +Val.outsidetemp,data=gec1_c1)
      coeffs = coefficients(g1c1reg)
      coeffs 
      gec1_c1_actual <- ds1
 
      n=length(coeffs)
      
      newg1c1actual= coeffs[1]+coeffs[2]*gec1_c1_actual$Val.condsattemp+coeffs[3]*gec1_c1_actual$Val.evapsattemp+coeffs[4]*gec1_c1_actual$Val.compressordischargetemp+coeffs[5]*gec1_c1_actual$Val.lchwt+
        coeffs[6]*gec1_c1_actual$Val.rchwt+coeffs[7]*gec1_c1_actual$Val.lcdwt+coeffs[8]*gec1_c1_actual$Val.rcdwt+coeffs[9]*gec1_c1_actual$Val.cteffectiveness+coeffs[10]*gec1_c1_actual$Val.wetbulbtemp+
        coeffs[11]*gec1_c1_actual$Val.outsidetemp
      
      newg1c1good= coeffs[1]+coeffs[2]*gec1_c1$Val.condsattemp+coeffs[3]*gec1_c1$Val.evapsattemp+coeffs[4]*gec1_c1$Val.compressordischargetemp+coeffs[5]*gec1_c1$Val.lchwt+
        coeffs[6]*gec1_c1$Val.rchwt+coeffs[7]*gec1_c1$Val.lcdwt+coeffs[8]*gec1_c1$Val.rcdwt+coeffs[9]*gec1_c1$Val.cteffectiveness+coeffs[10]*gec1_c1$Val.wetbulbtemp+
        coeffs[11]*gec1_c1$Val.outsidetemp
      
      gec1_c1$calc_goodikw<-newg1c1good
      gec1_c1$goodikwdevpct<-((gec1_c1$Val.instantikwtr-gec1_c1$calc_goodikw)/gec1_c1$Val.instantikwtr)*100
      gooddata<-gec1_c1[,c("V11","Val.instantikwtr","calc_goodikw","goodikwdevpct")]
      
      g1both<-merge(gec1_c1_actual,gooddata,by="V11",all.x = TRUE)
      names(g1both)
      g1both$calc_actualikw<-newg1c1actual
      g1both$calc_actualdevpcent<-((g1both$Val.instantikwtr.x- g1both$calc_actualikw)/g1both$Val.instantikwtr.x)*100
    }
    if(mdl=="rf"){
      
      g1c1reg<-randomForest(Val.instantikwtr~Val.condsattemp+Val.evapsattemp+Val.compressordischargetemp+Val.lchwt
                            +Val.rchwt+Val.lcdwt
                            +Val.rcdwt
                            +Val.cteffectiveness
                            +Val.wetbulbtemp
                            +Val.outsidetemp,data=gec1_c1,degree=2)
      
      coeffs = coefficients(g1c1reg)
      coeffs 
      
      y<-predict(g1c1reg,newdata=gec1_c1,n.ahead=10,prediction.interval=TRUE)
      gec1_c1_actual <- ds1
      n=length(coeffs)
      actual_y<-predict(g1c1reg,newdata=gec1_c1_actual)
      
      
      gec1_c1$calc_goodikw<-y
      gec1_c1$goodikwdevpct<-((gec1_c1$Val.instantikwtr-gec1_c1$calc_goodikw)/gec1_c1$Val.instantikwtr)*100
      gooddata<-gec1_c1[,c("V11","Val.instantikwtr","calc_goodikw","goodikwdevpct")]
      
      g1both<-merge(gec1_c1_actual,gooddata,by="V11",all.x = TRUE)
      g1both$calc_actualikw<-actual_y
      g1both$calc_actualdevpcent<-((g1both$Val.instantikwtr.x- g1both$calc_actualikw)/g1both$Val.instantikwtr.x)*100
    }
    if(mdl=="earth"){
      
      g1c1reg<-earth(Val.instantikwtr~Val.condsattemp+Val.evapsattemp+Val.compressordischargetemp+Val.lchwt
                     +Val.rchwt+Val.lcdwt
                     +Val.rcdwt
                     +Val.cteffectiveness
                     +Val.wetbulbtemp
                     +Val.outsidetemp,data=gec1_c1,degree=2)
      coeffs = coefficients(g1c1reg)
      coeffs 
      
      y<-predict(g1c1reg,newdata=gec1_c1)
      gec1_c1_actual <- ds1
      n=length(coeffs)
      actual_y<-predict(g1c1reg,newdata=gec1_c1_actual)
      
      gec1_c1$calc_goodikw<-y
      gec1_c1$goodikwdevpct<-((gec1_c1$Val.instantikwtr-gec1_c1$calc_goodikw)/gec1_c1$Val.instantikwtr)*100
  
      gooddata<-gec1_c1[,c("V11","Val.instantikwtr","calc_goodikw","goodikwdevpct")]
      
      
      g1both<-merge(gec1_c1_actual,gooddata,by="V11",all.x = TRUE)
      
      
      g1both$calc_actualikw<-actual_y
      g1both$calc_actualdevpcent<-((g1both$Val.instantikwtr.x- g1both$calc_actualikw)/g1both$Val.instantikwtr.x)*100
    }
    
    cname<-input$Locm
    
    g1rlm<-xts(g1both[,c("V11","Val.instantikwtr.x","calc_actualikw")],as.POSIXct(g1both$V11, format="%Y-%m-%d", tz="UTC", label="V11"))
    
    dygraph(g1rlm,main="ikW/TR - Actual vs calculated by time ",ylab = "ikW/TR")  %>%
      dySeries( name="Val.instantikwtr.x", stepPlot = FALSE ,strokeWidth = 3,strokePattern = "dotted",label="Actual ikW/TR") %>%
      dySeries( name="calc_actualikw",label="Calculated ikW/TR", strokeWidth = 2, strokePattern = "dashed") %>%
      dyEvent("2014-8-26", "Chiller-1 condenser and evaporator STD is high", labelLoc = "bottom",color="red")  %>%
      dyEvent("2014-12-3", "Chiller-1 refrigerant shortage.", labelLoc = "bottom",color="red")  %>%
      dyOptions(axisLineWidth = 2,fillGraph = FALSE,drawGrid = FALSE)%>%
      dyRoller(rollPeriod = 1) %>%
      dyRangeSelector(height = 10)
    
  })
  
  
  output$plot_md2 <- renderDygraph({ 
    mdl<-input$Model
    #ds1<-read.csv(input$Locm) 
    ds1<-read.csv(paste0(input$Locm,".csv"))
    gec1_c1<-ds1[(ds1$Val.instantikwtr<=0.6 )& (ds1$Val.condapproach<=4.5) & (ds1$Val.evapapproach<=4.5),]
    
    if(mdl=="lm"){
      g1c1reg<-lm(Val.instantikwtr~Val.condsattemp+Val.evapsattemp+Val.compressordischargetemp+Val.lchwt
                  +Val.rchwt+Val.lcdwt
                  +Val.rcdwt
                  +Val.cteffectiveness
                  +Val.wetbulbtemp
                  +Val.outsidetemp,data=gec1_c1)
      coeffs = coefficients(g1c1reg)
      coeffs 
      gec1_c1_actual <- ds1
      n=length(coeffs)

      newg1c1actual= coeffs[1]+coeffs[2]*gec1_c1_actual$Val.condsattemp+coeffs[3]*gec1_c1_actual$Val.evapsattemp+coeffs[4]*gec1_c1_actual$Val.compressordischargetemp+coeffs[5]*gec1_c1_actual$Val.lchwt+
        coeffs[6]*gec1_c1_actual$Val.rchwt+coeffs[7]*gec1_c1_actual$Val.lcdwt+coeffs[8]*gec1_c1_actual$Val.rcdwt+coeffs[9]*gec1_c1_actual$Val.cteffectiveness+coeffs[10]*gec1_c1_actual$Val.wetbulbtemp+
        coeffs[11]*gec1_c1_actual$Val.outsidetemp
      
      newg1c1good= coeffs[1]+coeffs[2]*gec1_c1$Val.condsattemp+coeffs[3]*gec1_c1$Val.evapsattemp+coeffs[4]*gec1_c1$Val.compressordischargetemp+coeffs[5]*gec1_c1$Val.lchwt+
        coeffs[6]*gec1_c1$Val.rchwt+coeffs[7]*gec1_c1$Val.lcdwt+coeffs[8]*gec1_c1$Val.rcdwt+coeffs[9]*gec1_c1$Val.cteffectiveness+coeffs[10]*gec1_c1$Val.wetbulbtemp+
        coeffs[11]*gec1_c1$Val.outsidetemp
      
      gec1_c1$calc_goodikw<-newg1c1good
      gec1_c1$goodikwdevpct<-((gec1_c1$Val.instantikwtr-gec1_c1$calc_goodikw)/gec1_c1$Val.instantikwtr)*100
    
      gooddata<-gec1_c1[,c("V11","Val.instantikwtr","calc_goodikw","goodikwdevpct")]

      g1both<-merge(gec1_c1_actual,gooddata,by="V11",all.x = TRUE)
      
      g1both$calc_actualikw<-newg1c1actual
      g1both$calc_actualdevpcent<-((g1both$Val.instantikwtr.x- g1both$calc_actualikw)/g1both$Val.instantikwtr.x)*100
    }
    if(mdl=="earth"){
      
      g1c1reg<-earth(Val.instantikwtr~Val.condsattemp+Val.evapsattemp+Val.compressordischargetemp+Val.lchwt
                     +Val.rchwt+Val.lcdwt
                     +Val.rcdwt
                     +Val.cteffectiveness
                     +Val.wetbulbtemp
                     +Val.outsidetemp,data=gec1_c1,degree=2)
      coeffs = coefficients(g1c1reg)
      coeffs 
      
      y<-predict(g1c1reg,newdata=gec1_c1)
      gec1_c1_actual <- ds1

      n=length(coeffs)
      actual_y<-predict(g1c1reg,newdata=gec1_c1_actual)
      
      gec1_c1$calc_goodikw<-y
      gec1_c1$goodikwdevpct<-((gec1_c1$Val.instantikwtr-gec1_c1$calc_goodikw)/gec1_c1$Val.instantikwtr)*100
  
      gooddata<-gec1_c1[,c("V11","Val.instantikwtr","calc_goodikw","goodikwdevpct")]
      
      
      g1both<-merge(gec1_c1_actual,gooddata,by="V11",all.x = TRUE)
      
      
      g1both$calc_actualikw<-actual_y
      g1both$calc_actualdevpcent<-((g1both$Val.instantikwtr.x- g1both$calc_actualikw)/g1both$Val.instantikwtr.x)*100
    }
    if(mdl=="rf"){
      
      g1c1reg<-randomForest(Val.instantikwtr~Val.condsattemp+Val.evapsattemp+Val.compressordischargetemp+Val.lchwt
                            +Val.rchwt+Val.lcdwt
                            +Val.rcdwt
                            +Val.cteffectiveness
                            +Val.wetbulbtemp
                            +Val.outsidetemp,data=gec1_c1,degree=2)
      coeffs = coefficients(g1c1reg)
      coeffs 
      
      y<-predict(g1c1reg,newdata=gec1_c1,n.ahead=10,prediction.interval=TRUE)
      gec1_c1_actual <- ds1

      n=length(coeffs)
      actual_y<-predict(g1c1reg,newdata=gec1_c1_actual)
      
      
      gec1_c1$calc_goodikw<-y
      gec1_c1$goodikwdevpct<-((gec1_c1$Val.instantikwtr-gec1_c1$calc_goodikw)/gec1_c1$Val.instantikwtr)*100

      gooddata<-gec1_c1[,c("V11","Val.instantikwtr","calc_goodikw","goodikwdevpct")]
      
      g1both<-merge(gec1_c1_actual,gooddata,by="V11",all.x = TRUE)
      g1both$calc_actualikw<-actual_y
      g1both$calc_actualdevpcent<-((g1both$Val.instantikwtr.x- g1both$calc_actualikw)/g1both$Val.instantikwtr.x)*100
    }

    cname<-input$Locm
    g1rlm<-xts(g1both[,c("V11","calc_actualdevpcent")],as.POSIXct(g1both$V11, format="%Y-%m-%d", tz="UTC", label="V11"))
    
    dygraph(g1rlm,main=" ikW/TR Deviation Percent by time ",ylab = "%Deviation")  %>%
      dySeries( name="calc_actualdevpcent", stepPlot = FALSE ,strokeWidth = 3,strokePattern = "dotted",label="%Deviation") %>%
     
      dyEvent("2014-8-26", "Chiller-1 condenser and evaporator STD is high", labelLoc = "bottom",color="red")  %>%
      dyEvent("2014-12-3", "Chiller-1 refrigerant shortage.", labelLoc = "bottom",color="red")  %>%
      dyOptions(axisLineWidth = 2,fillGraph = FALSE,drawGrid = FALSE)%>%
      dyRoller(rollPeriod = 1) %>%
      dyRangeSelector(height = 10)
    
  })
  
 
  output$plot_mp <- renderDygraph({ 
    
    #ds1<-read.csv(input$Loc) 
    ds1<-read.csv(paste0(input$Loc,".csv"))
    #print(names(ds1))
    names(ds1)<-c("x","V11","CDWSTD","CHWSTD","IKWTR","CondSatTemp","EvapSatTemp","CompDischargeTemp","LCHWT","RCHWT","LCDWT","RCDWT","CTEffectiveness","WerBulbTemp","OutsideTemp")
    #cp<-names(ds1)
    #print(head(ds1))
    
    cp<-trimws(c_param[c_param$Description==input$cparam,c("ShortName")])
   # print(cp)
    
    cname<-input$Loc
    stds<-na.omit(ds1[,c(cp)])
   # print(names(stds))
   # print(stds)
    mean1<-mean(stds)
    #print(mean1)
    sd1<-sd(stds)
    ucl<-mean1+3*sd1
    lcl<-mean1-3*sd1
    #print(ucl)
    xtsobjgc2<-xts(ds1[,c(cp)],as.POSIXct(ds1$V11, format="%Y-%m-%d"))
    dygraph(xtsobjgc2, main= paste(cp,"  by time for", cname),ylab=cp)  %>%
      dyOptions(axisLineWidth = 2,fillGraph = TRUE,drawGrid = FALSE)%>%
      dyLimit(mean1,paste("Mean=",mean1), color="green")%>%
      dyLimit(ucl,paste("UCL(Mean+3SD=",ucl), color="red")%>%
      dyLimit(lcl,paste("LCL(Mean-3SD)=",lcl), color="red")%>%
      dyEvent("2014-8-26", "Chiller-1 condenser and evaporator STD is high", labelLoc = "bottom",color="black")  %>%
      dyEvent("2014-12-3", "Chiller-1 refrigerant shortage.", labelLoc = "bottom",color="black")  %>%
      dyRangeSelector(height = 10)
  })
   

  output$mymap <- renderLeaflet({
    
    p<-   leaflet() %>%
      addMarkers(data = data1,
                 lng=~data1$Longitude,
                 lat=~data1$Latitude,
                 popup=~data1$Place) %>%
      addTiles()
    p
  })
  output$plot1 <- renderPlotly({
    
    inpParam<-input$param
    if (inpParam == "ChillerLoc")
    {
      inpParam="Location"
    }
    if (inpParam == "ChillerMakes")
    {
      inpParam="Make"
    }
    if (inpParam == "ChillerType")
    {
      inpParam="Type"
    }
    if (inpParam == "ChillerCapacity")
    {
      inpParam="Capacity"
    }
    if (inpParam == "Chillers")
    {
      inpParam="BuildingType"
    }
    temp = inp_data[,inpParam]
    temp = data.frame(temp)
    names(temp) = c("v1")
    temp$txt = inp_data$AssetName
    
    p <- plot_ly(dplyr::count(temp,v1), labels = ~v1, values = ~n, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste(v1),
                 marker = list(colors = c("green","rgb(211,94,96)"),
                               line = list(color = '#FFFFFF', width = 1)),
                 #The 'pull' attribute can also be used to create space between the sectors
                 showlegend = T) %>%
      layout(title = paste(inpParam),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%config(displayModeBar = F)
    
  })
  
  
  output$ChillerLoc <- renderValueBox({
    col="fuchsia"
    if (input$param == "ChillerLocations")
      col="green"    
    valueBox(length(unique(inp_data$Location)), "DCs", icon = icon("city"),color = col)
  })
  
  output$Chillers <- renderValueBox({
    col="yellow"
    if (input$param == "Chillers")
      col="green"    
    valueBox(nrow(inp_data), "Total Chillers", icon = icon("cogs"),color = col)
  })  
  
  output$ChillerMakes <- renderValueBox({
    col="purple"
    if (input$param == "ChillerMakes")
      col="green"    
    #valueBox(length(unique(inp_data$Make)), "Makes", icon = icon("copyright",class ="fa-pulse"),color = col)
    valueBox(length(unique(inp_data$Make)), "Makes", icon = icon("copyright"),color = col)
  })
  
  output$ChillerType <- renderValueBox({
    col="aqua"
    if (input$param == "ChillerType")
      col="green"    
    valueBox(length(unique(inp_data$Type)), "Chiller Types", icon = icon("industry"),color = col)
  })
  
 
  #Correlations
  output$plot41 <- renderPlot({
    ds1<-read.csv(paste0(input$Loc1,".csv"))
    newmaster<-na.omit(ds1)
    names(newmaster)<-gsub("Val.","",names(newmaster))
    names(newmaster)<-c("x","V11","CDWSTD","CHWSTD","IKWTR","CondSatTemp","EvapSatTemp","CompDischargeTemp","LCHWT","RCHWT","LCDWT","RCDWT","CTEffectiveness","WerBulbTemp","OutsideTemp")
    M<-cor(newmaster[3:ncol(newmaster)])
   
    col<- colorRampPalette(c("red", "white", "blue"))(20)
   
     corrplot(M, method="circle",insig="blank")
  })
  output$img1<-renderUI({
   img(src="Chiller_schematic1.png",width="300px",height="400px")
  })
  
}

shinyApp(ui, server)