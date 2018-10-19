library(rgdal)
library(raster)
library(stringr)
library(leaflet)
library(plotly)
library(rhandsontable)
library(shinyjs)
library(mapview)
library(gstat)
library(sp)
library(shinycssloaders)
library(zip)
library(plotKML)
library(rgeos)
library(rasterVis)
library(data.table)
library(ithir)
library(ranger)

source('DSMToolsConfig.R')
source('Utils/GeneralUtils.R')
source('Utils/VectorUtils.R')
source('Utils/ModelUtils.R')



######################################   UI   ###################################################
ui <- tagList(fluidPage(
  
  tags$head(tags$link( rel="icon", type="image/png", href="favicon-32x32.png", sizes="32x32" ),
              tags$head(tags$script(src = "message-handler.js")),
              tags$title("DSM Tools"),
              HTML("<img src=soilcores2.png style='vertical-align: top;'>")
              
                ),
  
  navbarPage("", id = "inTabset", 
             
              tabPanel("Select a Project",  icon = icon("list-ul"),
                       
                       fluidPage(
                         
                         fluidRow(br(), br(), br(), br(), br()),
                         fluidRow( column(4),
                                   column(8,selectInput('currentProject', 'Project', choices = NULL))
                         )
                         
                       )
              ),
              
              tabPanel("Covariate Rasters",  icon = icon("map"),
                       
                       tabsetPanel(id = "inCovariateTabset", 
                       
                           tabPanel("Currently Available Project Covariates",
                             sidebarLayout(
                               sidebarPanel(width = 2, 
                                 wellPanel(HTML('<p style="color:blue;font-weight: bold;">Upload New Covariates</p>'),
                                           fileInput("covariateFiles", "Choose GeoTiff File", multiple = T,  accept = c("image/tiff", ".tif")) )
                               ),
                               mainPanel(
                                 HTML('<H2 style="color:blue;font-weight: bold;">Currently Available Project Covariates</H2>'),
                                 plotOutput('covPlots')
                               )
                             )
                             ),
                           tabPanel("Explore Covariates",
                                    sidebarLayout(
                                      sidebarPanel(width = 2,
                                                   #HTML('<p  style="color:blue;font-weight: bold;">Currently Available Project Covariates</p>'),
                                                   selectInput('availCovList', 'Available Covariates', choices=NULL, multiple=F),
                                                   actionButton('DeleteCov', "Delete Covariate"),
                                                   selectInput('transformCov', 'Transform', choices=c('Log', 'Square Root', 'Exp'), multiple=F)
                                                   
                                      ),
                                      mainPanel(
                                        
                                        leafletOutput("CovariateMap", width = "450", height = "450"),
                                        verbatimTextOutput('covDesc'),
                                        plotlyOutput("covDensityChart", width = "450", height = "450")
                                      )
                                    )
                           )
                       )
              ),
             
             
             
              tabPanel("Soil Sample Data",  icon = icon("table"),
                       
                 tabsetPanel(id = "inCovariateTabset", 
                                   
                    tabPanel("Upload Sample Data",  icon = icon("cloud-upload-alt"), 
                       sidebarLayout(
                           sidebarPanel(width =2,
                             wellPanel(HTML('<p style="color:blue;font-weight: bold;">Upload My Soil Sample Data</p>'),
                                       fileInput("sampleFile", "Choose CSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")))
                           ),
                           mainPanel(
                             rHandsontableOutput("UploadSamplesTable")
                            
                             #withSpinner(leafletOutput("UploadSamplesMap", width = "650", height = "450"))
                           )
                       ) 
                     ),
                     tabPanel("Explore Sample Data",  icon = icon("cloud-upload-alt"), 
                             sidebarLayout(
                               sidebarPanel(width =2, 
                                 selectInput("SampleFile", "Soil Attribute Dataset", choices = NULL),  
                                 selectInput("SampleAtt", "Soil Attribute", choices = NULL),
                                 selectInput("SampleSite", "Soil Site", choices = NULL)
                               ),
                               mainPanel(
                                 fluidPage(
                                     fluidRow(
                                              leafletOutput("UploadSamplesMap", width = "450", height = "450")
                                              ), ##  something not right here with layout - need to sort out
                                     fluidRow( plotOutput("SplinePlot", width = "450", height = "350"),
                                               rHandsontableOutput("ExploreSamplesTable", width = "450", height = "350")
                                                
                                     )
                                 )
                                 #withSpinner(leafletOutput("UploadSamplesMap", width = "650", height = "450"))
                               )
                             ) 
                    )
                  )
              
              ),
              tabPanel("Run a Model",  icon = icon("laptop"),
                       sidebarLayout(
                         sidebarPanel(width = 2,
                           selectInput("MLType", "Machine Learning Method", choices = c("Random Forest", "Cubist")),
                           selectInput("ModelSampleFile", "Soil Attribute Dataset", choices = NULL),
                           selectInput("ModelAttribute", "Soil Attribute", choices = NULL),
                           selectInput("ModelDepth", "Depth Layer", choices =  c('0-5 cm', '5-15 cm', '15-30 cm', '30-60 cm', '60-100 cm', '100-200 cm')),
                           actionButton("RunModelBtn", 'Run a Model')
                         ),
                         mainPanel(
                           leafletOutput("RunModelMap", width = "650", height = "450")
                          
                        )
                      )
              )
                       
              
     )
)

)


###########################     SERVER    ###########################################

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) 
  
  
  RV <- reactiveValues()
  RV$SampleFileData = NULL
  RV$ProjCovStack = NULL
  RV$CurrentCovariate = NULL
  RV$CurrentSplineFile = NULL
  RV$CurrentSiteData = NULL
  RV$ModelSampleFileData = NULL
  
  
  
  observeEvent(input$RunModelBtn, {
  
    
    req(RV$ModelSampleFileData)
    print("you are modelling my friend")
    
    if(input$MLType == 'Cubist'){
          session$sendCustomMessage(type = 'testmessage', message = 'Cubist model is not implemented at the moment')
    }else{
      
      spls <-  RV$CurrentSplineFile 
      str(spls)
      head(spls$harmonised)
      
      svals <- spls$harmonised['0-5 cm']
      ids <- spls$harmonised['id']
      svalsDF <- data.frame(sid=ids, vals=svals)
      colnames(svalsDF) <- c('sid', 'vals')
      
      #inDF <- RV$SampleFileData 
      #att <- input$SampleAtt
      inDF <- RV$SampleFileData
      att <- input$ModelAttribute
      locs <- inDF[inDF$Attribute == att &inDF$UpperDepth == 0, ]
      locs2 <- locs[, c(1,2,3)]
      sdf <- merge(svalsDF, locs2, by.x = 'sid' , by.y = 'sid')
      
      coordinates(sdf) <- ~x+y
      
      covpaths <- list.files( 'C:/Projects/Myanmar/ShinyDeploy/Demo/Development/Covariates', pattern = paste0( '.tif$'), full.names = T, recursive =F)
      
      stk <- stack(covpaths)
      covPts <- extract(stk, sdf, df=T)
      modDF <- na.omit(data.frame(sdf, covPts[,-1] ))
      modDF2 <- modDF[, c(-1, -3,-4,-5)]
      
      colnames(modDF2)[1] <- att
      head(modDF2)
      
      
      splitSamples <-createTrainingSample(modDF2, 1, 66)
      trainSet <- as.data.frame(splitSamples[1])
      validSet <- as.data.frame(splitSamples[2])
      colnames(trainSet) <- colnames(modDF2)
      colnames(validSet) <- colnames(modDF2)
      
      
      Rmodel <- ranger(trainSet[,1] ~ ., data = trainSet[,-1], write.forest = TRUE, importance = 'impurity', num.trees = 500)
      
      mvals <- predict(Rmodel, validSet[,-1])
      mfit <- data.frame(validSet[,1], mvals$predictions)
      colnames(mfit) <- c('obs', 'mod')
      
      modelDir <- 'C:/temp/amodel'
      createDirectory(modelDir)
      RmodelFilename <- paste0(modelDir, '/', att)
      fitStats(mfit$obs ,mfit$mod , paste0( att,' using Ranger'),  paste0(RmodelFilename, '_ModelStats.txt'), 'topleft', verbose = T)
      
      MapRF(att=att, model = Rmodel, templateR = templateR, cpus = 7, theStack = stk, rootDir = modelDir, minBlocks = 14)
      
      
      
      
    }
    
  })
  
  
  
  observe({
    req(input$currentProject )
    inDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
    paths <- list.files( paste0(inDir), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.csv$'), full.names = F, recursive =F)
    if(length(paths) > 0)
    {
      updateSelectInput(session, "SampleFile", choices =  str_remove(paths, '.csv'))
      updateSelectInput(session, "ModelSampleFile", choices =  str_remove(paths, '.csv'))
    }
  })
  
  
  
  observe({
    
    req(input$ModelSampleFile)
    inDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
    inPath <- paste0(inDir, '/', input$ModelSampleFile, '.csv')
    df <- read.csv(inPath)
    RV$ModelSampleFileData <- df
    atts <- unique(df$Attribute)
    updateSelectInput(session, "ModelAttribute", choices = atts)
  })
  
  
  observe({

    req(input$SampleFile)
      inDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
      inPath <- paste0(inDir, '/', input$SampleFile, '.csv')
      df <- read.csv(inPath)
      RV$SampleFileData <- df
      atts <- unique(df$Attribute)
      updateSelectInput(session, "SampleAtt", choices = atts)
  })
  
  
  
  observe({
    
    req(RV$SampleFileData )
    req(input$SampleSite)
    
        inPath <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples/',input$SampleFile, '/', input$SampleAtt, '_spline.rds' )
        spls <- readRDS(inPath)
        RV$CurrentSplineFile <- spls
  })
  
  
  observe({
    
    req(RV$SampleFileData )
    
    df <- RV$SampleFileData 
    psites <- df[df$Attribute == input$SampleAtt, ]
    psitesU <- unique(psites$sid)
    updateSelectInput(session, "SampleSite", choices = psitesU)
  })
  
  
  observe({
    
    req(RV$SampleFileData )
    
      df <- RV$SampleFileData 
      psite <- df[df$Attribute == input$SampleAtt  & df$sid == input$SampleSite, ]
      RV$CurrentSiteData <- psite
  })
 
  
  observe({
    inDir <- paste0(rootDir, '/', currentUser)
    projects <- list.dirs(inDir, full.names = F, recursive = F)
    updateSelectInput(session, "currentProject", choices =  projects)
  })
  
  observe({
    req(RV$SampleFields)
    updateSelectInput(session, "ModelAttribute", choices =  colnames(RV$SampleFields))
  })
  
  observe({
    req(RV$ProjCovStack)
    updateSelectInput(session, "availCovList", choices =  names(RV$ProjCovStack))
  })
  
  

 
  observe({
   
    req(input$covariateFiles)
    stk <- stack(input$covariateFiles$datapath)
    covnames <- str_remove(input$covariateFiles$name, '.tif')

    names(stk) <- covnames
    
    isolate({
        for (i in 1:length(input$covariateFiles$datapath)) {
          f<- input$covariateFiles$datapath[i]
          r <- raster(f)
          covDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' )
          writeRaster(r, paste0(covDir, '/', covnames[i], '.tif'), overwrite=T)
        } 
        
        covDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' )
        paths <- list.files( paste0(covDir), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.tif$'), full.names = T, recursive =F)
        RV$ProjCovStack <- stack(paths)
    })
    
  })
  
  output$covPlots <- renderPlot({
    req(RV$ProjCovStack )
    plot(RV$ProjCovStack )
  })
  
  output$SplinePlot <- renderPlot({
    req(RV$CurrentSplineFile)
    
    
    print('SPLINE')
    spls <- RV$CurrentSplineFile
    
    stdDeps <- c('2.5', '10', '22.5', '45', '80', '150')
    
    sid <- which(spls$harmonised$id == as.numeric(input$SampleSite))
    pstd <- spls$harmonised[sid,]
    
    d = spls$var.1cm[,sid]
    minx <- min(d) - min(d) * 0.2
    maxx <- max(d) + max(d) * 0.2
    plot( d, 1:length(d), type="n", main=paste( paste0('Spline of ', input$SampleAtt, ' at ', input$SampleSite)), xlab = input$SampleAtt, ylab = "Depth", yaxs = "i", xaxs = "i", xlim = c(minx, maxx), ylim = rev(range(c(0,200))))
    lines( d, 1:length(d), type="l") 
     stdVals <- c(spls$harmonised$`0-5 cm`[sid], spls$harmonised$`5-15 cm`[sid], spls$harmonised$`15-30 cm`[sid], spls$harmonised$`30-60 cm`[sid], spls$harmonised$`60-100 cm`[sid], spls$harmonised$`100-200 cm` [sid])
     stdPts <- data.frame(stdDeps, stdVals)
     points(stdVals, stdDeps, col='red', pch=19)
    
  })
  

  
  observe({
    req(input$currentProject )

    createDirectory(paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' ))
    createDirectory(paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' ))
    createDirectory(paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Outputs' ))
    
    covDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' )
    paths <- list.files( paste0(covDir), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.tif$'), full.names = T, recursive =F)
    if(length(paths) > 0){
      RV$ProjCovStack <- stack(paths)
    }else{
      RV$ProjCovStack <- NULL
    }
  })
  
  
  
  
  output$ExploreSamplesTable <- renderRHandsontable({
    
    req(RV$CurrentSiteData)
       rhandsontable(RV$CurrentSiteData,  height = 600, manualColumnResize = T, readOnly = TRUE, rowHeaders = F) 
  })
  
 
  output$UploadSamplesTable <- renderRHandsontable({
    
    req(input$sampleFile)
    isolate({
       sampName <- str_remove(input$sampleFile$name, '.csv')

       sampDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
       outPath <- paste0(sampDir, '/', sampName, '.csv')
       print(outPath)
       file.copy(input$sampleFile$datapath, outPath)
    })
    
    
    inDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
    paths <- list.files( paste0(inDir), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.csv$'), full.names = F, recursive =F)
    if(length(paths) > 0)
    {
      updateSelectInput(session, "SampleFile", choices =  str_remove(paths, '.csv'))
    }
    
    tryCatch(
      {
        df <- read.csv(outPath,
                       header = T,
                       sep = ',',
                       quote = '"',
                       stringsAsFactors = F)
        
        stddf <- df[with(df, order(sid, Attribute, UpperDepth)),]
        print(head(stddf, 10))
        RV$SampleFields <- stddf
        
        atts <- unique(stddf$Attribute)
 
        
        spDir <- paste0(inDir, '/', sampName)
        createDirectory(paste0(spDir))
        for (i in 1: length(atts)) {
          att <- atts[i]
          spdf <- stddf[stddf$Attribute==att, c(1,4,5,7)]
          sp.fit <- ea_spline(spdf, var.name='Val')
          saveRDS(sp.fit, paste0(spDir, '/', att, '_spline.rds'))
          
        }
        
        
        
        
        rhandsontable(df,  height = 600, manualColumnResize = T, readOnly = TRUE, rowHeaders = F) 
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
})
  
  
  
  
  
  output$UploadSamplesMap <- renderLeaflet({
    
    req(RV$SampleFileData )
    
    inDF <- RV$SampleFileData 
    att <- input$SampleAtt
    sdf <- inDF[inDF$Attribute == att, ]
    coordinates(sdf) <- ~x+y

    leaflet() %>%
      
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%

      addCircleMarkers(
        data=sdf,
        group = "Soil Samples",
        radius = 2) %>%
        
      addLayersControl(
        baseGroups = c("Satelite Image"),
        overlayGroups = c("Soil Samples"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
  
  
  
  
  output$CovariateMap <- renderLeaflet({
    
    req(input$availCovList)
    
    covName <- input$availCovList
    
    isolate({
      covDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' )
    })
    rPath <- paste0(covDir, '/', covName, '.tif')
    r <- raster(rPath)
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r), na.color = "transparent")
    
    RV$CurrentCovariate <- r
    
    leaflet() %>%
      
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
      # addTiles(group = "Map") 
      addRasterImage(r, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(r), title = input$availCovList)
 
  })
  
  output$covDesc <- renderText({
    req(RV$CurrentCovariate)
    rsum <- paste0('dimensions  : ', dim(r)[1], ' ',dim(r)[2], ' ', dim(r)[1] * dim(r)[2],  ' (nrow, ncol, ncell) \n')
    rsum <- paste0(rsum, 'resolution  : ',format(round(res(r)[1], 5), nsmall = 2) , ' ',format(round(res(r)[2], 5), nsmall = 2), ' (x, y) \n')
    rsum <- paste0(rsum, 'extent      : ', format(round(extent(r)[1], 5), nsmall = 2), ' ',format(round(extent(r)[2], 5), nsmall = 2), ' ', format(round(extent(r)[3], 5), nsmall = 2), ' ', format(round(extent(r)[4], 5), nsmall = 2), '    (xmin, xmax, ymin, ymax) \n')
    rsum <- paste0(rsum, 'coord. ref. : ', crs(r), '\n')
    rsum <- paste0(rsum, 'values      : ', min(na.omit(r[])), ' ', format(round(max(na.omit(r[])), 5), nsmall = 4), '  (min, max) \n')
    rsum
    
    
  })
  
  output$covDensityChart <- renderPlotly({
    
    req(RV$CurrentCovariate)
    
        r <- RV$CurrentCovariate
        dens <- density(na.omit(r[]),adjust = .5)
        p <- plot_ly(x = ~dens$x, y = ~dens$y, type = 'scatter', mode = 'lines', fill = 'tozeroy', xaxis = list(rangemode = "tozero")) 
        p$elementId <- NULL
        p
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)






