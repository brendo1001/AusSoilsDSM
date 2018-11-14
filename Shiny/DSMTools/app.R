library(rgdal)
library(raster)
library(stringr)
library(leaflet)
library(plotly)
library(rhandsontable)
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
#library(mapview)
library(gstat)
library(sp)
library(shinycssloaders)
library(zip)
library(plotKML)
library(rgeos)
library(rasterVis)
library(data.table)
#library(ithir)
library(ranger)
library(Cubist)

library(epiR)

source('DSMToolsConfig.R')
source('mylibs/GeneralUtils.R')
source('mylibs/VectorUtils.R')
#source('mylibs/ModelUtils.R')
source('mylibs/MapCubistModels.R')
source('mylibs/ithirSub.R')
source("helpers.R")


defaultReps <- 2
defaultFolds <- 2

state <- suppressWarnings( read.csv(paste0(rootDir, '/', currentUser, '/state.cfg'), sep = "=", header = F ))

previousProj <- str_trim(as.character(state[1,2]))
previousSamples <- str_trim(as.character(state[2,2]))


includeHTML2 <- function (path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  # lines2 <-paste(lines, collapse = "\\r\\n")
  # l3 <- str_split(lines2, '<body>')
  # l4 <- str_split(l3[[1]][2], '</body>')
  # l5 <- l4[[1]][1]
  # l6 <- str_replace_all(l5, "[\\\\r\\\n]" , "")
  return(HTML(lines))
}


######################################   UI   ###################################################
ui <- tagList(fluidPage(
  
  
  useShinyjs(),
  tags$style(appCSS),
  useShinyalert(),
  
  tags$head(tags$link( rel="icon", type="image/png", href="favicon-32x32.png", sizes="32x32" ),
            tags$head(tags$script(src = "message-handler.js")),
            tags$title("DSM Tools"),
            HTML("<img src=soilcores2.PNG style='vertical-align: top;'>")
            
  ),
  
  navbarPage("", id = "inTabset", 
             
             tabPanel("Select a Project",  icon = icon("list-ul"),
                    
                    sidebarLayout(
                      sidebarPanel(width = 3, 
                                   wellPanel(HTML('<p style="color:blue;font-weight: bold;">Project Management</p>'),
                                             fluidRow(selectInput('currentProject', 'Select a Project to Use', choices = NULL)),
                                             fluidRow( actionButton('AddNewProject', "Create New Project"), br(), br(), br()),
                                             fluidRow( actionButton('DeleteProject', "Delete Project")),
                                             fluidRow( actionButton('RenameProject', "Rename Project"))
                                             )
                      ),
                      mainPanel(
                        fluidPage(
                          fluidRow( HTML('<H1><p style="color:blue;font-weight: bold;">Project Information</p></H1>')),
                        fluidRow(br(), br()),
                        
                        fluidRow(htmlOutput('projectInfoText'))
                      )
                    )
                        
                        
                        
                  )      
             ),
             
             tabPanel("Covariate Rasters",  icon = icon("map"),
                      
                      tabsetPanel(id = "inCovariateTabset", 
                                  
                                  tabPanel("Currently Available Project Covariates",
                                           sidebarLayout(
                                             sidebarPanel(width = 2, 
                                                          HTML('<p style="color:blue;font-weight: bold;">Upload New Covariates</p>'),
                                                                    fileInput("covariateFiles", "Choose GeoTiff File", multiple = T,  accept = c("image/tiff", ".tif")),
                                                                    br(),
                                                                    progressBar(id = "covariateProcessingProgress", value = 0, total = 100,  title = "", display_pct = TRUE )
                                                                    
                                                          
                                            ),
                                             mainPanel(
                                               HTML('<H2 style="color:blue;font-weight: bold;">Currently Available Project Covariates</H2>'),
                                               #withSpinner(plotOutput('covPlots', width=1000, height=1000)),
                                               withSpinner(imageOutput("covImage"))
                                               
                                             )
                                           )
                                           
                                  ),
                                  tabPanel("Manage Covariates",
                                           sidebarLayout(
                                             sidebarPanel(width = 2,
                                                          #HTML('<p  style="color:blue;font-weight: bold;">Currently Available Project Covariates</p>'),
                                                          selectInput('availCovList', 'Available Covariates', choices=NULL, multiple=F),
                                                          #actionButton('DeleteCov', "Delete Covariate"),
                                                          
                                                          HTML('<H4>Transform Values</H4>'),
                                                          wellPanel( selectInput('transformCovType', 'Transformation Type', choices=c('Power' ,'Exp', 'Log', 'Addition', 'Multiplication', 'Cos', 'Tan', 'Sin'), multiple=F),
                                                                     textInput("transformCovValue", label = "Transform Value", placeholder = "", width = 100),
                                                                     textInput("transformCovNewName", label = "New Covariate Name", placeholder = ''),
                                                                     actionButton('transformCov', "Transform")
                                                          ),
                                                          fluidRow( actionButton('CovariateDelete', "Delete Covariates")),
                                                          fluidRow( actionButton('CovariateRename', "Rename a Covariate"))
                                                          
                                             ),
                                             mainPanel(
                                               
                                               withSpinner(leafletOutput("CovariateMap", width = "450", height = "450")),
                                               withSpinner(verbatimTextOutput('covDesc')),
                                               withSpinner(plotlyOutput("covDensityChart", width = "450", height = "450"))
                                             )
                                           )
                                  )
                      )
             ),
             
             
             
             tabPanel("Soil Sample Data",  icon = icon("table"),
                      
                      tabsetPanel(id = "inSamplesTabset", 
                                  
                                  tabPanel("Upload Sample Data",  icon = icon("cloud-upload-alt"), 
                                           sidebarLayout(
                                             sidebarPanel(width =2,
                                                          wellPanel(HTML('<p style="color:blue;font-weight: bold;">Upload My Soil Sample Data</p>'),
                                                                    fileInput("sampleFileRaw", "Choose CSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
                                                          wellPanel(HTML('<p style="color:blue;font-weight: bold;">Process My Data</p>'),
                                                                    actionButton("ProcessSampleData","Import Sample Data"),
                                                                    selectInput("sampsSiteID", "Sample Site ID" , choices = NULL),
                                                                    selectInput("sampsLatitude", "Latitude  Field" , choices = NULL),
                                                                    selectInput("sampsLongitude", "Longitude Field", choices = NULL),
                                                                    #selectInput("sampsUpperDepth", "Upper Depth Field" , choices = NULL),
                                                                    #selectInput("sampsLowerDepth", "Lower Depth Field", choices = NULL),
                                                                    selectInput("sampsDepth", "Sample Depth Field", choices = NULL),
                                                                    selectInput("sampsStratified", "Stratify Field", choices = NULL),
                                                                    selectInput("sampsTransect", "Transect Field", choices = NULL),
                                                                    textInput("sampsNoDataVal", "No Data Value", value = -9999),
                                                                    pickerInput(
                                                                      inputId = "sampsDataFields", 
                                                                      label = "Sample Data Fields", 
                                                                      choices = NULL, 
                                                                      options = list(
                                                                        `actions-box` = TRUE, 
                                                                        size = 8,
                                                                        `selected-text-format` = "count > 3"
                                                                      ), 
                                                                      multiple = TRUE
                                                                    )
                                                          )
                                             ),
                                             mainPanel(
                                               withSpinner( rHandsontableOutput("UploadSamplesTable"))
                                               
                                               #withSpinner(leafletOutput("UploadSamplesMap", width = "650", height = "450"))
                                             )
                                           ) 
                                  ),
                                  tabPanel("Explore Sample Data",  icon = icon("cloud-upload-alt"), 
                                           sidebarLayout(
                                             sidebarPanel(width =2, 
                                                          selectInput("SampleFile", "Soil Attribute Dataset", choices = NULL),  
                                                          selectInput("SampleAtt", "Soil Attribute", choices = NULL)
                                             ),
                                             mainPanel(
                                               fluidPage(
                                                 fluidRow(withSpinner(tableOutput("ExploreAttSummaryTable" ))),      
                                                          fluidRow(withSpinner(plotOutput("ExploreAttPlotHist", width = "450", height = "450"))),
                                                          fluidRow(withSpinner(plotOutput("ExploreAttPlotBox", width = "450", height = "450"))),
                                                          fluidRow(withSpinner(plotOutput("ExploreAttPlotQNorm", width = "450", height = "450"))),
                                                 withSpinner(leafletOutput("ExploreSamplesMap", width = "450", height = "450"))
                                                 
                                                 )
                                               )
                                             )
                                        ) 
                                  
                      )
                      
             ),
             
             tabPanel("Drill Covariate Rasters",  icon = icon("map-pin"), 
                      sidebarLayout(
                        sidebarPanel(width =2,
                                    
                                     
                                               
                                               htmlOutput("DrillSampleFileText"),
                                     
                                     wellPanel(HTML('<p style="color:blue;font-weight: bold;">Drill Covariate Data</p>'),
                                               withBusyIndicatorUI( actionButton("DrillSampleData","Drill Rasters"))
                                     )
                        ),
                        mainPanel(
                          withSpinner( rHandsontableOutput("DrillSamplesTable"))
                        )
                      ) 
             ),
             tabPanel("Run a Model", id='RunModelTab', icon = icon("cogs"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     
                                     wellPanel(
                                       HTML('<p style="color:blue;font-weight: bold;">Display Existing Models</p>'),
                                       selectInput("ExistingModels", "Existing Models", choices = NULL)
                                     ),
                                     
                                     
                                     wellPanel(
                                       HTML('<p style="color:blue;font-weight: bold;">Generate New Model</p>'),
                                     selectInput("MLType", "Machine Learning Method", choices = c( "Cubist")),
                                    
                                     selectInput("ModelAttribute", "Soil Attribute", choices = NULL),
                                     numericInput("ModelDepth", "Depth (cm)", value=5, min=1, max=200),
                                     textInput('ModelName', 'Model Name'),
                                     selectInput("ModelTrainingMethod", "Training Method", choices = c('Simple Random','Stratified', 'Transect Lumped')),

                                     actionButton(
                                       inputId = "RunModelBtn",
                                       label = "Run Model"
                                     ),
                                     br(),  br(),
                                     progressBar(id = "modelRunProgress", value = 0, total = 100,  title = "", display_pct = TRUE ),
                                     

                                     actionLink("toggleModelOptions", "Toggle the following text"),
                                     conditionalPanel(
                                       condition = "input.toggleModelOptions % 2 == 1",
                                       
                                       numericInput("ModelNumRules", "Max No. Rules", value=10, min=1, max=200),
                                       numericInput("ModelExtrapThresh", "Extrapolatio", value=10, min=1, max=200),
                                       numericInput("ModelFoldNum", "Number of Folds", value=defaultFolds, min=1, max=20),
                                       numericInput("ModelRepsNum", "Number of Reps", value=defaultReps, min=1, max=20)
                                     )
                                     )
                                    
                                    

                        ),
                        mainPanel(
                          fluidPage(
                            fluidRow( 
                              withSpinner(leafletOutput("RunModelMap", width = "650", height = "450"))
                              
                              ),
                            fluidRow(htmlOutput('textLabel_CovariateUsage'), rHandsontableOutput("ModelConditionsTable")),
                            fluidRow(div(style="width:200px;",htmlOutput('textLabel_ModelFitStats'), withSpinner(verbatimTextOutput('ModelValidationStats')))),
                            fluidRow(HTML('<br>'), plotOutput("obsVmodPlot", width = "400", height = "400")),
                            fluidRow(htmlOutput('textLabel_InternalFoldStats'), rHandsontableOutput("InternalValidationTable")),
                            fluidRow(HTML('<br>'), plotOutput("InternalValidationTablePlot", width = "400", height = "400")),
                            fluidRow(htmlOutput('textLabel_ExternalFoldStats'), rHandsontableOutput("ExternalValidationTable")),
                            fluidRow(HTML('<br>'), plotOutput("ExtenalValidationTablePlot", width = "400", height = "400")),
                            fluidRow(div(style="width:700px;", htmlOutput('textLabel_ModelSummary'), withSpinner(verbatimTextOutput('ModelSummary'))))
                         
                          )
                        )
                      )
             ),
             tabPanel("Download Data", value ='DownloadDataPanel', icon = icon("download"),
                      wellPanel(
                        selectInput("ModelDownloadSelect", "Download Data", c('Model Package', 'Covariates', 'Sample Files'), width = 200),
                        downloadButton('ModelDownloadBtn', 'Download')
                      )
             ),
                     
             tabPanel("Help",  icon = icon("book"),
                      #includeHTML2(paste0( "www/StaticPages/DSMToolsHelp.htm"))),
             includeHTML("www/StaticPages/DSMToolsHelp.htm")),
             tabPanel("About",  icon = icon("info-circle"),
                      includeHTML("www/StaticPages/DSMToolsAbout.html")
                      )
                      
             
             
  )
)

)


#####  General Functions



getProjects <- function(){
  
 basename( list.dirs(paste0(rootDir, '/', currentUser), recursive = F))
  
}

createNewProject<- function(projname, templateRasterTempPath){
  

  dir.create(paste0(rootDir, '/', currentUser, '/', projname))
  dir.create(paste0(rootDir, '/', currentUser, '/', projname, '/Covariates'))
  dir.create(paste0(rootDir, '/', currentUser, '/', projname, '/Outputs'))
  dir.create(paste0(rootDir, '/', currentUser, '/', projname, '/Samples'))
  dir.create(paste0(rootDir, '/', currentUser, '/', projname, '/tmpData'))
  dir.create(paste0(rootDir, '/', currentUser, '/', projname, '/GeoTemplate'))
  

  
  r <- raster(templateRasterTempPath)
  
  templatePath <- paste0(rootDir, '/', currentUser, '/', projname, '/GeoTemplate/Template.tif' )
  writeRaster(r, templatePath)
  
}

deleteProject<- function(projname){
  if(projname != 'Demo')
    unlink(paste0(rootDir, '/', currentUser, '/', projname), recursive = T)
}

renameProject<- function(oldprojname, newprojname){
  if(oldprojname != 'Demo'){
    file.rename(paste0(rootDir, '/', currentUser, '/', oldprojname), paste0(rootDir, '/', currentUser, '/', newprojname))
  }
}

getCovariates <- function(projname){
  nms <- basename( list.files(paste0(rootDir, '/', currentUser, '/', projname, '/Covariates'), pattern='.tif', full.names = T, recursive = F))
  str_remove(nms , '.tif')
}

getSampleFiles <- function(projname){
  paths <- basename( list.files(paste0(rootDir, '/', currentUser, '/', projname, '/Samples'), pattern='.csv', full.names = T, recursive = F))
  nms2 <- clipPaths <- paths[!grepl('_Drill_Data', paths, ignore.case = T)]
  str_remove(nms2 , '.csv')
}

getCovariateStack <- function(projname, reducedRes=F){
  covDir <- paste0(rootDir, '/', currentUser, '/', projname, '/Covariates' )
  if(reducedRes){
  covpaths <- list.files( covDir, pattern = paste0( '_RedRes.tif$'), full.names = T, recursive =F)
  }else{
    paths <- list.files( covDir, pattern = paste0( '.tif$'), full.names = T, recursive =F)
    covpaths <- paths[!grepl('_RedRes', paths, ignore.case = T)]
  }
  if(length(covpaths) > 0){
    stk <- stack(covpaths)
  }else{
    stk <- NULL
  }
  stk
}

getTemplate <- function(projname){
  templatePath <- paste0(rootDir, '/', currentUser, '/',  projname, '/GeoTemplate/Template.tif' )
  templateR <- raster(templatePath)
  templateR
}

getModelNames <- function(projname){
  nms <- basename( list.files(paste0(rootDir, '/', currentUser, '/', projname, '/Outputs'), pattern='.rds', full.names = T, recursive = T))
  m1 <- str_remove(nms , '.rds')
  m2 <- str_remove(m1, 'Model!')
  #m3 <- str_split(m2, '!')
  m2
}


getModelNameFromPath <- function(ModelPath){
  
  if(file.exists(ModelPath)){
    basename(dirname(ModelPath))
  }else{
    return(NULL)
  }
}

###########################     SERVER    ###########################################

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) 
  
  
  RV <- reactiveValues()
  RV$SampleFileData = NULL
  RV$ProjCovStack = NULL
  RV$CurrentCovariate = NULL
  #RV$CurrentSplineFile = NULL
  
  RV$CurrentRawSiteData = NULL
  RV$CurrentSampleData = NULL
  RV$CurrentSampleDataName = NULL
  RV$CurrentSampleDataFields = NULL
  
  RV$CurrentCovDrillData = NULL
  
  #RV$ModelSampleFileData = NULL
  RV$CurrentModel = NULL
  RV$CurrentModelName = NULL
  RV$CurrentModelSummary = NULL
  RV$CurrentModelPath = NULL
  #RV$ModelData = NULL
  RV$CurrentModelRaster = NULL
  
  #RV$ShowMapGenProgress = F
  
  
  
  
  observe({
    
    updateSelectInput(session, "ExistingModels", choices =  getModelNames(input$currentProject))
    
  }) 
  
  
  output$textLabel_CovariateUsage <- renderText({
    paste0(('<br><H3>Model Covariate Usage</H3><br>'))
  })
  
  output$textLabel_ModelFitStats <- renderText({
    paste0(('<br><H3>Model Fit Stats</H3><br>'))
  })
  
  output$textLabel_InternalFoldStats<- renderText({
    paste0(('<br><H3>Internal Model Fold Stats</H3><br>'))
  })
  
  output$textLabel_ExternalFoldStats<- renderText({
    paste0(('<br><H3>External Model Fold Stats</H3><br>'))
  })
  
  output$textLabel_ModelSummary<- renderText({
    paste0(('<br><H3>Model Summary</H3><br>'))
  })
  
  output$projectInfoText <- renderText({
    req(input$currentProject)
    r <- getTemplate(input$currentProject)
    covs <- getCovariateStack(input$currentProject)
    covsAsList <- paste0('<li>', names(covs), '</li>', collapse = ' ')
    samps <- getSampleFiles(input$currentProject)
    sampsAsList <- paste0('<li>', samps, '</li>', collapse = ' ')
    models <- getModelNames(input$currentProject)
    bits <- str_split(models, '!')
    msamps <- sapply(bits, function (x) x[1])
    mprops <- sapply(bits, function (x) x[2])
    mdepths <- sapply(bits, function (x) x[3])
    modelsAsList <- paste0('<li> Samples = <b>', msamps, '</b> with soil properties = <b>', mprops, '</b> for <b>', mdepths, '</b> Depth</li>', collapse = ' ')
    
    rsum <- ''
    rsum <- paste0(rsum, '<H4><b>Geographic Template</b></H4>')
    rsum <- paste0(rsum, '<p>Dimensions  : ', dim(r)[1], ' ',dim(r)[2], ' ', dim(r)[1] * dim(r)[2],  ' (nrow, ncol, ncell)<br>')
    rsum <- paste0(rsum, 'Resolution  : ',format(round(res(r)[1], 5), nsmall = 2) , ' ',format(round(res(r)[2], 5), nsmall = 2), ' (x, y)<br>')
    rsum <- paste0(rsum, 'Extent      : ', format(round(extent(r)[1], 5), nsmall = 2), ' ',format(round(extent(r)[2], 5), nsmall = 2), ' ', format(round(extent(r)[3], 5), nsmall = 2), ' ', format(round(extent(r)[4], 5), nsmall = 2), '    (xmin, xmax, ymin, ymax) <br>')
    rsum <- paste0(rsum, 'Projection : ', crs(r), '<br>')
    rsum <- paste0(rsum, '</p><br>')
    rsum <- paste0(rsum, '<H4><b>Covariates</b></H4>')
    rsum <- paste0(rsum, '<ul>', covsAsList, '</ul><br>')
    rsum <- paste0(rsum, '<H4><b>Soil Sample Files</b></H4>')
    rsum <- paste0(rsum, '<ul>', sampsAsList, '</ul><br>')
    rsum <- paste0(rsum, '<H4><b>Existing Models</b></H4>')
    rsum <- paste0(rsum, '<ul>', modelsAsList, '</ul')
    
    rsum
    
    
  })
  
  
  
  output$ModelDownloadBtn <- downloadHandler(
    
    

    filename = function() {
      req(input$ModelDownloadSelect)
      d <- input$ModelDownloadSelect
      if(is.null(input$ModelName) | nchar(input$ModelName) < 1){
        ModelName <- paste0(input$SampleFile, '!', input$ModelAttribute, '!', input$ModelDepth)
      }else{
        ModelName <- input$ModelName
      }
      
      if(d == 'Model Package'){
        paste0(ModelName, '_ModelPackage.zip')
      }else if(d == 'Covariates'){
        paste0(ModelName, '_Covariates.zip')
      }else if(d == 'Sample Files'){
        paste0(ModelName, '_SoilSamples.zip')
      }
    },
    content = function(file) {
      
      req(input$ModelDownloadSelect)
      d <- input$ModelDownloadSelect
      
      if(is.null(input$ModelName) | nchar(input$ModelName) < 1){
        ModelName <- paste0(input$SampleFile, '!', input$ModelAttribute, '!', input$ModelDepth)
      }else{
        ModelName <- input$ModelName
      }
      
      if(d == 'Model Package'){
        outDir <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', ModelName)
        fls <- list.files(outDir, pattern='.*', full.names = T, recursive = F)
      }else if(d == 'Covariates'){
        outDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates')
        fls <- list.files(outDir, pattern='.tif', full.names = T, recursive = F)
      }else if(d == 'Sample Files'){
        outDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples')
        fls <- list.files(outDir, pattern='.csv', full.names = T, recursive = F)
      }

      
      #fs <- dir(outDir, full.names = F, pattern = '*')
     
      fn <- paste0( rootDir, '/', currentUser, '/', input$currentProject, '/tmpData/data.zip')
      zip::zip(fn, fls, recurse=F)
      
       file.copy(paste0(fn), file)
    }
  ) 
  
  
  
  
  
  observeEvent(input$ModelDownloadSelect, {
    
    req(input$ModelDownloadSelect)
    d <- input$ModelDownloadSelect
    
    if(is.null(input$ModelName) | nchar(input$ModelName) < 1){
      ModelName <- paste0(input$SampleFile, '!', input$ModelAttribute, '!', input$ModelDepth)
    }else{
      ModelName <- input$ModelName
    }

    if(d == 'Model Package'){
      outDir <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', ModelName)
      fls <- list.files(outDir, pattern='.*', full.names = T, recursive = F)
    }else if(d == 'Covariates'){
      outDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates')
      fls <- list.files(outDir, pattern='.tif', full.names = T, recursive = F)
    }else if(d == 'Sample Files'){
      outDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples')
      fls <- list.files(outDir, pattern='.csv', full.names = T, recursive = F)
    }
    
    if(length(fls) == 0){
      updateActionButton(session, 'ModelDownloadBtn', label= 'Nothing to Download')
      disable("ModelDownloadBtn")
    }else{
      updateActionButton(session, 'ModelDownloadBtn', label= 'Download')
      enable("ModelDownloadBtn")
    }
  })
  
  
  

  
  output$obsVmodPlot <- renderPlot({
    req(RV$CurrentModelPath)
    if(file.exists(RV$CurrentModelPath)){
      mdir <- dirname(RV$CurrentModelPath)
      mstatsPath <- paste0(mdir, '/', str_replace(basename(RV$CurrentModelPath), '.rds', ''), '!ObsVsMod.csv')
      if(file.exists(mstatsPath)){
        vt <- read.csv(mstatsPath)
        
        cccC <- epi.ccc(vt$Observed, vt$Modelled, ci = "z-transform",conf.level = 0.95)
        r.sqC <- cor(vt$Observed, vt$Modelled)^2
        fitC <- lm(Modelled~ Observed, data=vt)
        
        minX = min(vt$Observed)
        maxX = max(vt$Observed)
        
        minY = min(vt$Modelled)
        maxY = max(vt$Modelled)
        maxVal = max(maxX, maxY)
        
        
        plot(vt, main=paste0( 'Model Fit from K-Folds '), pch=3, cex =0.5, xlim = c(minX,maxX), ylim = c(minY,maxY))
        
          abline(fitC, col="red")
          abline(0,1, col="green")
          #mtext("subtitle", cex=0.5)
          
          tx =  minX * 1.1
          print(maxY)
          ty1 =  maxY * 0.98
          ty2 =  maxY  * 0.95
          
          legPos='bottomright'
          print(tx)
          print(ty1)
          #legend(legPos, c('Regression Line') , lty=1, col=c('red'), cex=1, bty="o", bg = 'gray90' )
          legend(legPos, c('Regression Line', '1:1') , lty=1, col=c('red','green'), bty='o', cex=1, bg = 'gray90')
          text(tx,ty1, paste("R2 = ",round(r.sqC, digits = 2)), pos=4)
          text(tx,ty2, paste("LCCC = ", round(cccC$rho.c[,1], digits = 2)), pos=4)
          
      }
    }
  })
  
  
  output$InternalValidationTablePlot <- renderPlot({
    req(RV$CurrentModelPath)
    if(file.exists(RV$CurrentModelPath)){
      mdir <- dirname(RV$CurrentModelPath)
      mstatsPath <- paste0(mdir, '/', str_replace(basename(RV$CurrentModelPath), '.rds', ''), '!InternalValidation.csv')
      if(file.exists(mstatsPath)){
        inTable <- read.csv(mstatsPath)
        hist(inTable$concordance, main= 'Internal Validation LCCC', xlab='Lins Concordance')
      }
    }
  })
  
  output$ExtenalValidationTablePlot <- renderPlot({
    req(RV$CurrentModelPath)
    if(file.exists(RV$CurrentModelPath)){
      mdir <- dirname(RV$CurrentModelPath)
      mstatsPath <- paste0(mdir, '/', str_replace(basename(RV$CurrentModelPath), '.rds', ''), '!ExternalValidation.csv')
      if(file.exists(mstatsPath)){
        inTable <- read.csv(mstatsPath)
        hist(inTable$concordance, main= 'External Validation LCCC', xlab='Lins Concordance')
      }
    }
  })
  
  
  
  output$ModelValidationStats <- renderText({
    req(RV$CurrentModelPath)
    rsum <-''
    if(file.exists(RV$CurrentModelPath)){
      mdir <- dirname(RV$CurrentModelPath)
      mstatsPath <- paste0(mdir, '/', str_replace(basename(RV$CurrentModelPath), '.rds', ''), '!ExternalValidation.csv')
      if(file.exists(mstatsPath)){
        inTable <- read.csv(mstatsPath)
        str(inTable)
        rsum <- paste0(rsum,'R2 : ', round(mean(inTable$R2), 2) , ' \n')
        rsum <- paste0(rsum,'LCCC : ', round(mean(inTable$concordance) ,2) , ' \n')
        rsum <- paste0(rsum,'MSE : ', round(mean(inTable$mse), 2) , ' \n')
        rsum <- paste0(rsum,'RMSE : ', round(mean(inTable$rmse), 2) , ' \n')
        rsum <- paste0(rsum,'Bias : ', round(mean(inTable$bias), 2) , ' \n')
      }
    }
    rsum
  })
  
  output$ModelConditionsTable <-renderRHandsontable({
    req(RV$CurrentModelPath)
    if(file.exists(RV$CurrentModelPath)){
      model <- readRDS(RV$CurrentModelPath)
      rhandsontable(model$usage, manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
    }else{
      NULL
    }
  })
  
  output$InternalValidationTable <-renderRHandsontable({
    req(RV$CurrentModelPath)
    if(file.exists(RV$CurrentModelPath)){
      mdir <- dirname(RV$CurrentModelPath)
      mstatsPath <- paste0(mdir, '/', str_replace(basename(RV$CurrentModelPath), '.rds', ''), '!InternalValidation.csv')
      if(file.exists(mstatsPath)){
        inTable <- read.csv(mstatsPath)
      rhandsontable(inTable, manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
      }else{
        NULL
      }
    }
  })
  
  output$ExternalValidationTable <-renderRHandsontable({
    req(RV$CurrentModelPath)
    if(file.exists(RV$CurrentModelPath)){
      mdir <- dirname(RV$CurrentModelPath)
      mstatsPath <- paste0(mdir, '/', str_replace(basename(RV$CurrentModelPath), '.rds', ''), '!ExternalValidation.csv')
      if(file.exists(mstatsPath)){
        inTable <- read.csv(mstatsPath)
      rhandsontable(inTable, manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
      }else{
        NULL
      }
    }
  })
  
  output$ModelSummary <- renderText({
    req(RV$CurrentModelPath)
    if(file.exists(RV$CurrentModelPath)){
        model <- readRDS(RV$CurrentModelPath)
        ms <- summary(model)
        ms$output
    }
  })
  
  
  
  
  
  
  ######   Sample file input   ###########
  
  
  
  
  observe ({
    if((input$toggleModelOptions  %% 2) == 1){
      updateActionButton(session, "toggleModelOptions", label = "Hide Advanced Model Options")
    }else{
      updateActionButton(session, "toggleModelOptions", label = "Show Advanced Model Options")
    }
  })
  
  output$ModelSampleFileText <- renderText({
   paste0('<p style="color:green;font-weight: bold;"><u>Soil Sample File</u><br>',   input$SampleFile, '</p>' )
  })
  
  output$DrillSampleFileText <- renderText({
    paste0('<p style="color:green;font-weight: bold;"><u>Soil Sample File </u><br>',   input$SampleFile, '</p>' )
  })
  
  
  observeEvent(input$DrillSampleData, {
    
    withBusyIndicatorServer("DrillSampleData", {
      
      req(RV$CurrentSampleData)
      SoilData <- RV$CurrentSampleData
      RV$CurrentSampleDataFields <- names(SoilData)

      coordinates(SoilData) <- ~Easting + Northing
      
      cvstack<- getCovariateStack(input$currentProject) 
      
      crs(SoilData) <- crs(cvstack)

      DSM_data<- extract(cvstack, SoilData, sp = 1, method = "simple")
      DSM_data <- as.data.frame(DSM_data)

      inDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
      spath <- paste0(inDir, '/', input$SampleFile, "_Drill_Data.csv")
      write.csv(DSM_data, spath, row.names = F)
      RV$CurrentCovDrillData <- DSM_data
      
    })
  })
  
  observe({
    
    req(input$SampleFile)
    inDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
    drillpath <- paste0(inDir, '/', input$SampleFile, "_Drill_Data.csv")
    if(file.exists(drillpath)){
      df <- read.csv(drillpath, stringsAsFactors = F)
      RV$CurrentCovDrillData <- df
    }
  })
  
  output$DrillSamplesTable <-renderRHandsontable({
    
   req(RV$CurrentCovDrillData)
    rhandsontable(RV$CurrentCovDrillData,  height = 600, manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
    
  })
  

  output$ExploreSamplesMap <- renderLeaflet({

    req(RV$CurrentSampleData)

    inDF <-RV$CurrentSampleData
    att <- input$SampleAtt
    sdf2 <- data.frame(SiteID=inDF$SiteID,Easting=inDF$Easting, Northing=inDF$Northing, Val=inDF[att] )
    sdf3 <- na.omit(sdf2)

    sdf4 <- unique(sdf3[c("SiteID", "Easting", "Northing")])
    head(sdf4)
    coordinates(sdf4) <- ~Easting+Northing
    crs(sdf4) <- crs('+proj=utm +zone=46 +ellps=WGS84 +units=m +no_defs ')
    psdf <- spTransform(sdf4, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    

    leaflet() %>%

      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%

      addCircleMarkers(
        data=psdf,
        group = "Soil Samples",
        radius = 2) %>%

      addLayersControl(
        baseGroups = c("Satelite Image"),
        overlayGroups = c("Soil Samples"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  output$ExploreAttSummaryTable <- renderTable({
    req(input$SampleAtt,  RV$CurrentSampleData )

    s <- summary(RV$CurrentSampleData[input$SampleAtt][[1]])

    dfs <- data.frame(Stat=names(s), Value=round(as.numeric(s[1:7]), digits=4), stringsAsFactors = F)
    # vad <- length(which(!is.na(mydf2$pH_H2O)))
    # dfs[8,] <- c('# Valid Vals', vad)
    # dfs[7,1] <- c('# NA Vals')
    dfs
  })
  
  
  output$ExploreAttPlotHist <- renderPlot({
   req(input$SampleAtt)
   fld <- na.omit(RV$CurrentSampleData[input$SampleAtt])
   hist(fld)
 }) 
 
  output$ExploreAttPlotBox <- renderPlot({
   req(input$SampleAtt)
   fld <- na.omit(RV$CurrentSampleData[input$SampleAtt])
   boxplot(fld)
 }) 
  
  
  output$ExploreAttPlotQNorm <- renderPlot({
   req(input$SampleAtt)
   fld <- na.omit(as.numeric(RV$CurrentSampleData[input$SampleAtt][[1]]))

   qqnorm(fld, plot.it = TRUE, pch = 4, cex = 0.7)
   qqline(fld, col="red", lwd = 2)
 }) 
 
  
  output$UploadSamplesTable <- renderRHandsontable({
    req(RV$CurrentSampleData)
      rhandsontable(RV$CurrentSampleData,  height = 600, manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
  })
  
  
 
  
  
  observeEvent( input$ProcessSampleData, {

    #isolate({
      
      sdf <-RV$CurrentRawSiteData
      
      sampflds <-  sdf[, input$sampsDataFields]
        
      sampflds2 <- Filter(is.numeric, sampflds)
      
      if(input$sampsStratified != 'None' &  nchar(str_trim(input$sampsStratified))>1 ){
        strat <- sdf[input$sampsStratified]
      }else{
        strat <- rep('None', NROW(sdf))
      }
      
      if(input$sampsTransect != 'None' &  nchar(str_trim(input$sampsStratified))>1 ){
        trans <- sdf[input$sampsTransect]
      }else{
        trans <- rep('None', NROW(sdf))
      }
      
      newdf <- data.frame(SiteID=sdf[input$sampsSiteID],
                          Easting=sdf[input$sampsLongitude],
                          Northing=sdf[input$sampsLatitude],
                          Depth=sdf[input$sampsDepth],
                          Stratifiy=strat,
                          Transect=trans,
                          sampflds2
                         
      )
      
      
      colnames(newdf)[1:6] <- c('SiteID', 'Easting', 'Northing', 'Depth', 'Stratify', 'Transect')
      
      newdf[newdf == -9999] <- NA
      
      RV$CurrentSampleDataFields <- input$sampsDataFields
      
      inDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
      spath <- paste0(inDir, '/', input$sampleFileRaw$name)
      write.csv(newdf, spath, row.names = F)
      
      RV$CurrentSampleDataName <- str_remove(input$sampleFileRaw$name, '.csv')
      updateSelectInput(session, 'SampleFile',  selected = RV$CurrentSampleDataName)
      
      RV$CurrentSampleData <- newdf

  })
  
  
  
  observe({
    
    req(input$SampleFile)
    fls <- getSampleFiles(input$currentProject)
    updateSelectInput(session, 'SampleFile', choices=fls, selected = RV$CurrentSampleDataName)
    fname <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples/', input$SampleFile, '.csv')
    if(file.exists(fname)){
      RV$CurrentSampleData <- read.csv(fname, stringsAsFactors = F)
    }
    
    updateSelectInput(session, 'ModelAttribute', choices=colnames(RV$CurrentSampleData))
  })
  

  
  observe({
    req(input$SampleFile)
    exflds <- c(1:5)
    nms <- colnames(RV$CurrentSampleData[-exflds])
    updateSelectInput(session, "SampleAtt", choices = nms )
    
  })
  
  observe({
    

    req(input$sampleFileRaw)
    
    RV$CurrentSampleData <- NULL
    
    isolate({
    
    df <- read.csv(input$sampleFileRaw$datapath,
                   
                   header = T,
                   sep = ',',
                   quote = '"',
                   stringsAsFactors = F)
    
    flds <-  colnames(df)
    
    lonInd <- unlist(sapply(c('lon', 'east', 'x'), grep, flds, ignore.case = T ))
    latInd <- unlist(sapply(c('lat', 'north', 'y'), grep, flds, ignore.case = T ))
    idInd <- unlist(sapply(c('Site', 'ID'), grep, flds, ignore.case = T ))
    # upperInd <- unlist(sapply(c('upper', 'up'), grep, flds, ignore.case = T ))
    # lowerInd <- unlist(sapply(c('lower', 'low'), grep, flds, ignore.case = T ))
    depthInd <- unlist(sapply(c('dep'), grep, flds, ignore.case = T ))
    idInd <- unlist(sapply(c('Site', 'ID'), grep, flds, ignore.case = T ))
    stratInd <- unlist(sapply(c('Strat' ), grep, flds, ignore.case = T ))
    transInd <- unlist(sapply(c('Trans'), grep, flds, ignore.case = T ))
    
    updateSelectInput(session, 'sampsSiteID', choices=flds, selected = flds[idInd][1])
    updateSelectInput(session, 'sampsLatitude', choices=flds, selected = flds[latInd][1])
    updateSelectInput(session, 'sampsLongitude', choices=flds, selected = flds[lonInd][1])
    
    updateSelectInput(session, 'sampsStratified', choices=c('None', flds), selected = flds[stratInd][1])
    updateSelectInput(session, 'sampsTransect', choices=c('None', flds), selected = flds[transInd][1])
    
    updateSelectInput(session, 'sampsDepth', choices=flds, selected = flds[depthInd][1])
    
    #mflds <- na.omit(c(lowerInd[1],upperInd[1], lonInd[1], latInd[1],idInd[1] ))
    mflds <- na.omit(c(depthInd[1], lonInd[1], latInd[1],idInd[1], stratInd[1], transInd[1]))
   
    
    updateSelectInput(session, 'sampsDataFields', choices=flds, selected = flds[-mflds] )
    
    RV$CurrentRawSiteData = df
    
    })
    
  })
  
  
  
  ###################   Explore Sample Data   #############################################3  
  
  output$ExploreSamplesTable <- renderRHandsontable({
    
    #req(RV$CurrentSampleData)
    #rhandsontable(RV$CurrentSiteData,  height = 600, manualColumnResize = T, readOnly = TRUE, rowHeaders = F) 
  })
  
  
  
  
  observe( {
   
    if(input$transformCov > 0){
          
          isolate({
            
            val <- as.numeric(input$transformCovValue)
            
            if(is.na(val)){
              shinyalert("Oops!", "Please provide a numeric value for the Transformation.", type = "error")
              return()
            }
            
            r <- RV$CurrentCovariate
            
            RV$CurrentCovariate <- NULL

          if(input$transformCovType == 'Power'){
            outR <- r^val
          }else if(input$transformCovType == 'Exp'){
            outR <- exp(r)
          }
          else if(input$transformCovType == 'Log'){
            outR <- log(r, val)
          }
          else if(input$transformCovType == 'Addition'){
            outR <- r + val
          }
          else if(input$transformCovType == 'Multiplication'){
            outR <- r * val
          }
          else if(input$transformCovType == 'Cos'){
            suffix <- 'Cos'
            outR <- cos(r)
          }
          else if(input$transformCovType == 'Tan'){
            outR <- tan(r)
          }
          else if(input$transformCovType == 'Sin'){
            outR <-sin(r)
          }else{
            outR <- NULL
          }
          
          if(!is.null(outR)){
            outDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' )
            
            
            if(is.null(input$transformCovNewName) | nchar(trim(input$transformCovNewName)) == 0){
              outname = paste0(input$availCovList, "_", input$transformCovType )
            }
            else{
              outname = paste0(input$transformCovNewName)
            }
            rt <- writeRaster(outR, paste0(outDir, '/', outname, '.tif'), overwrite=TRUE)
            aggregate(outR, rasterResFactor, filename= paste0(outDir, '/', outname, '_RedRes.tif'), overwrite=T)
            
            updateSelectInput(session, "availCovList", choices =  getCovariates(input$currentProject), selected = outname)
            
          }
          })
    }
    
  })
  
  

  
  
  
  observeEvent(input$CovariateDelete, {
    showModal(modalDialog(
      tagList(
        
        pickerInput(
          inputId = "covDeletePicker", 
          label = "Select/deselect all + format selected", 
          choices = names(getCovariateStack(input$currentProject)), 
          options = list(
            `actions-box` = TRUE, 
            size = 8,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        )
      ), 
      title="Delete Selected Covariates",
      footer = tagList(actionButton("confirmCovDelete", "Delete"),modalButton("Cancel")
      )
    ))
  })
  
  
  observeEvent(input$confirmCovDelete, {
    req(input$CovariateDelete)
    
    for (i in 1:length(input$covDeletePicker)) {
      fname <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates/', input$covDeletePicker[i], '.tif')
      unlink(fname)
      fname <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates/', input$covDeletePicker[i], '_RedRes.tif')
      unlink(fname)
    }
    updateSelectInput(session, "availCovList", choices =  getCovariates(input$currentProject))
    RV$ProjCovStack <- getCovariateStack(input$currentProject) 
    removeModal()
  })
  
  

  observeEvent(input$CovariateRename, {
    showModal(modalDialog(
      tagList(
        
        pickerInput(
          inputId = "covRenamePicker", 
          label = "Select covariate to rename", 
          choices = names(getCovariateStack(input$currentProject)), 
          options = list(
            `actions-box` = TRUE, 
            size = 8,
            `selected-text-format` = "count > 3"
          ), 
          multiple = F
        ),
        textInput('renameCovNewName', 'New name')
      ), 
      title="Rename Selected Covariate",
      footer = tagList(actionButton("confirmCovRename", "Rename"),modalButton("Cancel")
      )
    ))
  })
  
  
  observeEvent(input$confirmCovRename, {
    req(input$CovariateRename)
    
    origName <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates/', input$covRenamePicker, '.tif')
    newName <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates/', input$renameCovNewName, '.tif')
    if(file.exists(origName)){
      file.rename(origName, newName)
    }
    
    origNameRR <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates/', input$covRenamePicker, '_RedRes.tif')
    newNameRR <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates/', input$renameCovNewName, '_RedRes.tif')
    if(file.exists(origNameRR)){
      file.rename(origNameRR, newNameRR)
    }
    
    updateSelectInput(session, "availCovList", choices =  getCovariates(input$currentProject))
    RV$ProjCovStack <- getCovariateStack(input$currentProject) 
    removeModal()
  }) 
  
  
  
  
  
  
 ##### Project Management #####
  
 ### Delete an existing project
  observeEvent(input$DeleteProject, {
    showModal(modalDialog(
      tagList(
        selectInput("deleteProjSelect", label = "Delete a Project", choices = getProjects())
      ), 
      title="Delete a Project",
      footer = tagList(actionButton("confirmProjDelete", "Delete"),modalButton("Cancel")
      )
    ))
  })
  
  

  
  observeEvent(input$confirmProjDelete, {
    req(input$deleteProjSelect)
    deleteProject(input$deleteProjSelect)
    inDir <- paste0(rootDir, '/', currentUser)
    projects <- list.dirs(inDir, full.names = F, recursive = F)
    updateSelectInput(session, "currentProject", choices =  projects)
    RV$CurrentSampleData <- NULL
    RV$SampleFileData = NULL
    RV$ProjCovStack = NULL
    RV$CurrentCovariate = NULL
    RV$CurrentRawSiteData = NULL
    RV$CurrentSampleData = NULL
    RV$CurrentSampleDataName = NULL
    RV$CurrentSampleDataFields = NULL
    RV$CurrentCovDrillData = NULL
    RV$ModelSampleFileData = NULL
    RV$CurrentModel = NULL
    RV$ModelData = NULL
    RV$CurrentModelRaster = NULL
    
    updateSelectInput(session, "availCovList", choices =  getCovariates(input$currentProject))
    updateSelectInput(session, "SampleFile", choices =  getCovariates(input$currentProject))
    updateSelectInput(session, "SampleAtt", choices =  getCovariates(input$currentProject))
    
    removeModal()
  })
  
  
  ### Add a new project project
  observeEvent(input$AddNewProject, {
    showModal(modalDialog(
      tagList(
        textInput("newProjectname", label = "Project Name", placeholder = "NewProject"),
        fileInput("chooseTemplate", "Choose Spatial Template GeoTiff", multiple = F,  accept = c("image/tiff", ".tif"))
      ), 
      title="Create a Project",
      footer = tagList(actionButton("confirmCreateProject", "Create"), modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirmCreateProject, {
    req(input$newProjectname, input$chooseTemplate)
    createNewProject(input$newProjectname, input$chooseTemplate$datapath)
    inDir <- paste0(rootDir, '/', currentUser)
    projects <- list.dirs(inDir, full.names = F, recursive = F)
    updateSelectInput(session, "currentProject", choices =  projects, selected = input$newProjectname)
    
    RV$CurrentSampleData <- NULL
    RV$SampleFileData = NULL
    RV$ProjCovStack = NULL
    RV$CurrentCovariate = NULL
    RV$CurrentRawSiteData = NULL
    RV$CurrentSampleData = NULL
    RV$CurrentSampleDataName = NULL
    RV$CurrentSampleDataFields = NULL
    RV$CurrentCovDrillData = NULL
    RV$ModelSampleFileData = NULL
    RV$CurrentModel = NULL
    RV$ModelData = NULL
    RV$CurrentModelRaster = NULL
    
    updateSelectInput(session, "availCovList", choices =  getCovariates(input$currentProject))
    
    removeModal()
  })
  
  
  
  
  ### Rename an existing project
  observeEvent(input$RenameProject, {
    showModal(modalDialog(
      tagList(
        selectInput("renameProjFrom", label = "From Project Name", choices = getProjects()),
        textInput("renameProjectTo", label = "To Project Name", placeholder = "NewProject")
      ), 
      title="Create a Project",
      footer = tagList(actionButton("confirmRenameProject", "Rename"), modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirmRenameProject, {
    req(input$renameProjFrom)
    renameProject(input$renameProjFrom, input$renameProjectTo)
    inDir <- paste0(rootDir, '/', currentUser)
    projects <- list.dirs(inDir, full.names = F, recursive = F)
    updateSelectInput(session, "currentProject", choices =  projects)
    removeModal()
  })
  
  
  # modelPath <- reactive({
  #   
  #   mp <- NULL
  #   if(is.null(input$ModelName) | nchar(input$ModelName) < 1){
  #     ModelName <- paste0(input$SampleFile, '!', input$ModelAttribute, '!', input$ModelDepth)
  #   }else{
  #     ModelName <- input$ModelName
  #   }
  #   outDir <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', ModelName)
  #   modelPath<- paste0(outDir, "/model!", ModelName, ".rds")
  #   
  #   if(file.exists(modelPath)){
  #     mp <- modelPath
  #   }else{
  #     mp <- ''
  #   }
  #   
  # })
  
  observe({
    
   # req(modelPath)
    
    
    
    
  #   if(is.null(input$ModelName) | nchar(input$ModelName) < 1){
  #     ModelName <- paste0(input$SampleFile, '!', input$ModelAttribute, '!', input$ModelDepth)
  #   }else{
  #     ModelName <- input$ModelName
  #   }
  #   outDir <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', ModelName)
  #   modelPath<- paste0(outDir, "/model!", ModelName, ".rds")
  # 
  #   if(file.exists(modelPath)){
  #     RV$CurrentModelPath <- modelPath
  #   }
  #   
  # })
  # 
  # output$RunModelGenerateMap <- renderText({
  #   
  #   req(RV$CurrentModel)
  #   
  #  
  #   
  #   RV$ShowMapGenProgress <- T
  #   
  #   stk <- getCovariateStack(input$currentProject)
  #   templateR <- getTemplate(input$currentProject)
  #   isolate({ModelName <- RV$CurrentModelName})
  #   scratchDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/tmpData')
  #   createDirectory(scratchDir)
  #   tmpfls <- list.files(scratchDir, full.names =T)
  #   unlink(tmpfls)
  #   scratchOutRaster <-  paste0(scratchDir, '/model!', ModelName, '.tif' )
  #   
  #   outDir <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', ModelName)
  #   createDirectory(outDir)
  #   
  #   depth <- input$ModelDepth
  #   
  #   modelR <- makeMapParra(model=RV$CurrentModel, templateR, stk, depth, outRasterName=scratchOutRaster, numCPUs=NULL, minBlocks=30, tidyUp=F)
  #   
  #   modPath <- paste0(outDir, '/model!', ModelName, '.rds')
  #   
  #   writeRaster(modelR, paste0(outDir, '/model!', ModelName, '.tif'), overwrite=TRUE)
  #   saveRDS(RV$CurrentModel, modPath)
  #   ms <- summary(RV$CurrentModel)
  #   cat(ms$output, file = paste0(outDir, '/model!', ModelName, '.txt'))
  #   
  #   #RV$ShowMapGenProgress <- F
  #   
  #   RV$CurrentModelRaster <- modelR
  #   RV$CurrentModelPath <- modPath
  #   
  #   return(" ")
    
  })
  
  
  # output$MapGenProgress <- renderText( {
  #   
  # 
  #   if(RV$ShowMapGenProgress){
  #     #paste0("AAAAAAAAAAAAAAAAAAAAA")
  #     paste0("<div width=200>The map is being generated<br><img src='mapGenLoader.gif'></div")
  #   }else{
  #     paste0(" ")
  #   }
  # })
  
  observe({
    
    req(input$ExistingModels)
        
        modelPath <- paste0(getwd(), '/', rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', input$ExistingModels, '/Model!', input$ExistingModels, '.rds' )
        
        if(file.exists(modelPath)){
          model <- readRDS(modelPath)
          if(!is.null(model)){
              print(modelPath)
              modName <- getModelNameFromPath(modelPath)
              modRasterPath <- str_replace(modelPath, '.rds', '.tif')
              modelR <- raster(modRasterPath)
              
              RV$CurrentModel <- model
              RV$CurrentModelRaster <- modelR
              RV$CurrentModelPath <- modelPath
          }
        }
  })
  
  
  output$RunModelMap <- renderLeaflet({
    
    req(RV$CurrentModelPath)
    
    ModelName <- getModelNameFromPath(RV$CurrentModelPath)
    
    if(UseReducedResRasters){
      rasterPath <- str_replace(RV$CurrentModelPath, '.rds', '_RedRes.tif')
      
    }else{
      rasterPath <- str_replace(RV$CurrentModelPath, '.rds', '.tif')
    }
    
    
    
    if(file.exists(rasterPath)){

        modelR <- raster(rasterPath)
        pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(modelR), na.color = "transparent")
        leaflet() %>%
          addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
          addRasterImage(modelR, colors = pal, opacity = 0.8) %>%
          leaflet::addLegend(pal = pal, values = values(modelR), title = ModelName )
        
    }
      
  })
  
  

  
  ### Run the model
  observeEvent(  input$RunModelBtn, {
    
    req(RV$CurrentCovDrillData, RV$CurrentSampleData)
    
   # RV$ShowMapGenProgress <-T
    
    if(is.null(input$ModelName) | nchar(input$ModelName) < 1){
      ModelName <- paste0(input$SampleFile, '!', input$ModelAttribute, '!', input$ModelDepth)
    }else{
      ModelName <- input$ModelName
    }
    
    RV$CurrentModelName <- ModelName
    modOutDir <-paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Models/', ModelName )
    
    if(!dir.exists(modOutDir)){
      dir.create(modOutDir, recursive = T)
    }else{
        fls <- list.files(modOutDir, full.names = T)
        unlink(fls)
    }
    
    outDir <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', ModelName)
    if(!dir.exists(outDir)){
      dir.create(outDir, recursive = T)
    }else{
      fls <- list.files(outDir, full.names = T)
      unlink(fls)
    }
    
    
   
   
    
   
    tmpfls <- list.files(outDir, full.names =T)
    flds <- colnames(RV$CurrentCovDrillData)
    
    covNames <- names(getCovariateStack(input$currentProject))
    mismatch <- which(!covNames %in% flds)
    
    if(length(mismatch>0)){
      shinyalert("Oops!", "It looks like there are some new covariates that have not been drilled as yet. Please go back to the covariate drill tab and redrill your covariates", type = "error")
      return()
    }
    
    
    attInd<- which(flds==input$ModelAttribute)
    DSM_datasub<- RV$CurrentCovDrillData [c(1, 2, 3, 5, 6,  attInd, 4, (ncol(RV$CurrentSampleData )+1):ncol(RV$CurrentCovDrillData))]
    DSM_datasub<- DSM_datasub[complete.cases(DSM_datasub),]
    DSM_datasub$folds<- NA

## remove any missing values
      foldIndex<- which(names(DSM_datasub)=="folds")
      #which(!complete.cases(DSM_datasub[,-foldIndex]))
      
      nrow(DSM_datasub)
      
      # stratified k-fold cross-validation
      reps <- as.numeric(input$ModelRepsNum)
      folds <- as.numeric(input$ModelFoldNum)
      iterts <- reps*folds
      
      
      #tables to put goof outputs
      cGoof<- matrix(NA, nrow=reps*folds, ncol=5)
      vGoof<- matrix(NA, nrow=reps*folds, ncol=5)
      fmat<- matrix(NA, nrow=reps, ncol=folds)
      ObsVsModelled <- data.frame(Observed=numeric(), Modelled=numeric())
      
      cnt=1 # iteration counter
      #i=1
      
      for (r in 1:reps){
        
        ## permutate the folds in the dataset
        # split the data into different strata
        # sp_DSM_datasub<- split(DSM_datasub, DSM_datasub$strata)
        #for (split in 1:length(sp_DSM_datasub)){
        dats1<- DSM_datasub
        sel<- nrow(dats1)
        
        #random purmutation
        d<- as.factor(rep(sample(1:folds,folds) ,length.out=sel))
        p<-sample(1:sel,sel)
        d<-d[order(p)]
        
        # attribute each fold to each observation on transect
        DSM_datasub$folds<- d
        
        
        # melt the lists to a single data frame
        #DSM_datasub<- do.call(rbind.data.frame, sp_DSM_datasub)
        fmat[r,]<- t(as.matrix(summary(as.factor(DSM_datasub$folds))))
        
        ## do the k-fold cross-validation
       # fcnt<- 1
        for (f in 1:folds){

          updateProgressBar(
            session = session,
            id = "modelRunProgress",
            value = cnt, total = iterts,
            title = paste("Training the model...")
          )
         
          cDat<- DSM_datasub[DSM_datasub$folds != f,] # calibration data
          vDat<- DSM_datasub[DSM_datasub$folds == f,] # validation data
          
          #fit the model on the whole dataset
          df <- cDat[, c(7:(ncol(cDat)-1))]
          cub.mod<- cubist(x = df, y = cDat[, 6], cubistControl(rules = input$ModelNumRules, extrapolation = input$ModelExtrapThresh), committees = 1, label = input$ModelAttribute)

          modelFile<- paste(modOutDir, "/kfold_mod_", "r", r, "_f", f, "_.rds", sep="")
          saveRDS(object = cub.mod, file = modelFile)
          
          ## Goodness of fit
          #Internal validation
          Cubist.pred.C <- predict(cub.mod, newdata = cDat[, c(7:(ncol(cDat)-1))])
          cGoof[cnt,]<- as.matrix(goof(observed = cDat[, 6] , predicted = Cubist.pred.C ))
          
          #External validation
          Cubist.pred.V <- predict(cub.mod, newdata = vDat[, c(7:(ncol(cDat)-1))])
          vGoof[cnt,]<- as.matrix(goof(observed = vDat[,6], predicted = Cubist.pred.V ))
          
          itdf <- data.frame(Observed=vDat[,6], Modelled=Cubist.pred.V)
          ObsVsModelled <- rbind(ObsVsModelled,itdf )
          
          cnt<- cnt+1
          
        }
        
      }
      
     
      
      df <- DSM_datasub[, c(7:(ncol(cDat)-1))]

      cub.mod <- cubist(x = df, y = DSM_datasub[, 6], cubistControl(rules = input$ModelNumRules, extrapolation = input$ModelExtrapThresh), committees = 1)
      modelFile<- paste0(outDir, "/Model!", ModelName, ".rds")
      saveRDS(object = cub.mod, file = modelFile)
      
      cGoof<- as.data.frame(cGoof)
      names(cGoof)<- c("R2", "concordance", "mse", "rmse", "bias")
      vGoof<- as.data.frame(vGoof)
      names(vGoof)<- c("R2", "concordance", "mse", "rmse", "bias")
      write.csv(cGoof, paste0(outDir, "/model!", ModelName, "!InternalValidation.csv"), row.names = F) 
      write.csv(vGoof, paste0(outDir, "/model!", ModelName, "!ExternalValidation.csv"), row.names = F) 
      write.csv(ObsVsModelled, paste0(outDir, "/model!", ModelName, "!ObsVsMod.csv"), row.names = F)
    
    stk <- getCovariateStack(input$currentProject)
    templateR <- getTemplate(input$currentProject)
    isolate({ModelName <- RV$CurrentModelName})
    scratchDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/tmpData')
    createDirectory(scratchDir)
    tmpfls <- list.files(scratchDir, full.names =T)
    unlink(tmpfls)
    scratchOutRaster <-  paste0(scratchDir, '/model!', ModelName, '.tif' )
    
    outDir <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', ModelName)
    createDirectory(outDir)
    
    depth <- input$ModelDepth
    
    rName <- basename(scratchOutRaster)
    
    
    model<-cub.mod

    covNamesinModel <- getCovariatesUsedInModel(model)
    
    numCPUs = detectCores()-1
    cat(paste0('Using ', numCPUs, ' cores'), sep='\n')
    
    
    bs <- blockSize(stk, minblocks = minBlocks)
    saveRDS(bs, paste0(scratchDir, '/brickInfo.rds'))
    cat(paste0('Using ', bs$n, ' blocks'), sep='\n')
    cl <- makeSOCKcluster(numCPUs)
    registerDoSNOW(cl)
    
    
    pp <- ceiling(minBlocks/numCPUs)
    
    cntr=1
    lcnt <-0
    for (i in 1:pp) {
      
      updateProgressBar(
        session = session,
        id = "modelRunProgress",
        value = i, total = pp,
        title = paste("Generating Map....")
      )
      
      totCnt <- min((i * numCPUs), minBlocks)
      kst <- lcnt*numCPUs+1
      r <- foreach(k=kst:totCnt,  .packages=c('raster','rgdal', 'Cubist'), .export = c('applyMapParallelWithDepth')) %dopar% applyMapParallelWithDepth(model, templateR, stk, depth, scratchDir, bs, covNamesinModel)
      lcnt <- lcnt + 1
      cntr <- cntr+1
    }
    
    stopCluster(cl)
    
    modelR <- writeRasterFromFilesSimple(templateR=templateR, rasterDir=scratchDir, outRaster=rName)
    
    tidyUp=T
    if(tidyUp){
      unlink(scratchDir, recursive = T)
    }

    modPath <- paste0(outDir, '/model!', ModelName, '.rds')
    
    writeRaster(modelR, paste0(outDir, '/model!', ModelName, '.tif'), overwrite=TRUE)
    
    #if(UseReducedResRasters){
      aggregate(modelR, rasterResFactor, filename=paste0(outDir, '/model!', ModelName, '_RedRes.tif'))
    #}


    ms <- summary(model)
    cat(ms$output, file = paste0(outDir, '/model!', ModelName, '.txt'))
    
    updateProgressBar(
      session = session,
      id = "modelRunProgress",
      value = 0, total = 0,
      title = paste("")
    )
    
    #updateTabsetPanel(session, inputId="inTabset", selected="DownloadDataPanel")
    updateSelectInput(session, 'ExistingModels', choices = getModelNames(input$currentProject),  selected = ModelName)

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
    req(input$currentProject, input$SampleFile)
    state <- read.csv(paste0(rootDir, '/', currentUser, '/state.cfg'), sep = "=", header = F )
    
  })
  
  
  
  
  
  observe({
    inDir <- paste0(rootDir, '/', currentUser)
    projects <- list.dirs(inDir, full.names = F, recursive = F)
    updateSelectInput(session, "currentProject", choices =  projects, selected =  previousProj)
  })
  

  
  observe({
    req(RV$ProjCovStack)
    updateSelectInput(session, "availCovList", choices =  names(RV$ProjCovStack))
  })
  
  
  
  
  observe({
    
    req(input$covariateFiles)

    covnames <- str_remove(input$covariateFiles$name, '.tif')
    templateR <- getTemplate(input$currentProject)
    templateProj <- crs(templateR)

    
    
    pTot <- length(input$covariateFiles$datapath) * 3
    cntr <- 1
    
    isolate({
      for (i in 1:length(input$covariateFiles$datapath)) {
        
        f <- input$covariateFiles$datapath[i]
        inR<- raster(f)
        
        if(!compareCRS(templateR, inR)){
          inR2 <- projectRaster(inR, crs=templateProj,method="ngb")
        }else{
          inR2 <- inR
        }
        
        updateProgressBar(session = session, id = "covariateProcessingProgress", value = cntr, total = pTot, title = paste("Processing Covariate Rasters ...."))
        cntr<-cntr+1
        
        if(!compareRaster(templateR, inR, stopiffalse = F, extent = T, rowcol = T, res = T, orig = T)){
          
          inR3 <- resample(inR2, templateR)
        }else{
          inR3 <- inR2
        }
        
        
        updateProgressBar(session = session, id = "covariateProcessingProgress", value = cntr, total = pTot, title = paste("Processing Covariate Rasters ...."))
        cntr<-cntr+1
        
        inR4 <- mask(inR3, templateR)
        #plot(inR4)
        covDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' )
        writeRaster(inR4, paste0(covDir, '/', covnames[i], '.tif'), overwrite=T)
        aggregate(inR4, rasterResFactor, filename= paste0(covDir, '/', covnames[i], '_RedRes.tif'), overwrite=T)
        
        updateProgressBar(session = session, id = "covariateProcessingProgress", value = cntr, total = pTot, title = paste("Processing Covariate Rasters ...."))
        cntr<-cntr+1
      } 
      updateProgressBar(session = session, id = "covariateProcessingProgress", value = 0, total = 0, title = paste(""))
      
      RV$ProjCovStack <- getCovariateStack(input$currentProject) 
    })
    
  })
  
  
  output$covImage<- renderImage({
    
    req(RV$ProjCovStack )
    
    nc <- 3
    psize = 10
    marg = 0.1
    
    stk <- getCovariateStack(input$currentProject, UseReducedResRasters)
    
    nlayers(stk)
    names(stk) <- str_remove(names(stk), '_RedRes')
    nr <- ceiling(nlayers(stk)/nc)
    
    
    outfile <- tempfile(pattern = 'covs_', fileext = '.png')
    #outfile <- paste0( rootDir, '/', currentUser, '/', input$currentProject, '/tmpData/AllCovsPlot.png')

    png(filename = outfile,   width = psize * nc, height = psize * nr, units = "cm", res=150)
    nf <- layout(matrix(seq(1,(nc*nr)), nr, nc, byrow = TRUE), respect = TRUE, widths=c(lcm(psize),lcm(psize)), heights=c(lcm(psize),lcm(psize)))
    
    for (i in 1:nlayers(stk)) {
      plot(stk[[i]],  main=paste0(names(stk)[i]), cex.main=2)
    }
    
    dev.off()
    #dev.off()
    
    list(src = outfile,
        contentType = 'image/png',
        width = psize * nc * 30,
        height =  psize * nr * 30,
        alt = "Available Covariate Rasters")
    
  }, deleteFile = T)
  
  
  # output$covPlots <- renderPlot({
  #   
  # 
  #   req(RV$ProjCovStack )
  #   stk <- getCovariateStack(input$currentProject, UseReducedResRasters)
  #   
  #   nc <- 3
  #   psize = 2
  #   marg = .5
  #   
  #   nlayers(stk)
  #   nr <- ceiling(nlayers(stk)/nc)
  #   
  #   
  #   png(filename = paste0( 'c:/temp/aaaplot.png'),   width = psize * nc, height = psize * nr, units = "cm", res=150)
  #   
  #   #windows()
  #   par(mar=c(marg,marg,marg,marg))
  #   nf <- layout(matrix(seq(1,(nc*nr)), nr, nc, byrow = TRUE), respect = TRUE, widths=c(lcm(psize),lcm(psize)), heights=c(lcm(psize),lcm(psize)))
  #   
  #   p<- NULL
  #   for (i in 1:nlayers(stk)) {
  #     
  #   p <-  plot(stk[[i]],  main=paste0(names(stk)[i]))
  #     
  #     
  #   }
  #   dev.off()
  #   
  #   p
  #   
  #   
  #  # plot(stk)
  # })
  
  # output$SplinePlot <- renderPlot({
  #   req(RV$CurrentSplineFile)
  #   
  #   
  #   spls <- RV$CurrentSplineFile
  #   
  #   stdDeps <- c('2.5', '10', '22.5', '45', '80', '150')
  #   
  #   sid <- which(spls$harmonised$id == as.numeric(input$SampleSite))
  #   pstd <- spls$harmonised[sid,]
  #   
  #   d = spls$var.1cm[,sid]
  #   minx <- min(d) - min(d) * 0.2
  #   maxx <- max(d) + max(d) * 0.2
  #   plot( d, 1:length(d), type="n", main=paste( paste0('Spline of ', input$SampleAtt, ' at ', input$SampleSite)), xlab = input$SampleAtt, ylab = "Depth", yaxs = "i", xaxs = "i", xlim = c(minx, maxx), ylim = rev(range(c(0,200))))
  #   lines( d, 1:length(d), type="l") 
  #   stdVals <- c(spls$harmonised$`0-5 cm`[sid], spls$harmonised$`5-15 cm`[sid], spls$harmonised$`15-30 cm`[sid], spls$harmonised$`30-60 cm`[sid], spls$harmonised$`60-100 cm`[sid], spls$harmonised$`100-200 cm` [sid])
  #   stdPts <- data.frame(stdDeps, stdVals)
  #   points(stdVals, stdDeps, col='red', pch=19)
  #   
  # })
  
  
  
  observe({
    req(input$currentProject )
    
    # createDirectory(paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' ))
    # createDirectory(paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' ))
    # createDirectory(paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Outputs' ))
    
    covDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' )
    paths <- list.files( paste0(covDir), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.tif$'), full.names = T, recursive =F)
    if(length(paths) > 0){
      RV$ProjCovStack <- getCovariateStack(input$currentProject)
    }else{
      RV$ProjCovStack <- NULL
    }
  })
  
  
  
  
 
  
  
  # output$UploadSamplesTable <- renderRHandsontable({
  #   
  #   req(input$sampleFile)
  #   isolate({
  #     sampName <- str_remove(input$sampleFile$name, '.csv')
  #     
  #     sampDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
  #     outPath <- paste0(sampDir, '/', sampName, '.csv')
  #     file.copy(input$sampleFile$datapath, outPath)
  #   })
  #   
  #   
  #   inDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
  #   paths <- list.files( paste0(inDir), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.csv$'), full.names = F, recursive =F)
  #   if(length(paths) > 0)
  #   {
  #     updateSelectInput(session, "SampleFile", choices =  str_remove(paths, '.csv'))
  #   }
  #   
  #   tryCatch(
  #     {
  #       df <- read.csv(outPath,
  #                      header = T,
  #                      sep = ',',
  #                      quote = '"',
  #                      stringsAsFactors = F)
  #       
  #       stddf <- df[with(df, order(sid, Attribute, UpperDepth)),]
  #       
  #       RV$SampleFields <- stddf
  #       
  #       atts <- unique(stddf$Attribute)
  #       
  #       
  #       spDir <- paste0(inDir, '/', sampName)
  #       createDirectory(paste0(spDir))
  #       for (i in 1: length(atts)) {
  #         att <- atts[i]
  #         spdf <- stddf[stddf$Attribute==att, c(1,4,5,7)]
  #         sp.fit <- ea_spline(spdf, var.name='Val')
  #         saveRDS(sp.fit, paste0(spDir, '/', att, '_spline.rds'))
  #         
  #       }
  #       
  #       rhandsontable(df,  height = 600, manualColumnResize = T, readOnly = TRUE, rowHeaders = F) 
  #     },
  #     error = function(e) {
  #       stop(safeError(e))
  #     }
  #   )
  # })
  
  
  
  
  
  # output$UploadSamplesMap <- renderLeaflet({
  #   
  #   req(RV$SampleFileData )
  #   
  #   inDF <- RV$SampleFileData 
  #   att <- input$SampleAtt
  #   sdf <- inDF[inDF$Attribute == att, ]
  #   coordinates(sdf) <- ~x+y
  #   
  #   leaflet() %>%
  #     
  #     addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
  #     
  #     addCircleMarkers(
  #       data=sdf,
  #       group = "Soil Samples",
  #       radius = 2) %>%
  #     
  #     addLayersControl(
  #       baseGroups = c("Satelite Image"),
  #       overlayGroups = c("Soil Samples"),
  #       options = layersControlOptions(collapsed = FALSE)
  #     ) 
  # })
  
  
  
  observe({
    
    req(input$availCovList)
    
    covName <- input$availCovList
    
    isolate({
      covDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' )
    })
    rPath <- paste0(covDir, '/', covName, '.tif')
    r <- raster(rPath)
    
    
    RV$CurrentCovariate <- r
    
  })
  
  
  output$CovariateMap <- renderLeaflet({
    
    if(!is.null(RV$CurrentCovariate)){
      
      if(UseReducedResRasters){
        fn <- filename(RV$CurrentCovariate)
        fn2 <- str_replace(fn, '.tif', '_RedRes.tif')
        r <- raster(fn2)
      }else{
        r <- RV$CurrentCovariate
      }
    
    #r <- RV$CurrentCovariate
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r), na.color = "transparent")
    
    #if(!is.null(r)){
    leaflet() %>%
      
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
      addRasterImage(r, colors = pal, opacity = 0.8) %>%
      leaflet::addLegend(pal = pal, values = values(r), title = input$availCovList)
    }
  })
  
  output$covDesc <- renderText({
    req(RV$CurrentCovariate)
    r <- RV$CurrentCovariate
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







