Version = '0.1.0'
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
library(ranger)
library(Cubist)
library(png)
library(shinyBS)


source('DSMToolsConfig.R')
source('mylibs/GeneralUtils.R')
source('mylibs/VectorUtils.R')
source('mylibs/MapCubistModels.R')
source('mylibs/ithirSub.R')
source("helpers.R")


### Remember some states from the last session
stf <- paste0(rootDir, '/', currentUser, '/state.cfg')
if(file.exists(stf)){
state <- suppressWarnings( read.csv(stf, sep = "=", header = F ))

previousProj <- str_trim(as.character(state[1,2]))
previousSamples <- str_trim(as.character(state[2,2]))
}else{
  previousProj <- NULL
  previousSamples <- NULL
}

cat('yep', file = 'appisrunning.txt')



######################################   UI   ###################################################
ui <- tagList(fluidPage(
  
  
  useShinyjs(),
  tags$style(appCSS),
  useShinyalert(),
  
  tags$head(
    ### icon for web server version
    #tags$link( rel="icon", type="image/png", href="layers.png", sizes="32x32" ),
    ### icon for standalone version
    tags$link( rel="icon", type="image/png", href="http://localhost:1984/layers.png", sizes="32x32" ),
            tags$head(tags$script(src = "message-handler.js")),
            tags$title("DSM Tools"),
            HTML("<img src=soilcores2.PNG style='vertical-align: top;'>")
            
  ),
  
  navbarPage("", id = "inTabset", 
             
             tabPanel("Select a Project",  icon = icon("list-ul"),
                    
                    sidebarLayout(
                      sidebarPanel(width = 3, 
                                   wellPanel(HTML('<p style="color:blue;font-weight: bold;">Project Management</p>'),
                                             fluidRow(selectInput('currentProject', 'Select a Project to Use', choices = NULL)
                                                      ,bsTooltip("currentProject", "Select an existing project to use from this list")),
                                             fluidRow( actionButton('AddNewProject', "Create New Project", class = "btn-success")
                                                       ,bsPopover("AddNewProject", "Create a new project. You need to specify a name and a Template raster"), br(), br(), br(), br()),
                                             fluidRow( actionButton('DeleteProject', "Delete Project", class = "btn-info")), br(),
                                             fluidRow( actionButton('RenameProject', "Rename Project", class = "btn-info"))
                                             )
                      ),
                      mainPanel(
                        fluidPage(
                          fluidRow( HTML('<H1><p style="color:blue;font-weight: bold;">Project Information</p></H1>')),
                        fluidRow(br(), br()),
                        
                        fluidRow(htmlOutput('projectInfoText1')),
                        #fluidRow(plotOutput('projectInfoTemplateImage', width = "250", height = "250")),
                       
                        fluidRow(htmlOutput('projectInfoText2'))
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
                                               withSpinner(plotOutput('covPlots', width=1000, height=1000))
                                               #withSpinner(imageOutput("covImage"))
                                               
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
                                                                     actionButton('transformCov', "Transform", class = "btn-success"),br(),br()
                                                          ),
                                                          fluidRow( actionButton('CovariateDelete', "Delete Covariates", class = "btn-info")),br(),
                                                          fluidRow( actionButton('CovariateRename', "Rename a Covariate", class = "btn-info"))
                                                          
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
                                                                    actionButton("ProcessSampleData","Import Sample Data", class = "btn-success"),br(),br(),
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
                                               withSpinner( verbatimTextOutput("SampleSummaryInfo")),
                                               withSpinner( rHandsontableOutput("UploadSamplesTable"))
                                               
                                               #withSpinner(leafletOutput("UploadSamplesMap", width = "650", height = "450"))
                                             )
                                           ) 
                                  ),
                                  tabPanel("Explore Sample Data",  icon = icon("glasses"), 
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
                                               withBusyIndicatorUI( actionButton("DrillSampleData","Drill Rasters", class = "btn-success"))
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
                                     #textInput('ModelName', 'Model Name'),
                                     textAreaInput("ModelDescription", "Model Description"),
                                     selectInput("ModelTrainingMethod", "Training Method", choices = c('Simple Random','Stratified', 'Transect Lumped')),
                                     awesomeCheckbox(inputId = "ModelRunDoUncert", 
                                                     label = "Generate Uncertainty Bounds. (This will take much longer to generate)", 
                                                     value = F),
                                     actionButton(inputId = "RunModelBtn", label = "Run Model", class = "btn-success"),
                                     
                                     br(),  br(),
                                     progressBar(id = "modelRunProgress", value = 0, total = 100,  title = "", display_pct = TRUE ),
                                     
                                     actionLink("toggleModelOptions", "Toggle the following text"),
                                     conditionalPanel(
                                       condition = "input.toggleModelOptions % 2 == 1",
                                       
                                       numericInput("ModelNumRules", "Max No. Rules", value=DefNumCubistRules, min=1, max=200),
                                       numericInput("ModelExtrapThresh", "Extrapolation %", value=defCubistExtrapThresh, min=1, max=200),
                                       numericInput("ModelFoldNum", "Number of Folds", value=DefNumFolds, min=1, max=50),
                                       numericInput("ModelRepsNum", "Number of Reps", value=DefNumReps, min=1, max=50)
                                     )
                                     )
                                    
                        ),
                        mainPanel(
                          fluidPage(
                            fluidRow( 
                              
                              actionLink("toggleModelDetails", "Show the Model Setup Details"),
                              conditionalPanel(
                                condition = "input.toggleModelDetails % 2 == 1",
                                verbatimTextOutput('ModelDetails'))
                            ),
                              withSpinner(leafletOutput("RunModelMap", width = "650", height = "450")),
                              
                              #),
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
                       
                        uiOutput("ModelDownloadButtonRUI"),
                        uiOutput("ModelDownloadLnk")
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
  

  dir.create(paste0(rootDir, '/', currentUser, '/', projname), recursive = T)
  dir.create(paste0(rootDir, '/', currentUser, '/', projname, '/Covariates'))
  dir.create(paste0(rootDir, '/', currentUser, '/', projname, '/Outputs'))
  dir.create(paste0(rootDir, '/', currentUser, '/', projname, '/Samples'))
  dir.create(paste0(rootDir, '/', currentUser, '/', projname, '/tmpData'))
  dir.create(paste0(rootDir, '/', currentUser, '/', projname, '/GeoTemplate'))
  

  
  r <- raster(templateRasterTempPath)
  
  templatePath <- paste0(rootDir, '/', currentUser, '/', projname, '/GeoTemplate/Template.tif' )
  writeRaster(r, templatePath)
  
  m <- r/r
  pol <- rasterToPolygons(m, dissolve=TRUE)

  polPath <- paste0(rootDir, '/', currentUser, '/', projname, '/GeoTemplate' )
  writeOGR(pol, polPath, 'Template', driver="ESRI Shapefile")
  
  # nr <- nrow(r)  
  # nc <- ncol(r)  
  # ds <- 5
  # z <- nc/ds
  # marg<-0.01
  # par(mai=c(marg,marg,marg,marg))
  # par(oma=c(marg,marg,marg,marg))
  # 
  # imagefile <- paste0(rootDir, '/', currentUser, '/', projname, '/GeoTemplate/Template.png' )
  # png(filename = imagefile,   width = nc/10, height = nr/10, units = "cm", res=150)
  # plot(r, legend=FALSE, axes=FALSE, box=FALSE)
  # dev.off()
  
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
  nms2 <- paths[!grepl('_Drill_Data', paths, ignore.case = T)]
  if(length(nms2) >0){
  str_remove(nms2 , '.csv')
  }else{
    NULL
  }
}

getCovariateStack <- function(projname, reducedRes=F){
  covDir <- paste0(rootDir, '/', currentUser, '/', projname, '/Covariates' )
  if(reducedRes){
  covpaths <- list.files( covDir, pattern = paste0( '_RedRes.tif$'), full.names = T, recursive =F)
  }else{
    rpaths <- list.files( covDir, pattern = paste0( '.tif$'), full.names = T, recursive =F)
    covpaths <- rpaths[!grepl('_RedRes', rpaths, ignore.case = T)]
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
  if(length(nms)>0){
  m1 <- str_remove(nms , '.rds')
  m2 <- str_remove(m1, 'Model!')
  #m3 <- str_split(m2, '!')
  m2
  }else{
    NULL
  }
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
  
  cat('yep', file = 'appisrunning.txt')
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
  options(shiny.maxRequestSize=30*1024^2, shiny.usecairo=FALSE)
  
  
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
  
 
  
  
  output$SampleSummaryInfo<- renderText({
    
    t <- ''

    
    req(RV$CurrentSampleData)
    
    # exflds <- c(1:6)
    # df <- RV$CurrentSampleData[, -exflds]

    t <- paste0(t, 'Total No. of samples = ', nrow(RV$CurrentSampleData))

    t
  })
  
  
  output$ModelDownloadButtonRUI<- renderUI({
    
    if(currentUser != 'LocalPC'){
      downloadButton('ModelDownloadBtn', 'Download', class = "btn-success")
      #actionLink("ModelDownloadLnkReal","Show Data") 
    }
    
  })

  output$ModelDownloadLnk <- renderUI({
    
    if(currentUser == 'LocalPC'){
      actionLink("ModelDownloadLnkReal","Show Data") 
    }
    
  })
  
  observeEvent(input$ModelDownloadLnkReal,{
    
    req(input$ModelDownloadSelect)
    if(input$ModelDownloadSelect == 'Model Package'){
      ModelName <- paste0(input$SampleFile, '!', input$ModelAttribute, '!', input$ModelDepth)
      dPath <- paste0( getwd(), '/', rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', ModelName)

    }else if(input$ModelDownloadSelect == 'Covariates'){
      dPath <- paste0( getwd(), '/', rootDir, '/', currentUser, '/', input$currentProject, '/Covariates')
    }else if(input$ModelDownloadSelect == 'Sample Files'){
      dPath <- paste0( getwd(), '/', rootDir, '/', currentUser, '/', input$currentProject, '/Samples')
    }else{
      dPath <- paste0( getwd(), '/', rootDir, '/', currentUser, '/', input$currentProject)
    }
    dPathF <- str_replace_all(dPath, '/', '\\\\')
    suppressWarnings( shell(paste0("explorer ", dPathF), intern=TRUE))
  })
  
  observe({
    
    updateSelectInput(session, "ExistingModels", choices =  getModelNames(input$currentProject))
    
  }) 
  
  
  output$textLabel_CovariateUsage <- renderText({
    req(RV$CurrentModel)
    paste0(('<br><H3>Model Covariate Usage</H3><br>'))
  })
  
  output$textLabel_ModelFitStats <- renderText({
    req(RV$CurrentModel)
    paste0(('<br><H3>Model Fit Stats</H3><br>'))
  })
  
  output$textLabel_InternalFoldStats<- renderText({
    req(RV$CurrentModel)
    paste0(('<br><H3>Internal Model Fold Stats</H3><br>'))
  })
  
  output$textLabel_ExternalFoldStats<- renderText({
    req(RV$CurrentModel)
    paste0(('<br><H3>External Model Fold Stats</H3><br>'))
  })
  
  output$textLabel_ModelSummary<- renderText({
    req(RV$CurrentModel)
    paste0(('<br><H3>Model Summary</H3><br>'))
  })
  
  
  
  output$projectInfoText1 <- renderText({
    req(input$currentProject)
    r <- getTemplate(input$currentProject)
      
    rsum <- ''
    rsum <- paste0(rsum, '<H4><b>Geographic Template</b></H4>')
    rsum <- paste0(rsum, '<p>Dimensions  : ', dim(r)[1], ' ',dim(r)[2], ' ', dim(r)[1] * dim(r)[2],  ' (nrow, ncol, ncell)<br>')
    rsum <- paste0(rsum, 'Resolution  : ',format(round(res(r)[1], 5), nsmall = 2) , ' ',format(round(res(r)[2], 5), nsmall = 2), ' (x, y)<br>')
    rsum <- paste0(rsum, 'Extent      : ', format(round(extent(r)[1], 5), nsmall = 2), ' ',format(round(extent(r)[2], 5), nsmall = 2), ' ', format(round(extent(r)[3], 5), nsmall = 2), ' ', format(round(extent(r)[4], 5), nsmall = 2), '    (xmin, xmax, ymin, ymax) <br>')
    rsum <- paste0(rsum, 'Projection : ', crs(r), '<br>')
    rsum <- paste0(rsum, '</p><br>')
    
    rsum
  })
  
  output$projectInfoTemplateImage <- renderPlot({
    r <- getTemplate(input$currentProject)
    plot(r, legend=FALSE, axes=FALSE, box=FALSE)
    
  })
  
  # output$projectInfoTemplateImage <- renderImage({
  #   req(input$currentProject)
  #   imagefile <- paste0(getwd(), '/', rootDir, '/', currentUser, '/', input$currentProject, '/GeoTemplate/Template.png' )
  # 
  #   return(list(src = imagefile)
  #          )
  #   
  # }, deleteFile = F)
  
  output$projectInfoText2 <- renderText({
    req(input$currentProject)
    
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
    
    
    rsum <- paste0(rsum, '<H4><b>Covariates</b></H4>')
    if(is.null(covs)){
      rsum <- paste0(rsum, '<ul>None</ul><br>')
    }else{
      rsum <- paste0(rsum, '<ul>', covsAsList, '</ul><br>')
    }
    
    rsum <- paste0(rsum, '<H4><b>Soil Sample Files</b></H4>')
    #rsum <- paste0(rsum, '<ul>', sampsAsList, '</ul><br>')
    if(is.null(samps)){
      rsum <- paste0(rsum, '<ul>None</ul><br>')
    }else{
      rsum <- paste0(rsum, '<ul>', sampsAsList, '</ul><br>')
    }
    rsum <- paste0(rsum, '<H4><b>Existing Models</b></H4>')

    #rsum <- paste0(rsum, '<ul>', modelsAsList, '</ul')
    if(is.null(models)){
      rsum <- paste0(rsum, '<ul>None</ul><br>')
    }else{
      rsum <- paste0(rsum, '<ul>', modelsAsList, '</ul><br>')
    }
    
    rsum
    
    
  })
  
  
  
  output$ModelDownloadBtn <- downloadHandler(
    
    

    filename = function() {
      req(input$ModelDownloadSelect)
      d <- input$ModelDownloadSelect
      #if(is.null(input$ModelName) | nchar(input$ModelName) < 1){
        ModelName <- input$ExistingModels
      # }else{
      #   ModelName <- input$ModelName
      # }
      
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
      
      #if(is.null(input$ModelName) | nchar(input$ModelName) < 1){
        ModelName <- input$ExistingModels
      # }else{
      #   ModelName <- input$ModelName
      # }
      
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
     
      zipfn <- paste0( getwd(), '/', rootDir, '/', currentUser, '/', input$currentProject, '/tmpData/data.zip')

      tmpD <-paste0( rootDir, '/', currentUser, '/', input$currentProject, '/tmpData/zipData')
      dir.create(tmpD, recursive = T)
      zflsDel <- list.files(tmpD, full.names = T, recursive = F)
      unlink(zflsDel)
      file.copy(fls, tmpD)
      zfls <- list.files(tmpD, full.names = F, recursive = F)

      oldwd <- getwd()
      setwd(tmpD)

      zip::zip(zipfn, zfls, recurse=F, compression_level=1)
      setwd(oldwd)

      file.copy(paste0(zipfn), file)
    }
  ) 
  
  
  
  
  
  observeEvent(input$ModelDownloadSelect, {
    
    req(input$ModelDownloadSelect)
    d <- input$ModelDownloadSelect
    
    # if(is.null(input$ModelName) | nchar(input$ModelName) < 1){
       ModelName <- paste0(input$SampleFile, '!', input$ModelAttribute, '!', input$ModelDepth)
    # }else{
    #  ModelName <- input$ModelName
    #}

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
        
        # cccC <- epi.ccc(vt$Observed, vt$Modelled, ci = "z-transform",conf.level = 0.95)
        # r.sqC <- cor(vt$Observed, vt$Modelled)^2
        gof <- goof(observed = vt$Observed, predicted = vt$Modelled )
        cccC <- gof$concordance
        r.sqC <- gof$R2
        # cccC <- epi.ccc(vt$Observed, vt$Modelled, ci = "z-transform",conf.level = 0.95)
        # r.sqC <- cor(vt$Observed, vt$Modelled)^2
        
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
          ty1 =  maxY * 0.99
          ty2 =  maxY  * 0.94
          
          legPos='bottomright'
          #legend(legPos, c('Regression Line') , lty=1, col=c('red'), cex=1, bty="o", bg = 'gray90' )
          legend(legPos, c('Regression Line', '1:1') , lty=1, col=c('red','green'), bty='o', cex=1, bg = 'gray90')
          text(tx,ty1, paste("R2 = ",round(r.sqC, digits = 2)), pos=4)
          text(tx,ty2, paste("LCCC = ", round(cccC, digits = 2)), pos=4)
          
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
  
  
  output$ModelDetails<- renderText({
    req(RV$CurrentModelPath)
    dPath <-paste0( dirname(RV$CurrentModelPath), '/ModelDescription.txt')
    if(file.exists(dPath)){
      l <- readLines(dPath)
      paste0(l, collapse = '\n')
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

      DSM_data<- raster::extract(cvstack, SoilData, sp = 1, method = "simple")
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
    templateR <- getTemplate(input$currentProject)
    crs(sdf4) <- crs(templateR)
    psdf <- reproject(sdf4)
    
    inBdyPath <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/GeoTemplate' )
    bdy <- readOGR(inBdyPath, layer = "Template", GDAL1_integer64_policy = TRUE)
    
  
    leaflet() %>%

      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%

      addCircleMarkers(
        data=psdf,
        group = "Soil Samples",
        radius = 2) %>%
      addPolygons(data=bdy, color = "red", fillColor = NULL,  opacity = 1, fillOpacity = 0) %>%

      addLayersControl(
        baseGroups = c("Satelite Image"),
        overlayGroups = c("Soil Samples"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  output$ExploreAttSummaryTable <- renderTable({
    req(input$SampleAtt,  RV$CurrentSampleData )

    fld <- RV$CurrentSampleData[input$SampleAtt][[1]]
    s <- summary(fld)

    dfs <- data.frame(Stat=names(s), Value=round(as.numeric(s[1:7]), digits=4), stringsAsFactors = F)

     validCnt <- length(which(!is.na(fld)))
     naCnt <- length(which(is.na(fld)))
     dfs[7,] <- c('# Locations', length(unique(RV$CurrentSampleData$SiteID)))
     dfs[8,] <- c('# Records', length(fld))
     dfs[9,] <- c('# Valid Vals', validCnt)
     dfs[10,] <- c('# NA Vals', naCnt)
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
  
  observe({
    
    input$currentProject
    #req(RV$CurrentSampleDataName)
    samps <- getSampleFiles(input$currentProject)
    updateSelectInput(session, 'SampleFile', choices = samps, selected = RV$CurrentSampleDataName)
  })
  
  observe({
    req(RV$CurrentSampleDataFields)
    updateSelectInput(session, 'SampleAtt', choices = RV$CurrentSampleDataFields)
  })
  
 
  
  
  observeEvent( input$ProcessSampleData, {

    #isolate({
      
      sdf <-RV$CurrentRawSiteData
      
      
      sampflds <-  sdf[, input$sampsDataFields]
        
      sampflds2 <- Filter(is.numeric, sampflds)
      # chki <- which(sapply(sampflds2, is.integer) )
      # sampflds2[chki] <- lapply(sampflds2[chki], as.numeric)

      
      
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
                          sampflds2,
                          stringsAsFactors = F
                         
      )
      
      
      
      colnames(newdf)[1:6] <- c('SiteID', 'Easting', 'Northing', 'Depth', 'Stratify', 'Transect')
      
      newdf[newdf == -9999] <- NA
      
      RV$CurrentSampleDataFields <- input$sampsDataFields
      
      inDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples' )
      spath <- paste0( inDir, '/', input$sampleFileRaw$name)
      write.csv(newdf, spath, row.names = F)

      RV$CurrentSampleDataName <- str_remove(input$sampleFileRaw$name, '.csv')
     

      ## check if the location overlay the geographic template
      sdf4 <- unique(newdf[c("SiteID", "Easting", "Northing")])
      coordinates(sdf4) <- ~Easting+Northing
      templateR <- getTemplate(input$currentProject)
      crs(sdf4) <- crs(templateR)
      psdf <- reproject(sdf4)
      #deleteShapefile(paste0(inDir, '/', RV$CurrentSampleDataName, '.shp'))
      unlink(paste0(inDir, '/', RV$CurrentSampleDataName, '.shp'))
      unlink(paste0(inDir, '/', RV$CurrentSampleDataName, '.dbf'))
      unlink(paste0(inDir, '/', RV$CurrentSampleDataName, '.prj'))
      unlink(paste0(inDir, '/', RV$CurrentSampleDataName, '.shx'))
      writeOGR(psdf, inDir, RV$CurrentSampleDataName, driver="ESRI Shapefile")
      
      inBdyPath <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/GeoTemplate' )
      bdy <- readOGR(inBdyPath, layer = "Template", GDAL1_integer64_policy = TRUE)
      crs(bdy) <- crs(psdf)
      
      a<-over(psdf, bdy)
      
      
      if(all(is.na(a))){
        shinyalert("Oops!", "It looks as if none of the points in this sample file overlay your area of interest.", type = "error")
      }
      
      
      

      RV$CurrentSampleData <- newdf


  })
  
  
  
  observe({
    
    req(input$SampleFile)
    fls <- getSampleFiles(input$currentProject)
    updateSelectInput(session, 'SampleFile', choices=fls, selected = RV$CurrentSampleDataName)
    fname <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Samples/', input$SampleFile, '.csv')
    if(file.exists(fname)){
      df <- read.csv(fname, stringsAsFactors = F)
      exflds <- c(1:6)
      atts <- df[-exflds]
      chki <- which(sapply(atts, is.integer) )
      atts[chki] <- lapply(atts[chki], as.numeric)

      RV$CurrentSampleData <- data.frame(df[1:6], atts, stringsAsFactors = F)
      
      nms <- colnames(RV$CurrentSampleData[-exflds])
      updateSelectInput(session, 'ModelAttribute', choices=nms)
    }
    
    
  })
  

  
  observe({
    req(input$SampleFile)
    exflds <- c(1:6)
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
      footer = tagList(actionButton("confirmCovDelete", "Delete", class = "btn-info"),modalButton("Cancel")
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
      footer = tagList(actionButton("confirmCovRename", "Rename", class = "btn-info"),modalButton("Cancel")
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
      footer = tagList(actionButton("confirmProjDelete", "Delete", class = "btn-info"),modalButton("Cancel")
      )
    ))
  })
  
  

  
  observeEvent(input$confirmProjDelete, {
    req(input$deleteProjSelect)
    deleteProject(input$deleteProjSelect)
    inDir <- paste0(rootDir, '/', currentUser)
    projects <- list.dirs(inDir, full.names = F, recursive = F)
    updateSelectInput(session, "currentProject", choices =  projects)

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
      footer = tagList(actionButton("confirmCreateProject", "Create", class = "btn-success"), modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirmCreateProject, {
    req(input$newProjectname, input$chooseTemplate)
    createNewProject(input$newProjectname, input$chooseTemplate$datapath)
    inDir <- paste0(rootDir, '/', currentUser)
    projects <- list.dirs(inDir, full.names = F, recursive = F)
    updateSelectInput(session, "currentProject", choices =  projects, selected = input$newProjectname)
    

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
      footer = tagList(actionButton("confirmRenameProject", "Rename", class = "btn-info"), modalButton("Cancel")
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
  
  
  
  observe({
    
    req(input$ExistingModels)
        
        modelPath <- paste0(getwd(), '/', rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', input$ExistingModels, '/Model!', input$ExistingModels, '.rds' )
        
        if(file.exists(modelPath)){
          model <- readRDS(modelPath)
          if(!is.null(model)){
              modName <- getModelNameFromPath(modelPath)
              
              RV$CurrentModel <- model
              RV$CurrentModelPath <- modelPath
              
              modRasterPath <- str_replace(modelPath, '.rds', '.tif')
              if(file.exists(modRasterPath)){
                  #RV$CurrentModelRaster <- modelR
                  modelR <- raster(modRasterPath)
              }
              
             
          }
        }
  })
  
  
  output$RunModelMap <- renderLeaflet({
    
    req(RV$CurrentModelPath)
    
    ModelName <- getModelNameFromPath(RV$CurrentModelPath)
    
    if(UseReducedResRasters){
      rasterPath <- str_replace(RV$CurrentModelPath, '.rds', '_RedRes.tif')
      covPath <- str_replace(RV$CurrentModelPath, '.rds', '_CoV_RedRes.tif')
      fifPercPath <- str_replace(RV$CurrentModelPath, '.rds', '_5thPerc_RedRes.tif')
      ninfifPercPath <- str_replace(RV$CurrentModelPath, '.rds', '_95thPerc_RedRes.tif')
      rangePath <- str_replace(RV$CurrentModelPath, '.rds', '_UncertRange_RedRes.tif')
      
    }else{
      rasterPath <- str_replace(RV$CurrentModelPath, '.rds', '.tif')
      covPath <- str_replace(RV$CurrentModelPath, '.rds', '_CoV.tif')
      fifPercPath <- str_replace(RV$CurrentModelPath, '.rds', '_5thPerc.tif')
      ninfifPercPath <- str_replace(RV$CurrentModelPath, '.rds', '_95thPerc.tif')
      rangePath <- str_replace(RV$CurrentModelPath, '.rds', '_UncertRange_RedRes.tif')
    }
    
    lg <-c()
    
    
   modelR <- raster(rasterPath)
   resultPal <- colorNumeric("Spectral", values(modelR), na.color = "transparent")
   
   lg <-c(lg, "Model Result")
   modMap <- leaflet(modelR) %>%
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>% 
     addRasterImage(modelR, colors = resultPal, opacity = 1,  group = "Model Result") 
   

     if(file.exists(covPath)){
       modelRCov <- raster(covPath)
       #palCoV <- colorNumeric(c("green", 'black', 'yellow'), values(modelRCov), na.color = "transparent")
       coVPal <- colorNumeric('YlOrRd', values(modelRCov), na.color = "transparent")
       modMap <- addRasterImage(modMap, modelRCov, colors = coVPal, opacity = 1, group = "Model CoV")  
       lg <-c(lg, "Model CoV")
     }
   
     if(file.exists(fifPercPath)){
       modelRfif <- raster(fifPercPath)
       #palCoV <- colorNumeric(c("green", 'black', 'yellow'), values(modelRCov), na.color = "transparent")
       fifPal <- colorNumeric('Purples', values(modelRfif), na.color = "transparent")
       modMap <- addRasterImage(modMap, modelRfif, colors = fifPal, opacity = 1, group = "5th Percentile")  
       lg <-c(lg, "5th Percentile")
     }
   
     if(file.exists(ninfifPercPath)){
       modelRninfif <- raster(ninfifPercPath)
       #palCoV <- colorNumeric(c("green", 'black', 'yellow'), values(modelRCov), na.color = "transparent")
       ninfifPal <- colorNumeric('Purples', values(modelRninfif), na.color = "transparent")
       modMap <- addRasterImage(modMap, modelRninfif, colors = ninfifPal, opacity = 1, group = "95th Percentile")  
       lg <-c(lg, "95th Percentile")
     }
   
     if(file.exists(rangePath)){
       modelRrange <- raster(rangePath)
       #palCoV <- colorNumeric(c("green", 'black', 'yellow'), values(modelRCov), na.color = "transparent")
       rangePal <- colorNumeric('Purples', values(modelRrange), na.color = "transparent")
       modMap <- addRasterImage(modMap, modelRrange, colors = rangePal, opacity = 1, group = "Uncert Range")  
       lg <-c(lg, "Uncert Range")
     }
   
   modMap <- addLayersControl(modMap,
          baseGroups = c("Satelite Image"),
          #overlayGroups = c('Model Result', 'Model CoV'),
          overlayGroups = c(lg),
           options = layersControlOptions(collapsed = FALSE)
         ) 
   
   
   
   modMap <- leaflet::addLegend(modMap, pal = resultPal, values = values(modelR), title = "Model Result", group = "Model Result" )
   if(file.exists(covPath)){
     modMap <- leaflet::addLegend(modMap, pal = coVPal, values = values(modelRCov), title = "Model CoV", group = "Model CoV" )
     modMap <- leaflet::hideGroup(modMap, "Model CoV")
   }
   if(file.exists(fifPercPath)){
     modMap <- leaflet::addLegend(modMap, pal = fifPal, values = values(modelRfif), title = "5th Percentile", group = "5th Percentile" )
     modMap <- leaflet::hideGroup(modMap, "5th Percentile")
   }
   if(file.exists(ninfifPercPath)){
     modMap <- leaflet::addLegend(modMap, pal = ninfifPal, values = values(modelRninfif), title = "95th Percentile", group = "95th Percentile" )
     modMap <- leaflet::hideGroup(modMap, "95th Percentile")
   }
   if(file.exists(rangePath)){
     modMap <- leaflet::addLegend(modMap, pal = rangePal, values = values(modelRrange), title = "Uncert Range", group = "Uncert Range" )
     modMap <- leaflet::hideGroup(modMap, "Uncert Range")
   }
   
   modMap
      
  })
  
  

  
  ### Run the model
  observeEvent(  input$RunModelBtn, {
    
    req(RV$CurrentCovDrillData, RV$CurrentSampleData)
    
    RV$CurrentModelPath <- NULL
    
   # RV$ShowMapGenProgress <-T
    
    #if(is.null(input$ModelName) | nchar(input$ModelName) < 1){
      ModelName <- paste0(input$SampleFile, '!', input$ModelAttribute, '!', input$ModelDepth)
    # }else{
    #   ModelName <- input$ModelName
    # }
    
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
    
    
    attInd <- which(flds==input$ModelAttribute)
    DSM_datasub<- RV$CurrentCovDrillData [c(1, 2, 3, 5, 6,  attInd, 4, (ncol(RV$CurrentSampleData )+1):ncol(RV$CurrentCovDrillData))]
    DSM_datasub<- DSM_datasub[complete.cases(DSM_datasub),]
    DSM_datasub$folds<- NA

## remove any missing values
      foldIndex<- which(names(DSM_datasub)=="folds")
      #which(!complete.cases(DSM_datasub[,-foldIndex]))
      
     # nrow(DSM_datasub)
      
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
      
      for (r in 1:input$ModelRepsNum){
        
        if(input$ModelTrainingMethod == 'Simple Random'){

        dats1<- DSM_datasub
        sel<- nrow(dats1)
        #random purmutation
        d<- as.factor(rep(sample(1:folds,folds) ,length.out=sel))
        p<-sample(1:sel,sel)
        d<-d[order(p)]
        
        # attribute each fold to each observation on transect
        DSM_datasub$folds<- d
        
        
        }else if(input$ModelTrainingMethod == 'Stratified'){
     
          sp_DSM_datasub<- split(DSM_datasub, DSM_datasub$Stratify)
          for (split in 1:length(sp_DSM_datasub)){
            dats1<- sp_DSM_datasub[[split]]
            sel<- length(unique(dats1$Transect))
            
            #random purmutation
            d<- as.factor(rep(sample(1:folds,folds) ,length.out=sel))
            p<-sample(1:sel,sel)
            d<-d[order(p)]
            
            # attribute each fold to each observation on transect
            selu<- unique(dats1$Transect)
            for (att in 1:length(unique(dats1$Transect))){
              sp_DSM_datasub[[split]][which(dats1$Transect==selu[att]),"folds"] <- d[att]
            }}
          
          # melt the lists to a single data frame
          DSM_datasub<- do.call(rbind.data.frame, sp_DSM_datasub)
          
        } else if(input$ModelTrainingMethod == 'Transect Lumped'){

          dats1<- DSM_datasub
          sel<- length(unique(dats1$Transect))
          
          #random purmutation
          d<- as.factor(rep(sample(1:folds,folds) ,length.out=sel))
          p<-sample(1:sel,sel)
          d<-d[order(p)]
          
          # attribute each fold to each observation on transect
          selu<- unique(dats1$Transect)
          for (att in 1:length(unique(dats1$Transect))){
            DSM_datasub[which(dats1$Transect==selu[att]),"folds"] <- d[att]
          }
      }
        
        # melt the lists to a single data frame
        #DSM_datasub<- do.call(rbind.data.frame, sp_DSM_datasub)
        fmat[r,]<- t(as.matrix(summary(as.factor(DSM_datasub$folds))))
        
        ## do the k-fold cross-validation
       # fcnt<- 1
        for (f in 1:input$ModelFoldNum){

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
      write.csv(cGoof, paste0(outDir, "/Model!", ModelName, "!InternalValidation.csv"), row.names = F) 
      write.csv(vGoof, paste0(outDir, "/Model!", ModelName, "!ExternalValidation.csv"), row.names = F) 
      write.csv(ObsVsModelled, paste0(outDir, "/Model!", ModelName, "!ObsVsMod.csv"), row.names = F)
    
    stk <- getCovariateStack(input$currentProject)
    templateR <- getTemplate(input$currentProject)
    isolate({ModelName <- RV$CurrentModelName})
    scratchDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/tmpData')
    createDirectory(scratchDir)
    tmpfls <- list.files(scratchDir, full.names =T)
    unlink(tmpfls)
    #scratchOutRaster <-  paste0(scratchDir, )
    
    outDir <-  paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Outputs/', ModelName)
    createDirectory(outDir)
    
    depth <- input$ModelDepth
    rName <- paste0('Model!', ModelName, '.tif' )
    
    numCPUs = detectCores()-1
    cat(paste0('Using ', numCPUs, ' cores'), sep='\n')
    
    cl<-makeCluster(numCPUs)
    registerDoParallel(cl)
    bs <- blockSize(stk, minblocks = numChunks)
    minBlocks <- bs$n
    pp <- ceiling(minBlocks/numCPUs)
    saveRDS(bs, paste0(scratchDir, '/brickInfo.rds'))
    cat(paste0('Using ', bs$n, ' blocks'), sep='\n')

    

    if(!input$ModelRunDoUncert){
      
      model<-cub.mod
      covNamesinModel <- getCovariatesUsedInModel(model)
      
      cntr=1
      lcnt <-0
      for (i in 1:pp) {
        updateProgressBar(session = session,id = "modelRunProgress",value = i, total = pp,title = paste("Generating Map...."))
        totCnt <- min((i * numCPUs), minBlocks)
        kst <- lcnt*numCPUs+1
        r <- foreach(k=kst:totCnt,  .packages=c('raster','rgdal', 'Cubist'), .export = c('applyMapParallelWithDepth')) %dopar% applyMapParallelWithDepth(model, templateR, stk, depth, scratchDir, bs, covNamesinModel)
        lcnt <- lcnt + 1
        cntr <- cntr+1
        }
      
      rpath <- paste0(outDir, '/Model!', ModelName, '.tif')
      modelR <- writeRasterFromFilesSimple(templateR=templateR, rasterDir=scratchDir, outRaster=rpath)
      aggregate(modelR, rasterResFactor, filename=paste0(outDir, '/Model!', ModelName, '_RedRes.tif'))
      
    }else{
      
      mPfs <- list.files(modOutDir, '.rds', full.names = T)
      models <- lapply(mPfs,function(i){ readRDS(i)})
      covsinMod <- lapply(models,function(i){ getCovariatesUsedInModel(i)})
      covNamesinModel <- unique(do.call(c, covsinMod))
      
      cntr=1
      lcnt <-0
      for (i in 1:pp) {
        updateProgressBar(session = session,id = "modelRunProgress",value = i, total = pp,title = paste("Generating Map...."))
        totCnt <- min((i * numCPUs), minBlocks)
        kst <- lcnt*numCPUs+1
        r <- foreach(k=kst:totCnt,  .packages=c('raster','rgdal', 'Cubist'), .export = c('applyMapParallelWithDepthBooty')) %dopar% applyMapParallelWithDepthBooty(models, templateR, stk, depth, scratchDir, bs, covNamesinModel,doCIs=T)
        lcnt <- lcnt + 1
        cntr <- cntr+1
      }
      
      
      updateProgressBar(session = session,id = "modelRunProgress",value = 0, total = 5, title = paste("Generating Uncert Products"))
      rpath <- paste0(outDir, '/Model!', ModelName, '.tif')
      modelR <- writeRasterFromFilesSimple(templateR=templateR, rasterDir=scratchDir, outRaster=rpath, outType = 'mean')
      aggregate(modelR, rasterResFactor, filename=paste0(outDir, '/Model!', ModelName, '_RedRes.tif'), overwrite = T)
      
      updateProgressBar(session = session,id = "modelRunProgress",value = 0, total = 5, title = paste("Generating Uncert Products"))
      rpathCoV <- paste0(outDir, '/Model!', ModelName, '_CoV.tif')
      modelRCoV <- writeRasterFromFilesSimple(templateR=templateR, rasterDir=scratchDir, outRaster=rpathCoV, outType = 'CoV')
      aggregate(modelRCoV, rasterResFactor, filename=paste0(outDir, '/Model!', ModelName, '_CoV_RedRes.tif'), overwrite = T)
      
      updateProgressBar(session = session,id = "modelRunProgress",value = 2, total = 5, title = paste("Generating Uncert Products"))
      rpath5<- paste0(outDir, '/Model!', ModelName, '_5thPerc.tif')
      modelR5 <- writeRasterFromFilesSimple(templateR=templateR, rasterDir=scratchDir, outRaster=rpath5, outType = '5')
      aggregate(modelR5, rasterResFactor, filename=paste0(outDir, '/Model!', ModelName, '_5thPerc_RedRes.tif'), overwrite = T)
      
      updateProgressBar(session = session,id = "modelRunProgress",value = 3, total = 5, title = paste("Generating Uncert Products"))
      rpath95 <- paste0(outDir, '/Model!', ModelName, '_95thPerc.tif')
      modelR95 <- writeRasterFromFilesSimple(templateR=templateR, rasterDir=scratchDir, outRaster=rpath95, outType = '95')
      aggregate(modelR95, rasterResFactor, filename=paste0(outDir, '/Model!', ModelName, '_95thPerc_RedRes.tif'), overwrite = T)
      
      updateProgressBar(session = session,id = "modelRunProgress",value = 4, total = 5, title = paste("Generating Uncert Products"))
      rpathRange <- paste0(outDir, '/Model!', ModelName, '_UncertRange.tif')
      udif <- modelR95 - modelR5
      writeRaster(udif, rpathRange, overwrite=T)
      aggregate(udif, rasterResFactor, filename=paste0(outDir, '/Model!', ModelName, '_UncertRange_RedRes.tif'), overwrite = T)
      
    }
   
    stopCluster(cl)
    
    tidyUp=T
    if(tidyUp){
      unlink(scratchDir, recursive = T)
    }

    modPath <- paste0(outDir, '/Model!', ModelName, '.rds')
    model <- readRDS(modPath)
    ms <- summary(model)
    cat(ms$output, file = paste0(outDir, '/Model!', ModelName, '.txt'))
    
    descPath <- paste0(outDir, '/ModelDescription.txt')
    cat('Model Name : ', ModelName, '\n', sep = '', append = F, file = descPath)
    cat('Run on :  ', format(Sys.time()), '\n', sep = '', append = T, file = descPath)
    si <- Sys.info()
    
    cat('Run by :  ', si[['user']], '\n', sep = '', append = T, file = descPath)
    cat('Project : ', input$currentProject, '\n', sep = '', append = T, file = descPath)
    cat('Sample File : ', input$SampleFile, '\n', sep = '', append = T, file = descPath)
    cat('Attribute : ', input$ModelAttribute, '\n', sep = '', append = T, file = descPath)
    cat('Depth : ', input$ModelDepth, '\n', sep = '', append = T, file = descPath)
    cat('Description : ', input$ModelDescription, '\n', sep = '', append = T, file = descPath)
    cat('\n', sep = '', append = T, file = descPath)
    
    cat('\nTraining Data\n', sep = '', append = T, file = descPath)
    cat('--------------\n', sep = '', append = T, file = descPath)
    cat('Number of Samples : ', nrow(DSM_datasub), '\n', sep = '', append = T, file = descPath)
    cat('Number of Locations: ', length(unique(DSM_datasub$SiteID)), '\n', sep = '', append = T, file = descPath)
    cat('Minimum ', input$ModelAttribute, ' value : ', min(DSM_datasub[,6]), '\n', sep = '', append = T, file = descPath)
    cat('Maximum ', input$ModelAttribute, ' value : ', max(DSM_datasub[,6]), '\n', sep = '', append = T, file = descPath)
    cat('Mean ', input$ModelAttribute, ' value : ', mean(na.omit(DSM_datasub[,6])), '\n', sep = '', append = T, file = descPath)
    cat('Std Dev of ', input$ModelAttribute, ' values : ', sd(DSM_datasub[,6], na.rm = T), '\n', sep = '', append = T, file = descPath)
    cat('Depth Range : ', min(DSM_datasub$Depth), ' to ', max(DSM_datasub$Depth), '\n', sep = '', append = T, file = descPath)
    cat('\n', sep = '', append = T, file = descPath)
    
    cat('\nCovariates\n', sep = '', append = T, file = descPath)
    cat('----------\n', sep = '', append = T, file = descPath)
    covs <- getCovariateStack(input$currentProject)
    covsAsList <- paste0( names(covs),  collapse = '\n')
    cat(covsAsList, '\n\n', sep = '', append = T, file = descPath)
    
    cat('\nModel Parameters\n', sep = '', append = T, file = descPath)
    cat('----------------\n', sep = '', append = T, file = descPath)
    cat('Model Type : ', input$MLType, '\n', sep = '', append = T, file = descPath)
    cat('Training Method : ', input$ModelTrainingMethod, '\n', sep = '', append = T, file = descPath)
    cat('Max No. Rules : ', input$ModelNumRules,'\n', sep = '', append = T, file = descPath)
    cat('Model Extrapolation Threshold : ', input$ModelExtrapThresh,'\n', sep = '', append = T, file = descPath)
    cat('Number of Folds : ', input$ModelFoldNum,'\n', sep = '', append = T, file = descPath)
    cat('Number of Reps : ', input$ModelRepsNum,'\n', sep = '', append = T, file = descPath)
    cat('Generate Uncertainties : ', input$ModelRunDoUncert,'\n', sep = '', append = T, file = descPath)
    
    cat('\n\nModel Outputs\n', sep = '', append = T, file = descPath)
    cat('----------------\n', sep = '', append = T, file = descPath)
    fls <- list.files(outDir)
    flsAsList <- paste0( fls,  collapse = '\n')
    cat(flsAsList, '\n', sep = '', append = T, file = descPath)
    cat('\n', sep = '', append = T, file = descPath)
    cat('\n', sep = '', append = T, file = descPath)
    cat('\n', sep = '', append = T, file = descPath)
    
   
    
  
    
    updateProgressBar(session = session,id = "modelRunProgress",value = 0, total = 0, title = paste(""))
    
    updateSelectInput(session, 'ExistingModels', choices = getModelNames(input$currentProject),  selected = ModelName)
    RV$CurrentModelPath <- modelFile

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
  
  
  # observe({
  #   req(input$currentProject, input$SampleFile)
  #   stf <- paste0(rootDir, '/', currentUser, '/state.cfg')
  #   if(file.exists(stf)){
  #     state <- read.csv(stf, sep = "=", header = F )
  #   }
  #   
  # })
  
  
  
  
  
  observe({
    inDir <- paste0(rootDir, '/', currentUser)
    projects <- list.dirs(inDir, full.names = F, recursive = F)
    updateSelectInput(session, "currentProject", choices =  projects, selected =  previousProj)
  })
  
  observe({
    inDir <- paste0(rootDir, '/', currentUser)
    samps <- getSampleFiles(input$currentProject)
    updateSelectInput(session, "SampleFile", choices =  samps, selected =  previousSamples)

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
  
  
  output$covPlots<- renderPlot({
    
    req(input$currentProject)
    
    RV$ProjCovStack
    stk <- getCovariateStack(input$currentProject)
    
    if(!is.null(stk)){
    
    stk <- getCovariateStack(input$currentProject, UseReducedResRasters)
    plot(stk, nc = 4, nr = 5)
    }else{
      NULL
    }
  })
  
  output$covImage<- renderImage({


    req(RV$ProjCovStack )

    stk <- getCovariateStack(input$currentProject, UseReducedResRasters)


    nc <- 3
    psize = 10
    marg = 0.1

    par(mai=c(marg,marg,marg,marg))

    stk <- getCovariateStack(input$currentProject, UseReducedResRasters)

    nlayers(stk)
    names(stk) <- str_remove(names(stk), '_RedRes')
    nr <- ceiling(nlayers(stk)/nc)


    #outfile <- tempfile(pattern = 'covs_', fileext = '.png')
    outfile <- paste0(getwd(), '/', rootDir, '/', currentUser, '/', input$currentProject, '/Covariates/AllCovsPlot.png')


    png(filename = outfile,   width = psize * nc, height = psize * nr, units = "cm", res=150)
    mat <- matrix(seq(1,(nc*nr)), nr, nc, byrow = TRUE)
    nf <- layout(mat, widths = rep.int(1, ncol(mat)), heights = rep.int(1, nrow(mat)))


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



  }, deleteFile = F)
  
  

  
  observe({
    req(input$currentProject )
  
    covDir <- paste0(rootDir, '/', currentUser, '/', input$currentProject, '/Covariates' )
    paths <- list.files( paste0(covDir), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.tif$'), full.names = T, recursive =F)
    if(length(paths) > 0){
      RV$ProjCovStack <- getCovariateStack(input$currentProject)
    }else{
      RV$ProjCovStack <- NULL
    }
  })
  
  
  
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




