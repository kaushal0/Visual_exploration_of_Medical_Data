#ui.R
# getwd()
# installed.packages()

##############    Install required packages if not installed already    ####################    
list.of.packages <- c("shiny", "shinydashboard","dygraphs", "dplyr","tidyverse", "stringr",
                      "xts", "data.table", "DT", "shinyjs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
print()

############################################################################################
library(shiny)
library(shinydashboard)
library(dygraphs)
library(dplyr)
library(tidyverse)
library(stringr)
library(xts)
library(data.table)
library(DT)
library(shinyjs)

setwd("/Users/kaushal/Desktop/Trinity/Dissertation/Data/Latest/Testing Dashboard/App7")
# encounter_inline= read.csv("searchResults-20180521_encounters_inline.csv", header = TRUE, check.names = F, as.is=TRUE)
dataset= read.csv("bindf.csv", header = TRUE, check.names = F, as.is=TRUE)

# includeScript("www/syncGraph.js")
# 
# var g1 = dygraph(graphXts3, main = "IV Therapy") %>% 
#   dyRangeSelector(strokeColor = "darkred", fillColor = "darkred") %>%
#   dyOptions(colors = c("red", "navy", "green","fuchsia")) %>%
#   dyLegend(show = "always",width = 250,showZeroValues = TRUE, hideOnMouseOut = FALSE, labelsDiv= "legendBox3",labelsSeparateLines = TRUE) 
# 
#    g2 = new Dygraph(graphXts3)
    

#####################################
#encounters - 3,4, 5:106
#biopsy - 3,4, 107:122
#continuous_medication - 3,4, 123:128
#iv_therapy - 3,4, 129:132
#plasma_exchange - 3,4, 133:134
#demographics - 3,4, 135:350
#complications - 3,4, 351:360
#####################################
#Add Renal Transplant columns from the treatment file
#####################################

#Select Biopsy columns and remove the columns with no values
dataset1= dataset
SelBiopsy= dataset1[(dataset1$PatientID==4020)&(dataset1$Label=="Biopsy"),]
SelBiopsy= SelBiopsy[,c(3,4,107:122)]
SelBiopsy= SelBiopsy[,which(unlist(lapply(SelBiopsy, function(x)!all(is.na(x)|(x=="")))))]

patient1= dataset[which(dataset$PatientID==1),]
names(dataset[360])

patientID= unique(dataset$PatientID)

#For DropDown options in Encounter & Biopsy page
SelEncounters= dataset1[(dataset1$PatientID==1)&(dataset1$Label=="Encounters"),]
SelEncounters= SelEncounters[,c(3,4,5:106)]
featureEncounter1= names(dataset1[,c(13,71,84:96,98:106)])
featureEncounter2= names(dataset1[,c(13,71,84:96,98:106)])
featureEncounter3= names(dataset1[,c(13,71,84:96,98:106)])


############################    For Treatment     ######################################
dataset1= dataset
# SelCoMed= dataset1[(dataset1$PatientID==1)&(dataset1$Label=="Continuous_Medication"),]

SelCoMedAll= dataset1[dataset1$Label=="Continuous_Medication",]
SelIVTAll= dataset1[dataset1$Label=="IV_Therapy",]
SelCompAll= dataset1[dataset1$Label=="Complications",]

##  Adding Current Sys Date where treatment is ongoing
# SelCoMedAll= SelCoMedAll[,c(3,4,123:128)]
# SelCoMedAll[ , sapply(SelCoMedAll$Stop.Date, function(x) all(x=="") ) ]<- NULL
class(SelCoMedAll$Stop.Date)
SelCoMedAll$Stop.Date= as.Date(SelCoMedAll$Stop.Date)
SelCoMedAll$Stop.Date[is.na(SelCoMedAll$Stop.Date)]<- Sys.Date()


######################################  TRIAL  ################################################
SelCoMed= SelCoMedAll[(SelCoMedAll$Label=="Continuous_Medication"),]
SelCoMedPid= SelCoMed[,c(2,3,4,123:128)]

SelIVT= SelIVTAll[(SelIVTAll$Label=="IV_Therapy"),]
SelIVTPid= SelIVT[,c(2,3,4,129:132)]

date1= SelCoMed["Date"]
date2= SelCoMed["Stop.Date"]
names(date2)[1]= "Date"
date1$Date= as.Date(date1$Date)
date1= rbind(date1, date2)
# class(date1$Date)
# class(date2$Date)
# date1$Date= as.Date(date1$Date)
# date1= date1[-which(is.na(date1$Date)), ]
# date1= data.frame(date1)
out <- date1[rev(order(as.Date(date1$Date))), ]
maxDate=out[1]
minDate=out[length(out)]

allDates= seq(minDate, maxDate, by="days")
allDates= data.frame(allDates)

x= unique(SelCoMed["Drug"])
xrows= nrow(x)
SelCoMedFinal= data.frame(matrix(ncol = xrows+2, nrow = 0))
x[xrows+1,]= "Date"
x[xrows+2,]= "Frequency"
colnames(SelCoMedFinal)= x$Drug

class(SelCoMedFinal$Date)
SelCoMedFinal$Date= as.Date(SelCoMedFinal$Date)

colnames(SelCoMedFinal[ncol(SelCoMedFinal)-1])
colnames(SelCoMedFinal[510])
SelCoMedFinal= SelCoMedFinal[,c(ncol(SelCoMedFinal)-1,ncol(SelCoMedFinal),1:(ncol(SelCoMedFinal)-2))]

# k=grep("Date", colnames(SelCoMedFinal))
# for(j in 1:nrow(allDates)){
#   SelCoMedFinal[j,k]= allDates[j,1]
# }


# q=1
# for(q in 1:2){
#   SelCoMedTemp= SelCoMedPid[(SelCoMedPid$PatientID==2),]
#   # CoMedPid= SelCoMedPid$PatientID
#   SelCoMed= SelCoMedTemp[,c(2:ncol(SelCoMedTemp))]
#   flg1=0
#   flg2=0
#   for(k in 1:nrow(SelCoMed)){
#     dosage= SelCoMed[k,4]
#     colName= SelCoMed[k,3]
#     print(k)
#     # for(j in 1:nrow(SelCoMedFinal)){
#     for(n in 1:(ncol(SelCoMedFinal)-2)){
#       if(colName==colnames(SelCoMedFinal[n])){
#         DT= seq(as.Date(SelCoMed[k,2]), as.Date(SelCoMed[k,8]), by="days")
#         # length(DT)
#         for(j in 1:nrow(SelCoMedFinal)){
#           for(m in 1:length(DT)){
#             if(DT[m]==SelCoMedFinal$Date[j]){
#               flg1=1
#               flg2=1
#               len= j+length(DT)-1
#               for(q in j:len){
#                 SelCoMedFinal[q,colName]= dosage
#                 print(q)
#               }
#               # SelCoMedFinal[j,colName]= dosage
#             }
#             if(flg1==1){
#               flg1=0
#               break
#             }
#           }
#           if(flg2==1){
#             flg2=0
#             break
#           }
#         }
#       }
#     }
#   }
# 
# }

# SelCoMedFinal["Date"]

###################################   CONTINUOUS MEDICATION   #################################
## Finding Min and Max Dates
# SelCoMed= SelCoMedAll[(SelCoMedAll$PatientID==1)&(SelCoMedAll$Label=="Continuous_Medication"),]
# SelCoMed= SelCoMed[,c(3,4,123:128)]
# 
# # date1= SelCoMed[2]
# # date2= SelCoMed[8]
# date1= SelCoMed["Date"]
# date2= SelCoMed["Stop.Date"]
# names(date2)[1]= "Date"
# date1$Date= as.Date(date1$Date)
# date1= rbind(date1, date2)
# # class(date1$Date)
# # class(date2$Date)
# # date1$Date= as.Date(date1$Date)
# # date1= date1[-which(is.na(date1$Date)), ]
# # date1= data.frame(date1)
# out <- date1[rev(order(as.Date(date1$Date))), ]
# maxDate=out[1]
# minDate=out[length(out)]
# 
# allDates= seq(minDate, maxDate, by="days")
# allDates= data.frame(allDates)
# 
# ## Creating empty dataframe
# # x= unique(SelCoMed[3])
# x= unique(SelCoMed["Drug"])
# xrows= nrow(x)
# SelCoMedFinal= data.frame(matrix(ncol = xrows+2, nrow = 0))
# x[xrows+1,]= "Date"
# x[xrows+2,]= "Frequency"
# colnames(SelCoMedFinal)= x$Drug
# 
# class(SelCoMedFinal$Date)
# SelCoMedFinal$Date= as.Date(SelCoMedFinal$Date)
# # k=6
# k=grep("Date", colnames(SelCoMedFinal))
# for(j in 1:nrow(allDates)){
#     SelCoMedFinal[j,k]= allDates[j,1]
# }
# 
# # q=113
# # k=5
# # n=1
# # j=88
# # m=1
# flg1=0
# flg2=0
# for(k in 1:nrow(SelCoMed)){
#     dosage= SelCoMed[k,4]
#     colName= SelCoMed[k,3]
#     print(k)
#     # for(j in 1:nrow(SelCoMedFinal)){
#       for(n in 1:(ncol(SelCoMedFinal)-2)){
#         if(colName==colnames(SelCoMedFinal[n])){
#           DT= seq(as.Date(SelCoMed[k,2]), as.Date(SelCoMed[k,8]), by="days")
#           # length(DT)
#           for(j in 1:nrow(SelCoMedFinal)){
#             for(m in 1:length(DT)){
#               if(DT[m]==SelCoMedFinal$Date[j]){
#                 flg1=1
#                 flg2=1
#                 len= j+length(DT)-1
#                 for(q in j:len){
#                   SelCoMedFinal[q,colName]= dosage
#                   print(q)
#                 }
#                 # SelCoMedFinal[j,colName]= dosage
#               }
#               if(flg1==1){
#                 flg1=0
#                 break
#               }
#             }
#             if(flg2==1){
#               flg2=0
#               break
#             }
#         }
#       }
#     }
# }

# SelCoMedFinal1= SelCoMedFinal
# SelCoMedFinal2= SelCoMedFinal
# SelCoMedFinal2[is.na(SelCoMedFinal2)]<-0
# class(SelCoMedFinal2$Date)
# 
# SelCoMedFinal2Xts= xts(SelCoMedFinal2[, -1, drop= FALSE], order.by= SelCoMedFinal2[,6])
# graphXts2= SelCoMedFinal2Xts[,c(1,2,3,4)]
# 
# dygraph(graphXts2, main = "Treatment Details") %>% 
#   dyRangeSelector(strokeColor = "darkred", fillColor = "darkred") %>%
#   dyOptions(colors = c("red", "navy", "green","fuchsia")) %>%
#   dyLegend(show = "always",width = 250,showZeroValues = TRUE, hideOnMouseOut = FALSE, labelsDiv= "legendBox",labelsSeparateLines = TRUE) 
# 
# 
# library(compare)
# install.packages("compare")
# comparison <- compare(SelCoMedFinal,SelCoMedFinal1,allowAll=TRUE)


########################################################################################

############################  Complications ############################################

# SelComp= SelCompAll[(SelCompAll$PatientID==1)&(SelCompAll$Label=="Complications"),]
# SelComp= SelComp[,c(2,3,4,351:360)]






########################################################################################



# library(xts)
# lines<- lines[-which(lines$"Date Of Visit"==""), ]
# class(lines$`Date Of Visit`)
# lines$`Date Of Visit`= as.Date(lines$`Date Of Visit`)
# # oLines= lines[order(as.Date(lines$"Date Of Visit", "%d-%m-%Y")),]
# linesxts= xts(lines[, -1, drop=FALSE],  order.by=lines[,1])
# # linesxts1= linesxts[9:23,c(13, 29)]
# # patientxts1= linesxts1
# # dygraph(patientxts1, main = "Weight in KG") %>% dyRangeSelector()
# 
# 
# # linesxts= xts(oLines, order.by=oLines[,1])
# patientxts1= linesxts[,c(10,81)]
# selectedLines = linesxts[,c(10,81)]
# dygraph(selectedLines, main = "Weight (KG)") %>% dyRangeSelector()


####################     UI         ##################################

# shinyApp(
ui=
  # fluidPage(
  dashboardPage(
    dashboardHeader(title= "Graphical Analysis"),
    dashboardSidebar(
      sidebarMenu(id= "sideMenu",
                  # sliderInput(inputId="num",
                  #             label="Choose a number",
                  #             value= 25, min=1, max= 100),
                  
                  selectInput("patientID", "Choose the Patient ID:",
                              patientID,
                              selected= patientID[1]
                  ),
                  menuItem("Dashboard", tabName = "dashboard",
                           menuSubItem("Patient Summary", tabName = "info"),
                           menuSubItem("Medical Summary", tabName = "summary")
                  ),
                  # menuItem("Test", tabName = "widgets"),
                  # menuItem("Selection Menu", tabName = "dashboard"),
                  
                  menuItem("Selection Menu", tabName = "selMenu",
                           menuItem("Encounters & Biopsy", tabName = "encounter"),
                           menuItem("Treatment", #tabName = "treatment",
                                    menuSubItem("Continuous Medication",tabName = "Cmed"),
                                    menuSubItem("IV Therapy", tabName= "IVT"),
                                    menuSubItem("Plasma Exchange", tabName= "plex")
                           ),
                           menuItem("Complications", tabName = "complication")
                  ),
                  
                  conditionalPanel("input.sideMenu == 'summary'",
                                   # sliderInput("b", "Under Treatment Menu", 1, 100, 50),
                                   selectInput("feature4", "Choose the Encounter Feature to be displayed in Green",
                                               featureEncounter4
                                   ),
                                   selectInput("feature5", "Choose the Encounter Feature to be displayed in Purple",
                                               featureEncounter5
                                   ),
                                   selectInput("feature6", "Choose the Encounter Feature to be displayed in Red",
                                               featureEncounter6
                                   )
                  ),
                  
                  conditionalPanel("input.sideMenu == 'encounter'",
                                   # sliderInput("b", "Under Treatment Menu", 1, 100, 50),
                                   selectInput("feature1", "Choose the Feature to be displayed in Green",
                                               featureEncounter1
                                   ),
                                   selectInput("feature2", "Choose the Feature to be displayed in Purple",
                                               featureEncounter2
                                   ),
                                   selectInput("feature3", "Choose the Feature to be displayed in Red",
                                               featureEncounter3
                                   )
                  )
                  
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "widgets",
                sliderInput('sampleSize', 'Sample Size (Laps)', min=1, max=100,
                            value=1, step=1, round=0),
                h2("Widgets tab content")
        ),
        tabItem(tabName = "info",
                h1(
                  htmlOutput("headerSummary"), align= "center"
                ),
                fluidRow(
                  # valueBox( textOutput(patientID), "Patient ID",icon = icon("glyphicon-user")),
                  # textOutput("Encounter")
                  box( width = 5, background = 'blue',
                       htmlOutput("patientSum")
                  ),
                  infoBoxOutput("paID", width = 2),
                  box( width = 5, background = 'blue',
                       htmlOutput("diagnosisSum")
                  )
                  # ?valueBox
                  ######    show patient ID on the Summary Dashboard   ######
                  # tableOutput('table1'),
                  # dygraphOutput(outputId="dygraph")
                  # textOutput("feature1")
                ),
                fluidRow(
                  box( width = 4, background = 'blue',
                       # htmlOutput("sysInvolved")
                       tableOutput("sysInvolved")
                  ),
                  box( width = 4, background = 'blue',
                       htmlOutput("indTreatment")
                  ),
                  box( width = 4, background = 'blue',
                       htmlOutput("mtnTreatment")
                  )
                ),
                
                fluidRow(
                  box( width = 4, background = 'blue',
                       # htmlOutput("sysInvolved")
                       tableOutput("cmdDemo")
                  )
                )
        ),
        tabItem(tabName = "summary",
                # h1(
                #   htmlOutput("headerMedSummary"), align= "center"
                # ),
                fluidRow(
                  box(
                    width=10,
                    dygraphOutput(outputId="SummaryGraph1")
                  ),
                  box(id = "Box1",
                      width=2
                  )
                ),
                fluidRow(
                  box(
                    width=6,
                    dygraphOutput(outputId="SummaryGraph3")
                  ),
                  box(
                    width=6,
                    dygraphOutput(outputId="SummaryGraph2")
                  )
                )
        ),
        tabItem(tabName = "encounter",
                # h1("patientID"),
                h1(
                  htmlOutput("headerEncounter"), align= "center"
                ),
                fluidPage(
                  box( width = 12,
                       tabsetPanel(
                         tabPanel("Summary of Biopsy",
                                  dataTableOutput("biopSummary")
                                  # tableOutput("biopSummary1")
                         )
                       )
                  ),
                  fluidRow(
                    box(
                      width=10,
                      dygraphOutput(outputId="dygraph1")
                    ),
                    box(id = "legendBox1",
                        width=2
                    )
                  )
                )
        ),
        tabItem(tabName = "Cmed",
                h1(
                  htmlOutput("headerCmed"), align= "center"
                ),
                fluidRow(
                  box(
                    width=10,
                    dygraphOutput(outputId="dygraph2")
                  ),
                  box(id = "legendBox2",
                      width=2
                  )
                ),
                fluidRow(
                  box( width = 12,
                       tabsetPanel(
                         tabPanel("Continuous Medication Details",
                                  dataTableOutput("CmedSummary")
                                  # tableOutput("biopSummary1")
                         )
                       )
                  )
                )
                # textOutput("featureEncounter1"),
                # textOutput("Treatment")
                # textOutput("feature1")
        ),
        tabItem(tabName = "IVT",
                h1(
                  htmlOutput("headerIVT"), align= "center"
                ),
                fluidRow(
                  box(
                    width=10,
                    dygraphOutput(outputId="dygraph3")
                  ),
                  box(id = "legendBox3",
                      width=2
                  )
                ),
                fluidRow(
                  box( width = 12,
                       tabsetPanel(
                         tabPanel("IV Therapy Details",
                                  dataTableOutput("ivtSummary")
                                  # tableOutput("biopSummary1")
                         )
                       )
                  )
                )
        ),
        tabItem(tabName = "plex",
                h1(
                  htmlOutput("headerplex"), align= "center"
                ),
                fluidRow(
                  box(
                    width=10,
                    dygraphOutput(outputId="dygraph4")
                  ),
                  box(id = "legendBox4",
                      width=2
                  )
                ),
                fluidRow(
                  box( width = 12,
                       tabsetPanel(
                         tabPanel("Plasma Exchange Details",
                                  dataTableOutput("plexSummary")
                                  # tableOutput("biopSummary1")
                         )
                       )
                  )
                )
        ),
        tabItem(tabName = "complication",
                h1(
                  htmlOutput("headerComplications"), align= "center"
                ),
                box( width = 12,
                     tabsetPanel(
                       tabPanel("Summary of Complications",
                                dataTableOutput("compSummary")
                       )
                     )
                ),
                fluidRow(
                  box( width =6,
                       plotOutput("plotComp1")
                  ),
                  box( width =6,
                       plotOutput("plotComp3")
                  )
                ),
                fluidRow(
                  box( width =12,
                       plotOutput("plotComp2")
                  )
                )
                
        )
      )
    )
  )
# ),


# )


