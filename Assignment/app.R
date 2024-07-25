#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(ggpubr)

# Load data
MEA1D20 <- read.csv("P:/28-Project_Organization/SECPDp/MEA/MEA1/MEA1_D20(000).csv",
                    header = T, skip = 156, sep=",",   nrows = 26)

n <- MEA1D20$Measurement 
MEA1D20t <- as.data.frame(t(MEA1D20[,-1]))   
colnames(MEA1D20t) <- n  
m <- rownames(MEA1D20t) 
MEA1D20t %>% mutate(wells = rownames(MEA1D20t)) %>% mutate(timePoint = "day20") %>% mutate(day = 20) %>% mutate(experiment = "MEA1") -> MEA1D20t
m -> rownames(MEA1D20t)

MEA1D25 <- read.csv("P:/28-Project_Organization/SECPDp/MEA/MEA1/MEA1_D25(000).csv",
                    header = T, skip = 156, sep=",",   nrows = 26)

n <- MEA1D25$Measurement 
MEA1D25t <- as.data.frame(t(MEA1D25[,-1]))   
colnames(MEA1D25t) <- n  
m <- rownames(MEA1D25t) 
MEA1D25t %>% mutate(wells = rownames(MEA1D25t)) %>% mutate(timePoint = "day25") %>% mutate(day = 25) %>% mutate(experiment = "MEA1") -> MEA1D25t
m -> rownames(MEA1D25t)
#

MEA1D30 <- read.csv("P:/28-Project_Organization/SECPDp/MEA/MEA1/MEA1_D30(000).csv",
                    header = T, skip = 156, sep=",",   nrows = 26)

n <- MEA1D30$Measurement 
MEA1D30t <- as.data.frame(t(MEA1D30[,-1]))   
colnames(MEA1D30t) <- n  
m <- rownames(MEA1D30t) 
MEA1D30t %>% mutate(wells = rownames(MEA1D30t)) %>% mutate(timePoint = "day30") %>% mutate(day = 30) %>% mutate(experiment = "MEA1") -> MEA1D30t
m -> rownames(MEA1D30t)

MEA1D35 <- read.csv("P:/28-Project_Organization/SECPDp/MEA/MEA1/MEA1_D35(000).csv",
                    header = T, skip = 156, sep=",",   nrows = 26)

n <- MEA1D35$Measurement 
MEA1D35t <- as.data.frame(t(MEA1D35[,-1]))   
colnames(MEA1D35t) <- n  
m <- rownames(MEA1D35t) 
MEA1D35t %>% mutate(wells = rownames(MEA1D35t)) %>% mutate(timePoint = "day35") %>% mutate(day = 35) %>% mutate(experiment = "MEA1") -> MEA1D35t
m -> rownames(MEA1D35t)
#

MEA1D40 <- read.csv("P:/28-Project_Organization/SECPDp/MEA/MEA1/MEA1_D40(000).csv",
                    header = T, skip = 156, sep=",",   nrows = 26)

n <- MEA1D40$Measurement 
MEA1D40t <- as.data.frame(t(MEA1D40[,-1]))   
colnames(MEA1D40t) <- n  
m <- rownames(MEA1D40t) 
MEA1D40t %>% mutate(wells = rownames(MEA1D40t)) %>% mutate(timePoint = "day40") %>% mutate(day = 40) %>% mutate(experiment = "MEA1") -> MEA1D40t
m -> rownames(MEA1D40t)
#

## merge all files form different time points 

dataWell <- rbind(MEA1D20t, MEA1D25t, MEA1D30t, MEA1D35t, MEA1D40t)
head(dataWell)

#dataWell <- MEA1D20t, MEA1D25t, MEA1D30t, MEA1D35t, MEA1D40t

colnames(dataWell)<-c( 'NumberOfSpikes', 'MeanFiringRate', 'ISI', 'NumberOfBurst','BurstDuration_avrg', 'BurstDuration_std', 'NumberSpikesPerBurst_avg', 'NumberSpikesPerBurst_std', 'meanISI_burst_avg','meanISI_burst_std','medianISI_burst_avg','medianISI_burst_std','InterBurstInterval_avg', 'InterBurstInterval_std', 'BurstFrequency', 'IBI_CoefficientOfVariation', 'Normalized_Duration_IQR','BurstPercentage', 'Condition','wells', 'timepoint', 'day','experiment')

# create colum for removal of outliers (per condition & timepoint)
dataWell %>% mutate(out = paste(dataWell$Condition, dataWell$timepoint)) ->  dataWell

## eliminate all empty wells to do not generate a extra "out" variable that does not exists. for that eliminates all where condition = NA
to_rm = which(is.na(dataWell$Condition))
if (length(to_rm) ==0){
  dataWell = dataWell
}else{
  dataWell <- dataWell[-to_rm,]
}

# convert into numeric all data of interest fro plotting (needed because data was as character) 
dataWell$NumberOfSpikes <- as.numeric(as.character(dataWell$NumberOfSpikes))
dataWell$MeanFiringRate <- as.numeric(as.character(dataWell$MeanFiringRate))
dataWell$ISI <- as.numeric(as.character(dataWell$ISI))
dataWell$NumberOfBurst <- as.numeric(as.character(dataWell$NumberOfBurst))
dataWell$BurstDuration_avrg <- as.numeric(as.character(dataWell$BurstDuration_avrg))
dataWell$BurstDuration_std <- as.numeric(as.character(dataWell$BurstDuration_std))
dataWell$NumberSpikesPerBurst_avg <- as.numeric(as.character(dataWell$NumberSpikesPerBurst_avg))
dataWell$NumberSpikesPerBurst_std <- as.numeric(as.character(dataWell$NumberSpikesPerBurst_std))
dataWell$meanISI_burst_avg <- as.numeric(as.character(dataWell$meanISI_burst_avg))
dataWell$meanISI_burst_std <- as.numeric(as.character(dataWell$meanISI_burst_std))
dataWell$medianISI_burst_std <- as.numeric(as.character(dataWell$medianISI_burst_std))
dataWell$medianISI_burst_avg <- as.numeric(as.character(dataWell$medianISI_burst_avg))
dataWell$InterBurstInterval_avg <- as.numeric(as.character(dataWell$InterBurstInterval_avg))
dataWell$InterBurstInterval_std <- as.numeric(as.character(dataWell$InterBurstInterval_std))
dataWell$BurstFrequency <- as.numeric(as.character(dataWell$BurstFrequency))
dataWell$IBI_CoefficientOfVariation <- as.numeric(as.character(dataWell$IBI_CoefficientOfVariation))
dataWell$Normalized_Duration_IQR <- as.numeric(as.character(dataWell$Normalized_Duration_IQR))
dataWell$BurstPercentage <- as.numeric(as.character(dataWell$BurstPercentage))
dataWell$day <- as.numeric(as.character(dataWell$day))


# obtain data only with bursting information for extracting into CSV and plotting in graphad (needed to take into consideration SD already calculated by software)
#dataWell_burst <- dataWell[dataWell$NumberOfBurst != 0, ] #remove wells with no information for bursting 

dataWell_5spike <- dataWell[dataWell$NumberOfSpikes >= 5, ]

# NOTE!! if zeros are kept, may infomation will be extracted based on a simple value due to the ceros being the mean !! 
# transfrom 0 into NAs 

dataWell_5spike[is.na(dataWell_5spike)] <- 0
dataWell_noZero<-dataWell_5spike

#dataWell_noZero <- replace(dataWell_5spike, dataWell_5spike == 0, NA)

groupOut <- unique(dataWell_noZero$out)



index <- data.frame()
index2 <- data.frame()
index3 <- data.frame()
index4 <- data.frame()

for (i in 1:length(groupOut))
{
  datatest <- dataWell_noZero %>% 
    filter(out %in% groupOut[i]) # go for each line and extract rows for that line 
  #exprs <- datatest[2:24] # extract the variables within that line 
  feature_names <- colnames(datatest[1:18])  # create vector with names of variables 
  for (j in 1:18) # for (j in 2:length(exprs))
  {
    out <- boxplot.stats(datatest[,j])$out
    index2 <- data.frame()
    for (k in 1:length(out))
    {
      index <- data.frame()
      index <- datatest[datatest[,j]==out[k],]# sum up all index of all data so it can be removed all lies afterwards 
      index2 <- rbind(index2,index)
      rm(index)
    }
    index2 %>% mutate(cond_outlier = feature_names[j]) %>% mutate(group_tested = groupOut[i]) -> index3
    index4 <- rbind(index4,index3)
    rm(out, index2, index3)
  }
} 


# remove data from extra "NA"

summary_out <- unique(index4[1:24])
summary_in <- setdiff(dataWell_noZero, summary_out)

# Remove an entire row if it has >22 NAs (for example >50% of your features)
count_na <- function(x)sum(is.na(x))
data_noOut_well <- summary_in %>%
  dplyr::mutate(count_na = apply(., 1, count_na))
data_noOut_well <-summary_in[!(data_noOut_well$count_na >= 19),] # remove wells containing no info 


# remove the data containinf outlayers from main dataset * a outlayer in one single feature will cause the removal of full well data 

# NOTE!! if zeros are kept, may infomation will be extracted based on a simple value due to the zeros being the mean !! 
# transfrom 0 into NAs 


#dataWell_noZero <- replace(dataWell, dataWell == 0, NA)
datatest_NA <- data.frame()


for (i in 1:length(groupOut))
{
  datatest <- dataWell_noZero %>% 
    filter(out %in% groupOut[i]) # go for each line and extract rows for that line 
  for (j in 1:18) # for (j in 2:length(exprs))
  {
    out <- boxplot.stats(datatest[,j])$out
    for (k in 1:length(out))
    {
      datatest[,j][datatest[,j] == out[k]] <- NA
    }
    rm(out)
  }
  datatest_NA <- rbind(datatest_NA,datatest)
  rm(datatest)
} 


#saving info: 
# files without outliers were generated from file No zeros - reason: too many zeros cause exclusion of real values as outliers 
# _well > meaningn all well information was removed for all variable when at least one of the variables presented one outlier -> the well is not consider any longer 

# Remove an entire row if it has >22 NAs (for example >50% of your features)
count_na <- function(x)sum(is.na(x))
data_noOut_value <- datatest_NA %>%
  dplyr::mutate(count_na = apply(., 1, count_na))
data_noOut_value <-datatest_NA[!(data_noOut_value$count_na >=19),] # remove wells containing no info 


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  
  # App title ----
  titlePanel("DP-SMB - NextImmune2 - Data Science Meetings - Task 3"),
  
  # Use sandstone theme from shinythemes
  theme = shinytheme("sandstone"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("MEA1D20", "MEA1D25", "MEA1D30", "MEA1D35", "MEA1D40")),
      
      # Input: Specify the number of observations to view ----
      numericInput("obs", "Number of observations to view:", 10),
      
      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("update", "Update View")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Header + summary of distribution ----
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      # Output: Header + table of distribution ----
      h4("Observations"),
      tableOutput("view")
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "MEA1D20" = MEA1D20t,
           "MEA1D25" = MEA1D25t,
           "MEA1D30" = MEA1D30t,
           "MEA1D35" = MEA1D35t,
           "MEA1D40" = MEA1D40t)
  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)



