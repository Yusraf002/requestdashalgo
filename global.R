# ---- Library ----
library(shiny)
library(shinydashboard)
library(shinysurveys)
library(shinyWidgets)
library(shinyjs)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(lubridate)
library(glue)
library(DT)
library(ggplot2)
library(plotly)
library(stringr)

# ---- Setup gdrive & gsheet ----
options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = ".secrets"
  #googledrive::drive_auth(email = "yusraf@algorit.ma", cache = ".secrets")
)


## Get the ID of the sheet for writing programmatically
## This should be placed at the top of your shiny app
sheet_id <- drive_get("dashboard")[1,]$id

## ---- membaca data dari gsheet ----
url <- "https://docs.google.com/spreadsheets/d/1rc6x9cOuE4cvWMg2-Poox4ta86WnBRYR3lXFEDCkrGY/edit?usp=sharing"
udata <- read_sheet(url)
sla_tabel <- read_sheet(url,sheet = "SLA")

# Date Question ----
extendInputType("date", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-60,
    max = Sys.Date()+0
  )
})

extendInputType("date2", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-60,
    max = Sys.Date()+21
  )
})

extendInputType("date3", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-60,
    max = Sys.Date()+21
  )
})

extendInputType("date4", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-60,
    max = Sys.Date()+21
  )
})

extendInputType("date5", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-60,
    max = Sys.Date()+21
  )
})

## Req date----
req_date <- data.frame(question = "Request Date",
                       option = NA,
                       input_type = "date",
                       input_id = "req_date",
                       dependence = NA,
                       dependence_value = NA,
                       required = F)

## Due date ----
due_dates <- data.frame(question = "for when this request was made",
                        option = NA,
                        input_type = "date2",
                        input_id = "due_date",
                        dependence = NA,
                        dependence_value = NA,
                        required = FALSE)
## Done date ----
done_date <- data.frame(question = "Done Date",
                        option = NA,
                        input_type = "date3",
                        input_id = "done_date",
                        dependence = "sts",
                        dependence_value = "Done",
                        required = F)
## Canceled date ----
cancel_date <- data.frame(question = "Canceled Date",
                          option = NA,
                          input_type = "date4",
                          input_id = "cancel_date",
                          dependence = "sts",
                          dependence_value = "Cancel",
                          required = F)
## Blocked date ----
block_date <- data.frame(question = "Blocked Date",
                         option = NA,
                         input_type = "date5",
                         input_id = "block_date",
                         dependence = "sts",
                         dependence_value = "Blocked",
                         required = F)

# Main Question ----
## Tim/Divisi ----
P1 <- data.frame(
  question  = c("What is your team","What is your team","What is your team","What is your team","What is your team"),
  option    = c("Business Development","Marketing","Product","Human Resources","Finance"),
  input_type= c("mc","mc","mc","mc","mc"),
  input_id  = c("tim","tim","tim","tim","tim"), 
  dependence = c(NA,NA,NA,NA,NA),
  dependence_value = c(NA,NA,NA,NA,NA),
  required  = c(T,T,T,T,T)
)

## Nama ----
P2 <- data.frame(
  question  = c("What is your name"),
  option    = c("type your name in lowercase"),
  input_type= c("text"),
  input_id  = c("nama"), 
  dependence = c(NA),
  dependence_value = c(NA),
  required  = c(T)
)

## Merch Request ----

P3 <- data.frame(
  question  = 
    c(
      # 3.0 merc req 
      "Merchandise request","Merchandise request",
      
      # 3.1 merch detail (1)
      "Merchandise detail (1)","Merchandise detail (1)","Merchandise detail (1)",
      "Merchandise detail (1)","Merchandise detail (1)","Merchandise detail (1)",
      "Merchandise detail (1)","Merchandise detail (1)","Merchandise detail (1)",
      "Merchandise detail (1)",
      
      # 3.2 qty merch (1)
      "Quantity Merchandise (1)",
      
      # 3.3 more merch (1)
      "More merch request","More merch request",
      
      # 3.1 merch detail (2)
      "Merchandise detail (2)","Merchandise detail (2)","Merchandise detail (2)",
      "Merchandise detail (2)","Merchandise detail (2)","Merchandise detail (2)",
      "Merchandise detail (2)","Merchandise detail (2)","Merchandise detail (2)",
      "Merchandise detail (2)",
      
      # 3.2 qty merch (2)
      "Quantity Merchandise (2)",
      
      # 3.3 more merch (2)
      "More merch request","More merch request",
      
      # 3.1 merch detail (3)
      "Merchandise detail (3)","Merchandise detail (3)","Merchandise detail (3)",
      "Merchandise detail (3)","Merchandise detail (3)","Merchandise detail (3)",
      "Merchandise detail (3)","Merchandise detail (3)","Merchandise detail (3)",
      "Merchandise detail (3)",
      
      # 3.2 qty merch (3)
      "Quantity Merchandise (3)",
      
      # 3.3 more merch (3)
      "More merch request","More merch request",
      
      # 3.1 merch detail (4)
      "Merchandise detail (4)","Merchandise detail (4)","Merchandise detail (4)",
      "Merchandise detail (4)","Merchandise detail (4)","Merchandise detail (4)",
      "Merchandise detail (4)","Merchandise detail (4)","Merchandise detail (4)",
      "Merchandise detail (4)",
      
      # 3.2 qty merch (4)
      "Quantity Merchandise (4)",
      
      # 3.3 more merch (4)
      "More merch request","More merch request",
      
      # 3.1 merch detail (5)
      "Merchandise detail (5)","Merchandise detail (5)","Merchandise detail (5)",
      "Merchandise detail (5)","Merchandise detail (5)","Merchandise detail (5)",
      "Merchandise detail (5)","Merchandise detail (5)","Merchandise detail (5)",
      "Merchandise detail (5)",
      
      # 3.2 qty merch (5)
      "Quantity Merchandise (5)"
    ),
  option    = 
    c(
      # Ans3.0 merch 
      "Yes","No",
      
      # Ans3.1 [marchandise detail 1]
      "Notebook", "Pullpen", "Lanyard", "Sticker", "Tumbler", 
      "Mug", "Totebag", "Pouch", "Neck Pillow", "T-Shirt",
      
      # Ans3.2 [merch qty 1]
      "Quantity merchandise request 1",
      
      # Ans3.3 
      "Yes","No",
      
      # Ans3.1 [marchandise detail 2]
      "Notebook", "Pullpen", "Lanyard", "Sticker", "Tumbler", 
      "Mug", "Totebag", "Pouch", "Neck Pillow", "T-Shirt",
      
      # Ans3.2 [merch qty 2]
      "Quantity merchandise request 2",
      
      # Ans3.3 
      "Yes","No",
      
      # Ans3.1 [marchandise detail 3]
      "Notebook", "Pullpen", "Lanyard", "Sticker", "Tumbler", 
      "Mug", "Totebag", "Pouch", "Neck Pillow", "T-Shirt",
      
      # Ans3.2 [merch qty 3]
      "Quantity merchandise request 3",
      
      # Ans3.3 
      "Yes","No",
      
      # Ans3.1 [marchandise detail 4]
      "Notebook", "Pullpen", "Lanyard", "Sticker", "Tumbler", 
      "Mug", "Totebag", "Pouch", "Neck Pillow", "T-Shirt",
      
      # Ans3.2 [merch qty 4]
      "Quantity merchandise request 4",
      
      # Ans3.3 
      "Yes","No",
      
      # Ans3.1 [marchandise detail 5]
      "Notebook", "Pullpen", "Lanyard", "Sticker", "Tumbler", 
      "Mug", "Totebag", "Pouch", "Neck Pillow", "T-Shirt",
      
      # Ans3.2 [merch qty 5]
      "Quantity merchandise request 5"
    ),
  input_type= 
    c(
      # type3.0 merch
      "select","select",
      
      # type3.1 (1)
      "select","select","select","select","select",
      "select","select","select","select","select",
      
      # type3.2 (1)
      "numeric",
      
      # type3.3 (1)
      "mc","mc",
      
      # type3.1 (2)
      "select","select","select","select","select",
      "select","select","select","select","select",
      
      # type3.2 (2)
      "numeric",
      
      # type3.3 (2)
      "mc","mc",
      
      # type3.1 (3)
      "select","select","select","select","select",
      "select","select","select","select","select",
      
      # type3.2 (3)
      "numeric",
      
      # type3.3 (3)
      "mc","mc",
      
      # type3.1 (4)
      "select","select","select","select","select",
      "select","select","select","select","select",
      
      # type3.2 (4)
      "numeric",
      
      # type3.3 (4)
      "mc","mc",
      
      # type3.1 (5)
      "select","select","select","select","select",
      "select","select","select","select","select",
      
      # type3.2 (5)
      "numeric"
    ),
  input_id  = 
    c(
      # id3.0
      "mr","mr",
      
      # id3.1 (1)
      "mr1","mr1","mr1","mr1","mr1",
      "mr1","mr1","mr1","mr1","mr1",
      
      # id3.2 (1)
      "qmr1",
      
      # id3.3 (1)
      "more1","more1",
      
      # id3.1 (2)
      "mr2","mr2","mr2","mr2","mr2",
      "mr2","mr2","mr2","mr2","mr2",
      
      # id3.2 (2)
      "qmr2",
      
      # id3.3 (2)
      "more2","more2",
      
      # id3.1 (3)
      "mr3","mr3","mr3","mr3","mr3",
      "mr3","mr3","mr3","mr3","mr3",
      
      # id3.2 (3)
      "qmr3",
      
      # id3.3 (3)
      "more3","more3",
      
      # id3.1 (4)
      "mr4","mr4","mr4","mr4","mr4",
      "mr4","mr4","mr4","mr4","mr4",
      
      # id3.2 (4)
      "qmr4",
      
      # id3.3 (4)
      "more4","more4",
      
      # id3.1 (5)
      "mr5","mr5","mr5","mr5","mr5",
      "mr5","mr5","mr5","mr5","mr5",
      
      # id3.2 (5)
      "qmr5"
    ), 
  dependence = 
    c(
      # dep3.0
      NA,NA,
      
      # dep3.1 (1)
      "mr","mr","mr","mr","mr",
      "mr","mr","mr","mr","mr",
      
      # dep3.2 (1)
      "mr",
      
      # dep3.3 (1)
      "mr","mr",
      
      # dep3.1 (2)
      "more1","more1","more1","more1","more1",
      "more1","more1","more1","more1","more1",
      
      # dep3.2 (2)
      "more1",
      
      # dep3.3 (2)
      "more1","more1",
      
      # dep3.1 (3)
      "more2","more2","more2","more2","more2",
      "more2","more2","more2","more2","more2",
      
      # dep3.2 (3)
      "more2",
      
      # dep3.3 (3)
      "more2","more2",
      
      # dep3.1 (4)
      "more3","more3","more3","more3","more3",
      "more3","more3","more3","more3","more3",
      
      # dep3.2 (4)
      "more3",
      
      # dep3.3 (4)
      "more3","more3",
      
      # dep3.1 (5)
      "more4","more4","more4","more4","more4",
      "more4","more4","more4","more4","more4",
      
      # dep3.2 (5)
      "more4"
      
    ),
  dependence_value = 
    c(
      # val3.0
      NA,NA,
      
      # val3.1 [merch detail 1 ~ mr]
      "Yes","Yes","Yes","Yes","Yes",
      "Yes","Yes","Yes","Yes","Yes",
      
      # val3.2
      "Yes",
      
      # val3.3
      "Yes","Yes",
      
      # val3.1 [merch detail 2 ~ mr]
      "Yes","Yes","Yes","Yes","Yes",
      "Yes","Yes","Yes","Yes","Yes",
      
      # val3.2
      "Yes",
      
      # val3.3
      "Yes","Yes",
      
      # val3.1 [merch detail 3 ~ mr]
      "Yes","Yes","Yes","Yes","Yes",
      "Yes","Yes","Yes","Yes","Yes",
      
      # val3.2
      "Yes",
      
      # val3.3
      "Yes","Yes",
      
      # val3.1[merch detail 4 ~ mr]
      "Yes","Yes","Yes","Yes","Yes",
      "Yes","Yes","Yes","Yes","Yes",
      
      # val3.2
      "Yes",
      
      # val3.3
      "Yes","Yes",
      
      # val3.1 [merch detail 5 ~ mr]
      "Yes","Yes","Yes","Yes","Yes",
      "Yes","Yes","Yes","Yes","Yes",
      
      # val3.2
      "Yes"
    ),
  required  = 
    c(
      # req3.0
      F,F,
      
      # req3.1 (1)
      T,T,T,T,T,
      T,T,T,T,T,
      
      # req3.2 (1)
      T,
      
      # req3.3 (1)
      F,F,
      
      # req3.1 (2)
      T,T,T,T,T,
      T,T,T,T,T,
      
      # req3.2 (2)
      T,
      
      # req3.3 (2)
      F,F,
      
      # req3.1 (3)
      T,T,T,T,T,
      T,T,T,T,T,
      
      # req3.2 (3)
      T,
      
      # req3.3 (3)
      F,F,
      
      # req3.1 (4)
      T,T,T,T,T,
      T,T,T,T,T,
      
      # req3.2 (4)
      T,
      
      # req3.3 (4)
      F,F,
      
      # req3.1 (5)
      T,T,T,T,T,
      T,T,T,T,T,
      
      # req3.2 (5)
      T
    )
)


## Print/Jilid Request ----

P4 <- data.frame(
  question  = 
    c(
      # Pertanyaan 4.0 print/jilid 
      "Print/Jilid Request","Print/Jilid Request",
      
      # Pertanyaan 4.1 (1)
      "Print/Jilid detail (1)","Print/Jilid detail (1)",
      "Print/Jilid detail (1)","Print/Jilid detail (1)",
      
      # Pertanyaan 4.2 (1)
      "Quantity print/jilid (1)",
      
      # Pertanyaan 4.3 (1)
      "More print/jilid request","More print/jilid request",
      
      # Pertanyaan 4.1 (2)
      "Print/Jilid detail (2)","Print/Jilid detail (2)",
      "Print/Jilid detail (2)","Print/Jilid detail (2)",
      
      # Pertanyaan 4.2 (2)
      "Quantity print/jilid (2)",
      
      # Pertanyaan 4.3 (2)
      "More print/jilid request","More print/jilid request",
      
      # Pertanyaan 4.1 (3)
      "Print/Jilid detail (3)","Print/Jilid detail (3)",
      "Print/Jilid detail (3)","Print/Jilid detail (3)",
      
      # Pertanyaan 4.2 (3)
      "Quantity print/jilid (3)"
    ),
  option    = 
    c(
      # Ans4.0 print/jilid 
      "Yes","No",
      
      # Ans4.1 [print/jilid detail 1]
      "Certificate","Banner","Coursebook","Syllabus",
      
      # Ans4.2 [print/jilid qty 1]
      "Quantity print/jilid request 1",
      
      # Ans4.3 
      "Yes","No",
      
      # Ans4.1 [print/jilid detail 2]
      "Certificate","Banner","Coursebook","Syllabus",
      
      # Ans4.2 [print/jilid qty 2]
      "Quantity print/jilid request 2",
      
      # Ans4.3 
      "Yes","No",
      
      # Ans4.1 [print/jilid detail 3]
      "Certificate","Banner","Coursebook","Syllabus",
      
      # Ans4.2 [print/jilid qty 3]
      "Quantity print/jilid request 3"
      
    ),
  input_type= 
    c(
      # type4.0 print/jilid
      "select","select",
      
      # type4.1 print/jilid detail (1)
      "select","select","select","select",
      
      # type4.2
      "numeric",
      
      # type4.3
      "mc","mc",
      
      # type4.1 print/jilid detail (2)
      "select","select","select","select",
      
      # type4.2
      "numeric",
      
      # type4.3
      "mc","mc",
      
      # type4.1 print/jilid detail (3)
      "select","select","select","select",
      
      # type4.2
      "numeric"
    ),
  input_id  = 
    c(
      # id4.0
      "poj","poj",
      
      # id4.1 (1)
      "poj1","poj1","poj1","poj1",
      
      # id4.2 (1)
      "qpoj1",
      
      # id4.3 (1)
      "more5","more5",
      
      # id4.1 (2)
      "poj2","poj2","poj2","poj2",
      
      # id4.2 (2)
      "qpoj2",
      
      # id4.3 (2)
      "more6","more6",
      
      # id4.1 (3)
      "poj3","poj3","poj3","poj3",
      
      # id4.2 (3)
      "qpoj3"
    ), 
  dependence = 
    c(
      # depend4.0
      NA,NA,
      
      # depend4.1 [poj1 ~ poj]
      "poj","poj","poj","poj",
      
      # depend4.2 [qpoj1 ~ poj]
      "poj",
      
      # DEPEND4.3
      "poj","poj",
      
      # depend4.1 [poj2 ~ more5]
      "more5","more5","more5","more5",
      
      # depend4.2 [qpoj2 ~ more5]
      "more5",
      
      # depend4.3
      "more5","more5",
      
      # depend4.1 [poj3 ~ more6]
      "more6","more6","more6","more6",
      
      # depend4.2 [poj3 ~ poj]
      "more6"
    ),
  dependence_value = 
    c(
      # val4.0
      NA,NA,
      
      # val4.1 [poj1 ~ poj]
      "Yes","Yes","Yes","Yes",
      
      # val4.2 [qpoj1 ~ poj]
      "Yes",
      
      # val4.3
      "Yes","Yes",
      
      # val4.1 [poj2 ~ poj]
      "Yes","Yes","Yes","Yes",
      
      # val4.2 [qpoj2 ~ poj]
      "Yes",
      
      # val4.3
      "Yes","Yes",
      
      # val4.1 [poj3 ~ poj]
      "Yes","Yes","Yes","Yes",
      
      # val4.2 [qpoj3 ~ poj]
      "Yes"
    ),
  required  = 
    c(
      # req4.0
      F,F,
      
      # req4.1 (1)
      T,T,T,T,
      
      # req4.2 (1)
      T,
      
      # req4.3 (1)
      F,F,
      
      # req4.1 (2)
      T,T,T,T,
      
      # req4.2 (2)
      T,
      
      # req4.3 (2)
      F,F,
      
      # req4.1 (3)
      T,T,T,T,
      
      # req4.2 (3)
      T
    )
)


## Non merch or Print/jilid request ----

P5 <- data.frame(
  question  = 
    c(# Pertanyaan 5.0 non merch 
      "Non merch  request","Non merch  request",
      
      # Pertanyaan 5.1 Non merch  
      "Non merch detail (1)","Non merch detail (1)","Non merch detail (1)",
      "Non merch detail (1)","Non merch detail (1)","Non merch detail (1)",
      "Non merch detail (1)","Non merch detail (1)",
      
      # Pertanyaan 5.2
      "Quantity Non merch (1)",
      
      # Pertanyaan 5.3
      "More non merch request","More non merch request",
      
      # Pertanyaan 5.1 Non merch  
      "Non merch detail (2)","Non merch detail (2)","Non merch detail (2)",
      "Non merch detail (2)","Non merch detail (2)","Non merch detail (2)",
      "Non merch detail (2)","Non merch detail (2)",
      
      # Pertanyaan 5.2.2
      "Quantity Non merch (2)",
      
      # Pertanyaan 5.2.3
      "More non merch request","More non merch request",
      
      # Pertanyaan 5.1 Non merch  
      "Non merch detail (3)","Non merch detail (3)","Non merch detail (3)",
      "Non merch detail (3)","Non merch detail (3)","Non merch detail (3)",
      "Non merch detail (3)","Non merch detail (3)",
      
      # Pertanyaan 5.3.2
      "Quantity Non merch (3)"
    ),
  option    = 
    c(
      # Ans5.0 Non merch 
      "Yes","No",
      
      # Ans5.1 (1)
      "Grab Time Policy Request",
      "Pembuatan Plakat",
      "Meals Benefit for Event",
      "Pembelian Barang",
      "Event Tools Support",
      "Asset Request",
      "Venue Request",
      "Pengiriman Barang/Dokumen",
      
      # Ans5.2 (1) [quantity]
      "Quantity Non merch  request 1",
      
      # Ans5.2 (1)
      "Yes","No",
      
      # Ans5.1 (2)
      "Grab Time Policy Request",
      "Pembuatan Plakat",
      "Meals Benefit for Event",
      "Pembelian Barang",
      "Event Tools Support",
      "Asset Request",
      "Venue Request",
      "Pengiriman Barang/Dokumen",
      
      # Ans5.2 (2) [quantity]
      "Quantity Non merch  request 2",
      
      # Ans5.3 (2)
      "Yes","No",
      
      # Ans5.1 (3) 
      "Grab Time Policy Request",
      "Pembuatan Plakat",
      "Meals Benefit for Event",
      "Pembelian Barang",
      "Event Tools Support",
      "Asset Request",
      "Venue Request",
      "Pengiriman Barang/Dokumen",
      
      # Ans5.2 (1) [quantity]
      "Quantity Non merch  request 3"
    ),
  
  input_type= 
    c(
      # type5.0 Non merch 
      "select","select",
      
      # type5.1 (1)
      "select","select","select","select",
      "select","select","select","select",
      
      # type5.2 (1)
      "numeric",
      
      # type5.3 (1)
      "mc","mc",
      
      # type5.1 (2)
      "select","select","select","select",
      "select","select","select","select",
      
      # type5.2 (2)
      "numeric",
      
      # type5.3 (2)
      "mc","mc",
      
      # type5.1 (3)
      "select","select","select","select",
      "select","select","select","select",
      
      # type5.2 (3)
      "numeric"
    ),
  input_id  = 
    c(
      # id5.0 (non merch 
      "nmr","nmr",
      
      # id5.1 (1)
      "nmr1","nmr1","nmr1","nmr1",
      "nmr1","nmr1","nmr1","nmr1",
      
      # id5.2 (1)
      "qnm1",
      
      # id5.3 (1)
      "more7","more7",
      
      # id5.1 (2)
      "nmr2","nmr2","nmr2","nmr2",
      "nmr2","nmr2","nmr2","nmr2",
      
      # id5.2 (2)
      "qnm2",
      
      # id5.3 (2)
      "more8","more8",
      
      # id5.1 (3)
      "nmr3","nmr3","nmr3","nmr3",
      "nmr3","nmr3","nmr3","nmr3",
      
      # id5.2 (3)
      "qnm3"
    ), 
  dependence = 
    c(
      # depend5.0
      NA,NA,
      
      # depend5.1 (1) [nmr1 ~ nmr]
      "nmr","nmr","nmr","nmr",
      "nmr","nmr","nmr","nmr",
      
      # depend5.2 (1)
      "nmr",
      
      # depend5.3 (1)
      "nmr","nmr",
      
      # depend5.1 (2) [nmr2 ~ more7]
      "more7","more7","more7","more7",
      "more7","more7","more7","more7",
      
      # depend5.2 (2)
      "more7",
      
      # depend5.3 (2)
      "more7","more7",
      
      # depend5.1 (3) [nmr3 ~ more8]
      "more8","more8","more8","more8",
      "more8","more8","more8","more8",
      
      # depend5.2 (3)
      "more8"
    ),
  dependence_value = 
    c(
      # val5.0
      NA,NA,
      
      # val5.1 (1)
      "Yes","Yes","Yes","Yes",
      "Yes","Yes","Yes","Yes",
      
      # val5.2 (1)
      "Yes",
      
      # val5.3 (1)
      "Yes","Yes",
      
      # val5.1 (2)
      "Yes","Yes","Yes","Yes",
      "Yes","Yes","Yes","Yes",
      
      # val5.2 (2)
      "Yes",
      
      # val5.3 (2)
      "Yes","Yes",
      
      # val5.1 (3)
      "Yes","Yes","Yes","Yes",
      "Yes","Yes","Yes","Yes",
      
      # val5.2 (3)
      "Yes"
    ),
  required  = 
    c(
      # req5.0
      F,F,
      
      # req5.1 (1)
      T,T,T,T,
      T,T,T,T,
      
      # req5.2 (1)
      T,
      
      # req5.3 (1)
      F,F,
      
      # req5.1 (2)
      T,T,T,T,
      T,T,T,T,
      
      # req5.2 (2)
      T,
      
      # req5.3 (2)
      F,F,
      
      # req5.1 (3)
      T,T,T,T,
      T,T,T,T,
      
      # req5.2 (3)
      T
    )
)

## Other request ----

P6 <- data.frame(
  question  = 
    c(
      # Pertanyaan 6.0 others
      "Other request","Other request",
      
      # Pertanyaan 6.1 (1)
      "Other request detail (1) : item 1, qty",
      
      # Pertanyaan 6.2 (1)
      "Any other request","Any other request",
      
      # Pertanyaan 6.1 (2)
      "Other request detail (2) : item 2, qty",
      
      # Pertanyaan 6.2 (2)
      "Any other request","Any other request",
      
      # Pertanyaan 6.3 (3)
      "Other request detail (3) : item 3, qty"
    ),
  option    = 
    c(
      # Ans6.0 others
      "Yes","No",
      
      # Ans6.1 (1) [other 1]
      "exp: item name , 1",
      
      # Ans6.2 (1)
      "Yes","No",
      
      # Ans6.1 (2) [other 2]
      "exp: item name, 2",
      
      # Ans6.2 (2) 
      "Yes","No",
      
      # Ans6.3 (3) [other 3]
      "exp: item name, 3"
    ),
  input_type= 
    c(
      # type6.0 other
      "select","select",
      
      # type6.1 (1) 
      "text",
      
      # type6.2
      "mc","mc",
      
      # type6.1 (2)
      "text",
      
      # type6.2
      "mc","mc",
      
      # type6.1 (3) 
      "text"
    ),
  input_id  = 
    c(
      # id6.0
      "oth","oth",
      
      # id6.1 (1)
      "oth1",
      
      # id6.2 (1)
      "more9","more9",
      
      # id6.1 (2)
      "oth2",
      
      # id6.2 (2)
      "more10","more10",
      
      # id6.1 (3)
      "oth3"
    ),
  dependence = 
    c(
      # depend6.0
      NA,NA,
      
      # depend6.1 (1) [oth1 ~ oth]
      "oth",
      
      # depend6.2 (1)
      "oth","oth",
      
      # depend6.1 (2) [oth2 ~ more9]
      "more9",
      
      # depend6.2 (2)
      "more9","more9",
      
      # depend6.1 (3) [oth3 ~ more10]
      "more10"
    ),
  dependence_value = 
    c(
      # val6.0
      NA,NA,
      
      # val6.1 (1) [oth1 ~ oth]
      "Yes",
      
      # val6.2 (1)
      "Yes","Yes",
      
      # val6.1 (2) [oth2 ~ more9]
      "Yes",
      
      # val6.2 (2)
      "Yes","Yes",
      
      # val6.3 (1) [oth3 ~ more10]
      "Yes"
    ),
  required  = 
    c(
      # req6.0
      F,F,
      
      # req6.1 (1)
      T,
      
      # req6.2 (1)
      F,F,
      
      # req6.1 (2)
      T,
      
      # req6.2 (2)
      F,F,
      
      # req6.3 (3)
      T
    )
)

## Info Detail ----
P7 <- data.frame(
  question  = c("Further details"),
  option    = c("exp: Req for doorprize P4DA wizard night"),
  input_type= c("text"),
  input_id  = c("detail"), 
  dependence = c(NA),
  dependence_value = c(NA),
  required  = c(T)
)

## Request Status ----
P8 <- data.frame(
  question  = c("Status","Status","Status","Status"),
  option    = c("In Progress","Done","Cancel","Blocked"),
  input_type= c("select"),
  input_id  = c("sts"), 
  dependence = c(NA),
  dependence_value = c(NA),
  required  = c(T)
)

## Reason canceled/blocked
P9 <- data.frame(
  question  = c("Reason canceled","Reason blocked"),
  option    = c("Why the request was canceled","Why the request was blocked"),
  input_type= c("text","text"),
  input_id  = c("whyc","whyb"), 
  dependence = c("sts","sts"),
  dependence_value = c("Cancel","Blocked"),
  required  = c(T,T)
)
# Compiling question ----
df <- rbind(req_date,due_dates,P1,P2,P3,P4,P5,P6,P7,P8,P9,done_date,cancel_date,block_date)

# Sapply Function ----
reqs <- 
  function(x) {
    if (x == "Notebook" |
        x == "Pullpen" |
        x == "Lanyard" |
        x == "Sticker" |
        x == "Tumbler" |
        x == "Mug" |
        x == "Totebag" |
        x == "Pouch" |
        x == "Neck Pillow" |
        x == "T-Shirt")
    {
      request = "Merchandise Request"
    }
    else if (x == "Certificate" |
             x == "Banner" |
             x == "Coursebook" |
             x == "Syllabus")
    {
      request = "Request Print/Jilid"
    }
    else if (x == "Grab Time Policy Request")
    {
      request = "Grab Time Policy Request"
    }
    else if (x == "Pengiriman Barang/Dokumen")
    {
      request = "Pengiriman Barang/Dokumen"
    }
    else if (x == "Meals Benefit for Event")
    {
      request = "Meals Benefit for Event"
    }
    else if (x == "Pembuatan Plakat")
    {
      request = "Pembuatan Plakat"
    }
    else if (x == "Pembelian Barang")
    {
      request = "Pembelian Barang"
    }
    else if (x == "Venue Request")
    {
      request = "Venue Request"
    }
    else if (x == "Event Tools Support")
    {
      request = "Event Tools Support"
    }
    else if (x == "Asset Request")
    {
      request = "Asset Request"
    } else
    {
      request = "Other Request"
    }
  }

alesan <- function(x){
  if (x == "Cancel") {
    alasan = whyc
  } else if (x == "Blocked"){
    alasan = whyb
  } else {
    alasan = "-"
  }
}


sla <- function(x){
  
  if (x == "Grab Time Policy Request") {
    SLA = "2 Days"
  } else if (x == "Merchandise Request" |
             x == "Pengiriman Barang/Dokumen") {
    SLA = "3 Days"
  } else if (x == "Meals Benefit for Event") {
    SLA = "1 Day"
  } else if (x == "Pembuatan Plakat") {
    SLA = "8 Days"
  } else if (x == "Pembelian Barang" |
             x == "Venue Request") {
    SLA = "14 Days"
  } else if (x == "Event Tools Support" |
             x == "Request Print/Jilid") {
    SLA = "7 Days"
  } else {
    SLA = "5 Days"
  }
}


urg <- function(x){
  if(x <= 2){
    urgency = "High"
  } else if(x <= 7){
    urgency = "Medium"
  } else {
    urgency = "Low"
  }
}


stat_sla <- function(x){
  if (x == "Grab Time Policy Request") {
    selisih = due_date - est_done
    if(selisih >= 2){
      stat_SLA = "Sesuai SLA"
    } else {
      stat_SLA = "Tidak sesuai SLA"
    }
  } else if (x == "Merchandise Request" |
             x == "Pengiriman Barang/Dokumen") {
    selisih = due_date - est_done
    if(selisih >= 3){
      stat_SLA = "Sesuai SLA"
    } else {
      stat_SLA = "Tidak sesuai SLA"
    }
  } else if (x == "Meals Benefit for Event") {
    selisih = due_date - est_done
    if(selisih >= 1){
      stat_SLA = "Sesuai SLA"
    } else {
      stat_SLA = "Tidak sesuai SLA"
    }
  } else if (x == "Pembuatan Plakat") {
    selisih = due_date - est_done
    if(selisih >= 8){
      stat_SLA = "Sesuai SLA"
    } else {
      stat_SLA = "Tidak sesuai SLA"
    }
  } else if (x == "Pembelian Barang" |
             x == "Venue Request") {
    selisih = due_date - est_done
    if(selisih >= 14){
      stat_SLA = "Sesuai SLA"
    } else {
      stat_SLA = "Tidak sesuai SLA"
    }
  } else if (x == "Event Tools Support" |
             x == "Request Print/Jilid") {
    selisih = due_date - est_done
    if(selisih >= 7){
      stat_SLA = "Sesuai SLA"
    } else {
      stat_SLA = "Tidak sesuai SLA"
    }
  } else {
    selisih = due_date - est_done
    if(selisih >= 5){
      stat_SLA = "Sesuai SLA"
    } else {
      stat_SLA = "Tidak sesuai SLA"
    }
  }
}






