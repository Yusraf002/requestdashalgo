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

# ---- Setup gdrive & gsheet ----
options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = ".secrets"
  #googledrive::drive_auth(email = "yusraf@algorit.ma", cache = ".secrets")
)


# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
sheet_id <- drive_get("dashboard")$id

# ---- membaca data dari gsheet ----
url <- "https://docs.google.com/spreadsheets/d/1rc6x9cOuE4cvWMg2-Poox4ta86WnBRYR3lXFEDCkrGY/edit?usp=sharing"
udata <- read_sheet(url)
sla_tabel <- read_sheet(url,sheet = "SLA")

# # mengambil data request berstatus in progress
# list_op <- udata %>% as.data.frame() %>% 
#   filter(status %in% "In Progress") %>% 
#   arrange(desc(req_date))%>%
#   mutate(req_date = date(req_date),
#          est_done = date(est_done),
#          due_date = date(due_date)) %>% 
#   select(tim,user,request,item,qty,req_date,due_date,est_done,SLA)
# 
# # mengambil data request berstatus done
# list_rd <- udata %>% as.data.frame() %>% 
#   filter(status %in% "Done") %>% 
#   arrange(desc(req_date))%>%
#   mutate(done_date = date(req_date)) %>% 
#   select(done_date,tim,user,request,item,qty)
# 
# # mengambil data request berstatus Cancel/Blocked
# list_rcb <- udata %>% as.data.frame() %>% 
#   filter(status %in% "Cancel" | status %in% "Blocked") %>% 
#   arrange(desc(req_date))%>%
#   mutate(done_date = date(req_date)) %>% 
#   select(done_date,tim,user,request,item,qty)

# ---- Date Question ----
extendInputType("date", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-0,
    max = Sys.Date()+0
  )
})

extendInputType("date2", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-0,
    max = Sys.Date()+21
  )
})

req_date <- data.frame(question = "Request Date",
                       option = NA,
                       input_type = "date",
                       input_id = "req_date",
                       dependence = NA,
                       dependence_value = NA,
                       required = F)

due_dates <- data.frame(question = "for when this request was made",
                        option = NA,
                        input_type = "date2",
                        input_id = "due_date",
                        dependence = NA,
                        dependence_value = NA,
                        required = FALSE)

# ---- List Question ----
df2 <- data.frame(
  question = 
    c(
      # Pertanyaan 1
      "What is your team","What is your team","What is your team",
      
      # Pertanyaan 2.1 [BD]
      "What is your name","What is your name","What is your name",
      "What is your name","What is your name","What is your name",
      "What is your name","What is your name","What is your name",
      "What is your name",
      
      # Pertanyaan 2.2 [Marketing]
      "What is your name","What is your name","What is your name",
      "What is your name","What is your name","What is your name",
      "What is your name","What is your name",
      
      # Pertanyaan 2.3 [Product]
      "What is your name","What is your name","What is your name",
      "What is your name","What is your name","What is your name",
      "What is your name","What is your name","What is your name",
      "What is your name","What is your name","What is your name",
      "What is your name","What is your name","What is your name",
      "What is your name","What is your name","What is your name",
      "What is your name","What is your name",
      
      # Pertanyaan 3
      "What do you want to request","What do you want to request","What do you want to request",
      "What do you want to request","What do you want to request","What do you want to request",
      "What do you want to request","What do you want to request","What do you want to request",
      "What do you want to request",
      
      # Pertanyaan 4
      "Merchandise detail","Merchandise detail","Merchandise detail",
      "Merchandise detail","Merchandise detail","Merchandise detail",
      "Merchandise detail","Merchandise detail","Merchandise detail",
      "Merchandise detail",
      
      # Pertanyaan 5
      #"Delivery required","Delivery required",
      
      # Pertanyaan 6
      #"Write down the recipient's name and address",
      
      # Pertanyaan 7
      "Printing/binding details","Printing/binding details",
      "Printing/binding details","Printing/binding details",
      
      # Pertanyaan 8
      "How much you need",
      
      # Pertanyaan 9
      "Further details"
      
    ),
  option = 
    c(
      # Ans1
      "Business Development","Marketing","Product",
      
      # Ans2.1 [BD]
      "Anas","Andra Farah","Aprilia","Charisma","Geo","Helida",
      "Mohammad De Syaeful","Naya","Rara","Sari",
      
      # Ans2.2 [marketing]
      "Betsy","Brian Victor", "Bunga","Dhalif", 
      "Ekky","Kyan","Safa","Tifani",
      
      # Ans2.3 [product]
      "Arkana","Cut","Diva K","Dwi","Dzaky", 
      "Fini","Fiqey","Handoyo","Ichaa","Ido Ali",
      "Irfan","Jafar","Kevin","Kinan","Lita",
      "Rany","Tata","Victor","Wulan","Yusraf",
      
      # Ans3 [from SLA]
      "Grab Time Policy Request",
      "Merchandise Request",
      "Pembuatan Plakat",
      "Meals Benefit for Event",
      "Pembelian Barang",
      "Event Tools Support",
      "Asset Request",
      "Venue Request",
      "Pengiriman Barang / Dokumen",
      "Request Print / Jilid",
      
      
      # Ans4 [marchandise detail]
      "Notebook","Pullpen","Lanyard",
      "Sticker","Tumbler","Mug",
      "Totebag","Pouch","Neck Pillow",
      "T-Shirt",
      
      # Ans5 [shipping status]
      #"yes","no",
      
      # Ans6 [shipping detail]
      #"exp: Bastian, Jln. Rasuna said no.3",
      
      # Ans7 [print detail]
      "Certificate","Banner",
      "Coursebook","Syllabus",
      
      # Ans8 [quantity]
      "Quantity",
      
      # Ans9 [keterangan]
      "exp: doorprize P4DA wizard night "
    ),
  input_type = 
    c(
      # type1
      "mc","mc","mc",
      
      # type2.1[BD]
      "select","select","select","select","select",
      "select","select","select","select","select",
      
      # type2.2[marekting]
      "select","select","select","select","select",
      "select","select","select",
      
      # type2.1[product]
      "select","select","select","select","select",
      "select","select","select","select","select",
      "select","select","select","select","select",
      "select","select","select","select","select",
      
      # type3
      "select","select","select","select","select",
      "select","select","select","select","select",
      
      # type4
      "select","select","select","select","select",
      "select","select","select","select","select",
      
      # type5
      #"y/n","y/n",
      
      # type6
      #"text",
      
      # type7
      "mc","mc","mc","mc",
      
      # type8
      "numeric",
      
      # type9
      "text"
      
    ),
  input_id = 
    c(
      # id1
      "tim","tim","tim",
      
      # id2.1
      "BD","BD","BD","BD","BD", "BD","BD","BD","BD","BD",
      
      # id2.2
      "mkt","mkt","mkt","mkt","mkt","mkt","mkt","mkt",
      
      # id2.3
      "pro","pro","pro","pro","pro", "pro","pro","pro","pro","pro",
      "pro","pro","pro","pro","pro", "pro","pro","pro","pro","pro",
      
      # id3
      "request","request","request","request","request",
      "request","request","request","request","request",
      
      # id4
      "merch","merch","merch","merch","merch",
      "merch","merch","merch","merch","merch",
      
      # id5
      #"shipping","shipping",
      
      # id6
      #"recipient",
      
      # id7
      "print","print","print","print",
      
      # id8
      "qty",
      
      # id9
      "detail"
    ),
  dependence = 
    c(
      # depend1
      NA,NA,NA,
      
      # depend2.1 [BD ~ tim] = 10
      "tim","tim","tim","tim","tim", "tim","tim","tim","tim","tim",
      
      # depend2.2 [mkt ~ tim] = 8
      "tim","tim","tim","tim","tim", "tim","tim","tim",
      
      # depend2.1 [pro ~ tim] = 20
      "tim","tim","tim","tim","tim", "tim","tim","tim","tim","tim",
      "tim","tim","tim","tim","tim", "tim","tim","tim","tim","tim",
      
      # depend3
      NA,NA,NA,NA,NA, NA,NA,NA,NA,NA,
      
      # depend4 [merch ~ request]
      "request","request","request","request","request",
      "request","request","request","request","request",
      
      # depend5 [merch ~ request]
      #"request","request",
      
      # depend6 [recepient ~ shipping]
      #"shipping",
      
      # depend8 [print ~ request]
      "request","request","request","request",
      
      # depend8
      NA,
      
      # depend9
      NA
    ),
  dependence_value = 
    c(
      # val1
      NA,NA,NA,
      
      # val2.1
      "Business Development","Business Development","Business Development",
      "Business Development","Business Development","Business Development",
      "Business Development","Business Development","Business Development",
      "Business Development",
      
      # val2.2
      "Marketing","Marketing","Marketing","Marketing","Marketing",
      "Marketing","Marketing","Marketing",
      
      # val2.3
      "Product","Product","Product","Product","Product",
      "Product","Product","Product","Product","Product",
      "Product","Product","Product","Product","Product",
      "Product","Product","Product","Product","Product",
      
      # val3
      NA,NA,NA,NA,NA, NA,NA,NA,NA,NA,
      
      # val4 [merch ~ request]
      "Merchandise Request","Merchandise Request","Merchandise Request",
      "Merchandise Request","Merchandise Request","Merchandise Request",
      "Merchandise Request","Merchandise Request","Merchandise Request",
      "Merchandise Request",
      
      # val5 [merch ~ request]
      #"Merchandise Request","Merchandise Request",
      
      # val6 [recepient ~ shipping]
      #"yes",
      
      # val7 [print~ request]
      "Request Print / Jilid","Request Print / Jilid",
      "Request Print / Jilid","Request Print / Jilid",
      
      # val8
      NA,
      
      # val9
      NA
      
    ),
  required =
    c(
      # req1
      T,T,T,
      
      # req2
      T,T,T,T,T, T,T,T,T,T,
      T,T,T,T,T, T,T,T,
      T,T,T,T,T, T,T,T,T,T,
      T,T,T,T,T, T,T,T,T,T,
      
      # req3
      T,T,T,T,T, T,T,T,T,T,
      
      # req4
      T,T,T,T,T, T,T,T,T,T,
      
      # req5
      #T,T,
      
      # req6
      #T,
      
      # req7
      T,T,T,T,
      
      # req8
      T,
      
      # req9
      T
    )
)


