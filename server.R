shinyServer(function(input, output, session) {
  # Navbar ------------------------------------------------------------------
  shinyjs::addClass(id = "navBar", class = "navbar-right")
  
  #  Panel 1  ------------------------------------------------------------------
  
  output$banner <- renderImage({
    list(src = "www/coverpage.png",
         width = "100%",
         height = "100%")
    
  }, deleteFile = F) # close of PANEL 1
  
  #  Panel 2  ------------------------------------------------------------------
  
  output$tabel_sla <- renderTable(
    sla_tabel %>% 
      as.data.frame() %>% 
      mutate(No = as.integer(No)) %>% 
      filter(No <= 10) %>% 
      select(No, Items, PIC, Service_Working_Time,Service_Performance) %>% 
      rename("Request" = Items,
             "Service Working Time" = Service_Working_Time,
             "Service Performance" = Service_Performance)
 
  ) # close of PANEL 2
  
  #  Panel 3  ------------------------------------------------------------------
  renderSurvey()
  
  observeEvent(input$submit, {
    # popup text on submit
    showModal(
      modalDialog(
        title = "Congrats, you completed your request application!",
        "You can monitor the progress of your request in the request status panel.",
        "after close this notification please refresh the page!",
      )
    )
    
    # menyimpan data hasil submit ke gsheet
    res_data <-
      getSurveyData(include_dependencies = TRUE,
                    dependency_string = "-") %>%
      select(question_id, response) %>%
      pivot_wider(names_from = question_id, values_from = response) %>%
      
      # menyimpan informasi nama
      mutate(user =
               if (tim == "Business Development") {
                 user = BD
               } else if (tim == "Marketing") {
                 user = mkt
               } else {
                 user = pro
               }) %>%
      
      # menyimpan informasi detail request
      mutate(item =
               if (request == "Merchandise Request") {
                 item = merch
               } else {
                 item = print
               },
             # membuat kolom tambahan
             status = "In Progress",
             done_date = NA) %>%
      
      # membuat perhitungan SLA
      mutate(
        req_date = ymd(req_date),
        due_date = ymd(due_date),
        SLA =
               if (request == "Grab Time Policy Request") {
                 SLA = "2 Days"
               } else if (request == "Merchandise Request" |
                          request == "Pengiriman Barang / Dokumen") {
                 SLA = "3 Days"
               } else if (request == "Meals Benefit for Event") {
                 SLA = "1 Day"
               } else if (request == "Pembuatan Plakat") {
                 SLA = "8 Days"
               } else if (request == "Pembelian Barang" |
                          request == "Venue Request") {
                 SLA = "14 Days"
               } else if (request == "Event Tools Support" |
                          request == "Request Print / Jilid") {
                 SLA = "7 Days"
               } else {
                 SLA = "5 Days"
               })
      
    # membuat estimasi tanggal selesai
    
    if (res_data$request == "Grab Time Policy Request") {
      est_done = res_data$req_date + days(2)
      hari = as.character(wday(est_done, label = TRUE, abbr = F))
      if (hari %in% c("Saturday", "Sunday")) {
        est_done = est_done + days(2)
      }
    } else if (res_data$request == "Merchandise Request" |
               res_data$request == "Pengiriman Barang / Dokumen") {
      est_done = res_data$req_date + days(3)
      hari = as.character(wday(est_done, label = TRUE, abbr = F))
      if (hari %in% c("Saturday", "Sunday")) {
        est_done = est_done + days(2)
      }
    } else if (res_data$request == "Meals Benefit for Event") {
      est_done = res_data$req_date + days(1)
      hari = as.character(wday(est_done, label = TRUE, abbr = F))
      if (hari %in% c("Saturday", "Sunday")) {
        est_done = est_done + days(2)
      }
    } else if (res_data$request == "Pembuatan Plakat") {
      est_done = res_data$req_date + days(10)
      hari = as.character(wday(est_done, label = TRUE, abbr = F))
      if (hari %in% c("Saturday", "Sunday")) {
        est_done = est_done + days(2)
      }
    } else if (res_data$request == "Pembelian Barang" |
               res_data$request == "Venue Request") {
      est_done = req_date + days(18)
      hari = as.character(wday(est_done, label = TRUE, abbr = F))
      if (hari %in% c("Saturday", "Sunday")) {
        est_done = est_done + days(2)
      }
    } else if (res_data$request == "Event Tools Support" |
               res_data$request == "Request Print / Jilid") {
      est_done = res_data$req_date + days(9)
      hari = as.character(wday(est_done, label = TRUE, abbr = F))
      if (hari %in% c("Saturday", "Sunday")) {
        est_done = est_done + days(2)
      }
    } else if (res_data$request == "Asset Request") {
      est_done = res_data$req_date + days(7)
    } else {
      est_done = res_data$req_date + days(1)
      if (hari %in% c("Saturday", "Sunday")) {
        est_done = est_done + days(2)
      }
    }
    
    # menyimpan nilai est_done
    res_data$est_done <- est_done
    
    # mengatur ulang kolom yang akan disimpan di google sheet
    res_data <- res_data %>% select(
        req_date,
        est_done,
        due_date,
        tim,
        user,
        request,
        item,
        qty,
        detail,
        status,
        done_date,
        SLA
      )
    
    
    # pemeriksaan dan pengaturan untuk
    # menyimpan respon data pada google sheet.
    
    values <- read_sheet(ss = sheet_id,
                         sheet = "main")
    
    if (nrow(values) == 0) {
      sheet_write(data = res_data,
                  ss = sheet_id,
                  sheet = "main")
    } else {
      sheet_append(data = res_data,
                   ss = sheet_id,
                   sheet = "main")
    }
    
    
    
  }) # close of PANEL 3
  
  #  Panel 4  ------------------------------------------------------------------
  # membaca data dari gsheet 
  url <- "https://docs.google.com/spreadsheets/d/1rc6x9cOuE4cvWMg2-Poox4ta86WnBRYR3lXFEDCkrGY/edit?usp=sharing"
  udata <- read_sheet(url)
  
  # mengambil data request berstatus in progress
  list_op <- udata %>% as.data.frame() %>% 
    filter(status %in% "In Progress") %>% 
    arrange(desc(req_date))%>%
    mutate(req_date = date(req_date),
           est_done = date(est_done),
           due_date = date(due_date),
           SLA = as.character(SLA)) %>% 
    select(tim,user,request,item,qty,req_date,due_date,est_done,SLA) %>% 
    rename("Division"= tim,
           "User" = user,
           "Request" = request,
           "Detail" = item,
           "Quantity" = qty,
           "Request Date" = req_date,
           "Due Date" = due_date,
           "Estimated Finish Date" = est_done)
  
  # mengambil data request berstatus done
  list_rd <- udata %>% as.data.frame() %>% 
    filter(status %in% "Done") %>% 
    arrange(desc(req_date))%>%
    mutate(done_date = date(req_date),
           est_done = date(est_done)) %>% 
    select(tim,user,request,item,qty,est_done,done_date)%>% 
    rename("Division"= tim,
           "User" = user,
           "Request" = request,
           "Detail" = item,
           "Quantity" = qty,
           "Done Date" = done_date,
           "Estimated Finish Date" = est_done)
  
  # mengambil data request berstatus Cancel/Blocked
  list_rcb <- udata %>% as.data.frame() %>% 
    filter(status %in% "Cancel" | status %in% "Blocked") %>% 
    arrange(desc(req_date))%>%
    mutate(done_date = date(req_date)) %>% 
    select(tim,user,request,item,qty,done_date)%>% 
    rename("Division"= tim,
           "User" = user,
           "Request" = request,
           "Detail" = item,
           "Quantity" = qty,
           "Canceled/Blocked Date" = done_date)
  
  # rander tabel data in progres
  output$table_rip <- renderDataTable(list_op, options = list(scrollX = TRUE,pageLength = 5))
  
  # rander tabel data done
  output$table_rd <- renderDataTable(list_rd, options = list(scrollX = TRUE,pageLength = 5))
  
  # rander tabel data cancel/blocked
  output$table_rcb <- renderDataTable(list_rcb, options = list(scrollX = TRUE,pageLength = 5))
  
  
  #  Panel 5  ------------------------------------------------------------------
  # infobox
  output$box_tr <- renderInfoBox({
    list_tr <- udata %>% as.data.frame() %>%
      arrange(desc(req_date)) %>%
      mutate(req_date = date(req_date)) %>%
      filter(req_date >= input$tgl[1] & req_date <= input$tgl[2])
    
    infoBox(
      title = HTML("Submitted"),
      value = length(list_tr[, 3]),
      color = "yellow",
      icon = icon("code-pull-request"),
      fill = T
    )
  })
  
  output$box_rip <- renderInfoBox({
    list_op1 <- udata %>% as.data.frame() %>%
      filter(status %in% "In Progress") %>%
      arrange(desc(req_date)) %>%
      mutate(req_date = date(req_date)) %>%
      filter(req_date >= input$tgl[1] & req_date <= input$tgl[2])
    
    infoBox(
      title = HTML("In Progress"),
      value = length(list_op1[, 3]),
      color = "orange",
      icon = icon("gears"),
      fill = T
    )
  })
  
  output$box_rd <- renderInfoBox({
    list_rd1 <- udata %>% as.data.frame() %>%
      filter(status %in% "Done") %>%
      arrange(desc(req_date)) %>%
      mutate(req_date = date(req_date)) %>%
      filter(req_date >= input$tgl[1] & req_date <= input$tgl[2])
    
    infoBox(
      title = HTML("Done"),
      value = length(list_rd1[, 3]),
      color = "red",
      icon = icon("circle-check"),
      fill = T
    )
  })
  
  output$box_rcb <- renderInfoBox({
    list_rcb1 <- udata %>% as.data.frame() %>%
      filter(status %in% "Cancel" | status %in% "Blocked") %>%
      arrange(desc(req_date)) %>%
      mutate(req_date = date(req_date)) %>%
      filter(req_date >= input$tgl[1] & req_date <= input$tgl[2])
    
    infoBox(
      title = HTML("Canceled or Blocked"),
      value = length(list_rcb1[, 3]),
      color = "black",
      icon = icon("ban"),
      fill = T
    )
  })
  
  # membuat grafik
  # Line chart [plot1]
  output$plot1 <- renderPlot({
    
    req_count <- udata %>% 
      mutate(req_date = ymd(req_date)) %>% 
      group_by(req_date) %>% 
      filter( req_date >= input$tgl[1] & req_date <= input$tgl[2]) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      mutate(tulisan = glue(
        "Date : {req_date}
        Submitted : {cnt} times
        "
      ))
    
    ggplot(req_count, mapping = aes(x = req_date,y = cnt))+
      geom_line(col = "red")+
      geom_point(aes(text=tulisan), col="black", size = 5) +
      geom_text(aes(label = cnt), colour = "white")+
      labs(title = "SUBMITTED REQUEST BY TIME",
           x = "Request Date",
           y = "Submitted Request")+
      theme(legend.position = "none",
            plot.title = element_text(colour = "#d63d2d", face = "bold", hjust = 0.5))
    
    #ggplotly(plot1,tooltip = "text")
  })
  
  # Bar chart [plot2]
  output$plot2 <- renderPlot({
    #udata <- read_sheet(url)
    req_list <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(req_date >= input$tgl[1] & req_date <= input$tgl[2]) %>% 
      group_by(request) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "{request}
        Submitted {cnt} times"
      ))
    
    ggplot(req_list, mapping = aes(y = reorder(request, cnt),x = cnt, text = tulisan))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#FCBF49", high = "#581f18")+
      geom_text(aes(label = cnt), hjust = 2, colour = "white")+
      labs(title = "SUBMITTED REQUEST BY TYPE",
           y = NULL,
           x = "Submitted Request")+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(colour = "#d63d2d", face = "bold", hjust = 0.2))
    
    #ggplotly(plot2,tooltip = "text")
  })
  
  # Bar chart [plot3]
  output$plot3 <- renderPlot({
    #udata <- read_sheet(url)
    req_list <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(req_date >= input$tgl[1] & req_date <= input$tgl[2]) %>% 
      group_by(tim) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "Division : {tim}
        Total Submitted : {cnt} times
        "
      ) )
    
    ggplot(req_list, mapping = aes(x = reorder(tim, -cnt),y =cnt, text = tulisan  ))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#f0a202", high = "#581f18")+
      geom_text(aes(label = cnt), vjust = 2, colour = "white")+
      labs(title = "SUBMITTED REQUEST BY DIVISION",
           y = "Submitted Request",
           x = NULL)+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(colour = "#d63d2d", face = "bold", hjust = 0.4))
    
    #ggplotly(plot3, tooltip = "text")
  })
  
  # bar chart [plot4]
  output$plot4 <- renderPlot({
    udata <- read_sheet(url)
    req_list <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(req_date >= input$tgl[1] & req_date <= input$tgl[2]) %>% 
      filter( request == "Merchandise Request") %>% 
      group_by(item) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "{item}
        Submitted {cnt} times"
      ))
    
    ggplot(req_list, mapping = aes(y = reorder(item, cnt),x = cnt, text = tulisan))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#FCBF49", high = "#581f18")+
      geom_text(aes(label = cnt), hjust = 2, colour = "white")+
      labs(title = "SUBMITTED MERCHANDISE REQUEST",
           y = NULL,
           x = "Submitted Request")+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(colour = "#d63d2d", face = "bold", hjust = 0.5))
    
    #ggplotly(plot4,tooltip = "text")
  })
  
  # Line chart [plot5]
  output$plot5 <- renderPlot({
    
    req_count <- udata %>% 
      filter(tim %in% input$team1) %>% 
      mutate(req_date = ymd(req_date)) %>% 
      group_by(req_date) %>% 
      filter( req_date >= input$tgl1[1] & req_date <= input$tgl1[2]) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      mutate(tulisan = glue(
        "Date : {req_date}
        Submitted : {cnt} times
        "
      ))
    
    ggplot(req_count, mapping = aes(x = req_date,y = cnt ))+
      geom_line(col = "red")+
      geom_point(aes(text = tulisan),col="black", size=5)+
      geom_text(aes(label = cnt), colour = "white")+
      labs(title = "SUBMITTED REQUEST BY TIME",
           x = "Request Date",
           y = "Submitted Request")+
      theme_minimal()+
      theme(legend.position = "none",
            plot.title = element_text(colour = "#d63d2d", face = "bold", hjust = 0.5))
    
    #ggplotly(plot5, tooltip = "text")
  })
  
  # bar chart [plot6]
  output$plot6 <- renderPlot({
    
    list_op2 <- udata %>% as.data.frame() %>%
      filter(status %in% "In Progress") %>%
      arrange(desc(req_date)) %>%
      mutate(req_date = date(req_date)) %>%
      filter(req_date >= input$tgl1[1] & req_date <= input$tgl1[2]) %>% 
      filter(tim %in% input$team1) %>% 
      select(request) 
    
    list_rd2 <- udata %>% as.data.frame() %>%
      filter(status %in% "Done") %>%
      arrange(desc(req_date)) %>%
      mutate(req_date = date(req_date)) %>%
      filter(req_date >= input$tgl1[1] & req_date <= input$tgl1[2]) %>% 
      filter(tim %in% input$team1) %>% 
      select(request) 
    
    list_rcb2 <- udata %>% as.data.frame() %>%
      filter(status %in% c("Cancel","Blocked")) %>%
      arrange(desc(req_date)) %>%
      mutate(req_date = date(req_date)) %>%
      filter(req_date >= input$tgl1[1] & req_date <= input$tgl1[2]) %>%
      filter(tim %in% input$team1) %>%
      select(request) 
    
    pie <- data.frame(status = as.factor(c("In Progress","Done","Cancel/Blocked")),
                      cnt = c(length(list_op2[,1]),
                              length(list_rd2[,1]),
                              length(list_rcb2[,1])
                              )) 
    
    ggplot(pie, mapping = aes(y = cnt,x = status))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#FCBF49", high = "#581f18")+
      geom_text(aes(label = cnt), vjust = 2, colour = "white")+
      labs(title = "REQUESTS STATUS",
           y = "Submitted Request",
           x = NULL)+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(colour = "#d63d2d", face = "bold", hjust = 0.5))
    
    #ggplotly(plot5, tooltip = "text")
  })
  
  # Bar plot [plot7]
  output$plot7 <- renderPlot({
    #udata <- read_sheet(url)
    req_list7 <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(tim %in% input$team1) %>%
      filter(req_date >= input$tgl[1] & req_date <= input$tgl[2]) %>% 
      group_by(request) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "{request}
        Submitted {cnt} times"
      ))
    
    ggplot(req_list7, mapping = aes(y = reorder(request, cnt),x = cnt, text = tulisan))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#FCBF49", high = "#581f18")+
      geom_text(aes(label = cnt), hjust = 2, colour = "white")+
      labs(title = "SUBMITTED REQUEST BY TYPE",
           y = NULL,
           x = "Submitted Request")+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(colour = "#d63d2d", face = "bold", hjust = 0.2))
    
    #ggplotly(plot7,tooltip = "text")
  })
  
  # Bar char [plot 8]
  output$plot8 <- renderPlot({
    udata <- read_sheet(url)
    req_list8 <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(tim %in% input$team1) %>%
      filter(req_date >= input$tgl[1] & req_date <= input$tgl[2]) %>% 
      filter( request == "Merchandise Request") %>% 
      group_by(item) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "{item}
        Submitted {cnt} times"
      ))
    
    ggplot(req_list8, mapping = aes(y = reorder(item, cnt),x = cnt, text = tulisan))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#FCBF49", high = "#581f18")+
      geom_text(aes(label = cnt), hjust = 2, colour = "white")+
      labs(title = "SUBMITTED MERCHANDISE REQUEST",
           y = NULL,
           x = "Submitted Request")+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(colour = "#d63d2d", face = "bold", hjust = 0.5))
    
    #ggplotly(plot4,tooltip = "text")
  })
  
}) # close of shiny server
