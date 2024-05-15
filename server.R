shinyServer(function(input, output, session) {
  # Navbar ------------------------------------------------------------------
  shinyjs::addClass(id = "navBar", class = "navbar-right")
  
  #  Panel 1: Home  ------------------------------------------------------------------
  
  output$banner <- renderImage({
    list(src = "www/coverpage.png",
         width = "100%",
         height = "100%")
    
  }, deleteFile = F) # close of PANEL 1
  
  #  Panel 2: SLA  ------------------------------------------------------------------
  
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
  
  #  Panel 3: req form ------------------------------------------------------------------
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
      mutate(
        mr1  = paste(mr1,  qmr1, sep = ","),
        mr2  = paste(mr2,  qmr2, sep = ","),
        mr3  = paste(mr3,  qmr3, sep = ","),
        mr4  = paste(mr4,  qmr4, sep = ","),
        mr5  = paste(mr5,  qmr5, sep = ","),
        poj1 = paste(poj1, qpoj1, sep = ","),
        poj2 = paste(poj2, qpoj2, sep = ","),
        poj3 = paste(poj3, qpoj3, sep = ","),
        nmr1 = paste(nmr1, qnm1, sep = ","),
        nmr2 = paste(nmr2, qnm2, sep = ","),
        nmr3 = paste(nmr3, qnm3, sep = ",")
      ) %>% 
      select(
        -c(
          mr,
          qmr1,
          qmr2,
          qmr3,
          qmr4,
          qmr5,
          poj,
          qpoj1,
          qpoj2,
          qpoj3,
          nmr,
          qnm1,
          qnm2,
          qnm3,
          oth,
          more1,
          more2,
          more3,
          more4,
          more5,
          more6,
          more7,
          more8,
          more9,
          more10
        )
      ) %>% 
      pivot_longer(
        cols = c(
          mr1,
          mr2,
          mr3,
          mr4,
          mr5,
          poj1,
          poj2,
          poj3,
          nmr1,
          nmr2,
          nmr3,
          oth1,
          oth2,
          oth3
        )
      ) %>%
      filter(value != "-") %>%
      separate(value, c("item", "qty"), sep = ",") %>% 
      mutate(request = sapply(X = item, FUN = reqs),
             item = str_to_title(item),
             user = str_to_title(nama),
             status = sts) %>% 
      filter(item != "-") %>% 
      select(-c(nama,name,sts))
    
    # menyimpan done dates
    for(i in 1:nrow(res_data)) {
      if (res_data[i, 'status'] == 'Done') {
        res_data[i, 'done_date'] = res_data[i, 'done_date']
      } else if (res_data[i, 'status'] == 'Blocked') {
        res_data[i, 'done_date'] = res_data[i, 'block_date']
      } else {
        res_data[i, 'done_date'] = res_data[i, 'cancel_date']
      }
    }
    
    # menyimpan alasan, menghitung SLA, dan penentuan Urgency
    res_data <- res_data %>% mutate(done_date = ymd(done_date)) %>%
      mutate(
        alasan = sapply(X = status, FUN = alesan),
        req_date = ymd(req_date),
        due_date = ymd(due_date),
        SLA = sapply(X = request, FUN = sla),
        selisih = due_date - req_date,
        urgency = sapply(X = selisih, FUN = urg)
      ) 
      
      # menghitung est done
      for (i in 1:nrow(res_data)) {
        if (res_data[i, "request"] == "Grab Time Policy Request") {
          res_data[i, "est_done"] = res_data[i, 'req_date'] + 2
          res_data$hari = wday(res_data$est_done, label = TRUE, abbr = F)
          if (res_data[i, "hari"] == "Saturday" |
              res_data[i, "hari"] == "Sunday") {
            res_data[i, "est_done"] = res_data[i, 'req_date'] + 2
          }
        } else if (res_data[i, "request"] == "Merchandise Request" |
                   res_data[i, "request"] == "Pengiriman Barang/Dokumen") {
          res_data[i, "est_done"] = res_data[i, 'req_date'] + 3
          res_data$hari = wday(res_data$est_done, label = TRUE, abbr = F)
          if (res_data[i, "hari"] == "Saturday" |
              res_data[i, "hari"] == "Sunday") {
            res_data[i, "est_done"] = res_data[i, 'req_date'] + 2
          }
        } else if (res_data[i, "request"] == "Meals Benefit for Event") {
          res_data[i, "est_done"] = res_data[i, 'req_date'] + 1
          res_data$hari = wday(res_data$est_done, label = TRUE, abbr = F)
          if (res_data[i, "hari"] == "Saturday" |
              res_data[i, "hari"] == "Sunday") {
            res_data[i, "est_done"] = res_data[i, 'req_date'] + 2
          }
        } else if (res_data[i, "request"] == "Pembuatan Plakat") {
          res_data[i, "est_done"] = res_data[i, 'req_date'] + 10
          res_data$hari = wday(res_data$est_done, label = TRUE, abbr = F)
          if (res_data[i, "hari"] == "Saturday" |
              res_data[i, "hari"] == "Sunday") {
            res_data[i, "est_done"] = res_data[i, 'req_date'] + 2
          }
        } else if (res_data[i, "request"] == "Pembelian Barang" |
                   res_data[i, "request"] == "Venue Request") {
          res_data[i, "est_done"] = res_data[i, 'req_date'] + 18
          res_data$hari = wday(res_data$est_done, label = TRUE, abbr = F)
          if (res_data[i, "hari"] == "Saturday" |
              res_data[i, "hari"] == "Sunday") {
            res_data[i, "est_done"] = res_data[i, 'req_date'] + 2
          }
        } else if (res_data[i, "request"] == "Event Tools Support" |
                   res_data[i, "request"] == "Request Print/Jilid") {
          res_data[i, "est_done"] = res_data[i, 'req_date'] + 9
          res_data$hari = wday(res_data$est_done, label = TRUE, abbr = F)
          if (res_data[i, "hari"] == "Saturday" |
              res_data[i, "hari"] == "Sunday") {
            res_data[i, "est_done"] = res_data[i, 'req_date'] + 2
          }
        } else if (res_data[i, "request"] == "Asset Request") {
          res_data[i, "est_done"] = res_data[i, 'req_date'] + 7
        } else {
          res_data[i, "est_done"] = res_data[i, 'req_date'] + 1
          res_data$hari = wday(res_data$est_done, label = TRUE, abbr = F)
          if (res_data[i, "hari"] == "Saturday" |
              res_data[i, "hari"] == "Sunday") {
            res_data[i, "est_done"] = res_data[i, 'req_date'] + 2
          }
        }
      }
    
    for (i in 1:nrow(res_data)) {
      if (res_data[i, "request"] == "Grab Time Policy Request") {
        res_data[i, "selisih"] = res_data[i, "due_date"] - res_data[i, "est_done"]
        if (res_data[i, "selisih"] >= 2) {
          res_data[i, "stat_SLA"] = "Sesuai SLA"
        } else {
          res_data[i, "stat_SLA"] = "Tidak sesuai SLA"
        }
      } else if (res_data[i, "request"] == "Merchandise Request" |
                 res_data[i, "request"] == "Pengiriman Barang/Dokumen") {
        res_data[i, "selisih"] = res_data[i, "due_date"] - res_data[i, "est_done"]
        if (res_data[i, "selisih"] >= 3) {
          res_data[i, "stat_SLA"] = "Sesuai SLA"
        } else {
          res_data[i, "stat_SLA"] = "Tidak sesuai SLA"
        }
      } else if (res_data[i, "request"] == "Meals Benefit for Event") {
        res_data[i, "selisih"] = res_data[i, "due_date"] - res_data[i, "est_done"]
        if (res_data[i, "selisih"] >= 1) {
          res_data[i, "stat_SLA"] = "Sesuai SLA"
        } else {
          res_data[i, "stat_SLA"] = "Tidak sesuai SLA"
        }
      } else if (res_data[i, "request"] == "Pembuatan Plakat") {
        res_data[i, "selisih"] = res_data[i, "due_date"] - res_data[i, "est_done"]
        if (res_data[i, "selisih"] >= 8) {
          res_data[i, "stat_SLA"] = "Sesuai SLA"
        } else {
          res_data[i, "stat_SLA"] = "Tidak sesuai SLA"
        }
      } else if (res_data[i, "request"] == "Pembelian Barang" |
                 res_data[i, "request"] == "Venue Request") {
        res_data[i, "selisih"] = res_data[i, "due_date"] - res_data[i, "est_done"]
        if (res_data[i, "selisih"] >= 14) {
          res_data[i, "stat_SLA"] = "Sesuai SLA"
        } else {
          res_data[i, "stat_SLA"] = "Tidak sesuai SLA"
        }
      } else if (res_data[i, "request"] == "Event Tools Support" |
                 res_data[i, "request"] == "Request Print/Jilid") {
        res_data[i, "selisih"] = res_data[i, "due_date"] - res_data[i, "est_done"]
        if (res_data[i, "selisih"] >= 7) {
          res_data[i, "stat_SLA"] = "Sesuai SLA"
        } else {
          res_data[i, "stat_SLA"] = "Tidak sesuai SLA"
        }
      } else {
        res_data[i, "selisih"] = res_data[i, "due_date"] - res_data[i, "est_done"]
        if (res_data[i, "selisih"] >= 5) {
          res_data[i, "stat_SLA"] = "Sesuai SLA"
        } else {
          res_data[i, "stat_SLA"] = "Tidak sesuai SLA"
        }
      }
    }
    
    # mengatur ulang kolom yang akan disimpan di google sheet
    res_data <- res_data %>% select(
      req_date,
      est_done,
      due_date,
      urgency,
      tim,
      user,
      request,
      item,
      qty,
      detail,
      status,
      done_date,
      alasan,
      SLA,
      stat_SLA
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
  
  #  Panel 4: req status  ------------------------------------------------------------------
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
           SLA = as.character(SLA),
           urgency = as.character(urgency),
           stat_SLA = as.character(stat_SLA)) %>% 
    select(tim,user,request,item,detail,qty,req_date,due_date,urgency,est_done,SLA,stat_SLA) %>% 
    rename("Division"= tim,
           "User" = user,
           "Request" = request,
           "Item" = item,
           "Detail" = detail,
           "Quantity" = qty,
           "Request Date" = req_date,
           "Due Date" = due_date,
           "Urgency" = urgency,
           "Estimated Finish Date" = est_done,
           "Status SLA" = stat_SLA)
  list_op <- datatable(list_op) %>% 
    formatStyle(
      columns = 'Urgency',
      target = 'row',
      backgroundColor = styleEqual(c("High", "Medium", "Low"), c("#EE7497","#FBFFB3","#9CE2C7")),
      color = styleEqual(c("High", "Medium", "Low"), c("white","black","black"))
    )
    
  
  # mengambil data request berstatus done
  list_rd <- udata %>% as.data.frame() %>% 
    filter(status %in% "Done") %>% 
    arrange(desc(req_date))%>%
    mutate(done_date = date(done_date),
           est_done = date(est_done)) %>% 
    select(tim,user,request,item,detail,qty,est_done,done_date)%>% 
    rename("Division"= tim,
           "User" = user,
           "Request" = request,
           "Item" = item,
           "Detail" = detail,
           "Quantity" = qty,
           "Done Date" = done_date,
           "Estimated Finish Date" = est_done)
  
  # mengambil data request berstatus Cancel/Blocked
  list_rcb <- udata %>% as.data.frame() %>% 
    filter(status %in% "Cancel" | status %in% "Blocked") %>% 
    arrange(desc(req_date))%>%
    mutate(done_date = date(done_date)) %>% 
    select(tim,user,request,item,detail,qty,status,alasan,done_date)%>% 
    rename("Division"= tim,
           "User" = user,
           "Request" = request,
           "Item" = item,
           "Detail" = detail,
           "Quantity" = qty,
           "Status" = status,
           "Reason" = alasan,
           "Canceled/Blocked Date" = done_date)
  
  list_rcb <- datatable(list_rcb) %>% 
    formatStyle(
      columns = 'Status',
      target = 'row',
      backgroundColor = styleEqual(c("Cancel", "Blocked"), c("#FBFFB3","#EE7497")),
      color = styleEqual(c("Cancel", "Blocked"), c("black","white"))
    )
  # rander tabel data in progres
  output$table_rip <- renderDataTable(list_op, options = list(scrollX = TRUE,pageLength = 10))
  
  # rander tabel data done
  output$table_rd <- renderDataTable(list_rd, options = list(scrollX = TRUE,pageLength = 10))
  
  # rander tabel data cancel/blocked
  output$table_rcb <- renderDataTable(list_rcb, options = list(scrollX = TRUE,pageLength = 10))
  
  
  #  Panel 5: req overview ------------------------------------------------------------------
  # infobox
  output$box_tr <- renderInfoBox({
    list_tr <- udata %>% as.data.frame() %>%
      arrange(desc(req_date)) %>%
      mutate(req_date = date(req_date)) %>%
      filter(req_date >= input$tgls & req_date <= input$tgle)
    
    infoBox(
      title = HTML("Request Submitted"),
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
      filter(req_date >= input$tgls & req_date <= input$tgle)
    
    infoBox(
      title = HTML("Request In Progress"),
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
      filter(req_date >= input$tgls & req_date <= input$tgle)
    
    infoBox(
      title = HTML("Request Done"),
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
      filter(req_date >= input$tgls & req_date <= input$tgle)
    
    infoBox(
      title = HTML("Request Canceled or Blocked"),
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
      filter(req_date >= input$tgls & req_date <= input$tgle) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      mutate(tulisan = glue(
        "Date : {req_date}
        Submitted : {cnt} times
        "
      ))
    
    ggplot(req_count, mapping = aes(x = req_date,y = cnt))+
      geom_line(col = "red")+
      geom_point(col="black", size = 3) +
      scale_x_date(date_breaks = "3 days", date_labels = "%b %d")+
      ylim(0,max(req_count$cnt)+2)+
      geom_text(aes(label = cnt), colour = "black",fontface = "bold", size = 5, vjust = -1)+
      labs(title = "SUBMITTED REQUEST BY TIME",
           x = NULL,
           y = NULL)+
      theme_minimal()+
      theme(legend.position = "none",
            plot.title = element_text(size = 18,color = "#d63d2d", face = "bold", hjust = 0.5),
            #axis.title = element_text(size = 12,color = "#d63d2d", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"))
    
    #ggplotly(plot1,tooltip = "text")
  })
  
  # Bar chart [plot2]
  output$plot2 <- renderPlot({
    #udata <- read_sheet(url)
    req_list2 <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(req_date >= input$tgls & req_date <= input$tgle) %>% 
      group_by(request) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "{request}
        Submitted {cnt} times"
      ))
    
    ggplot(req_list2, mapping = aes(y = reorder(request, cnt),x = cnt))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#FCBF49", high = "#581f18")+
      xlim(0,max(req_list2$cnt)+2)+
      geom_text(aes(label = cnt), hjust = -1, colour = "black",fontface = "bold", size = 5)+
      labs(title = "SUBMITTED REQUEST BY TYPE",
           y = NULL,
           x = NULL)+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(size = 18, color = "#d63d2d", face = "bold", hjust = 0.2),
            #axis.title.x = element_text(size = 12,color = "#d63d2d", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"))
    
    #ggplotly(plot2,tooltip = "text")
  })
  
  # Bar chart [plot3]
  output$plot3 <- renderPlot({
    #udata <- read_sheet(url)
    req_list3 <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(req_date >= input$tgls & req_date <= input$tgle) %>% 
      group_by(tim) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "Division : {tim}
        Total Submitted : {cnt} times
        "
      ) )
    
    ggplot(req_list3, mapping = aes(y = reorder(tim, cnt),x =cnt  ))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#f0a202", high = "#581f18")+
      xlim(0,max(req_list3$cnt)+2)+
      geom_text(aes(label = cnt), hjust = -1, colour = "black",fontface = "bold", size = 5)+
      labs(title = "SUBMITTED REQUEST BY DIVISION",
           y = NULL,
           x = NULL)+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(size = 18, colour = "#d63d2d", face = "bold", hjust = 0.4),
            #axis.title.y = element_text(size = 12,color = "#d63d2d", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"))
    
    #ggplotly(plot3, tooltip = "text")
  })
  
  # bar chart [plot4]
  output$plot4 <- renderPlot({
    udata <- read_sheet(url)
    req_list4 <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(req_date >= input$tgls & req_date <= input$tgle) %>% 
      filter( request == "Merchandise Request") %>% 
      group_by(item) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "{item}
        Submitted {cnt} times"
      ))
    
    ggplot(req_list4, mapping = aes(y = reorder(item, cnt),x = cnt))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#FCBF49", high = "#581f18")+
      xlim(0,max(req_list4$cnt)+2)+
      geom_text(aes(label = cnt), hjust = -1, colour = "black",fontface = "bold", size = 5)+
      labs(title = "SUBMITTED MERCHANDISE REQUEST",
           y = NULL,
           x = NULL)+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(size = 18,colour = "#d63d2d", face = "bold", hjust = 0.5),
            #axis.title.x = element_text(size = 12,color = "#d63d2d", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"))
    
    #ggplotly(plot4,tooltip = "text")
  })
  # PANEL SLA
  
  # Bar chart [plot_urgency]
  output$plot_urgen <- renderPlot({
    #udata <- read_sheet(url)
    req_list3 <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(req_date >= input$tglst & req_date <= input$tglen) %>% 
      group_by(urgency) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "Division : {urgency}
        Total Submitted : {cnt} times
        "
      ) )
    
    ggplot(req_list3, mapping = aes(x = reorder(urgency, -cnt),y =cnt  ))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#f0a202", high = "#581f18")+
      ylim(0,max(req_list3$cnt)+2)+
      geom_text(aes(label = cnt), vjust = -1, colour = "black",fontface = "bold", size = 5)+
      labs(title = "BY URGENCY",
           y = NULL,
           x = NULL)+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(size = 18, colour = "#d63d2d", face = "bold", hjust = 0.4),
            #axis.title.y = element_text(size = 12,color = "#d63d2d", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"))
    
    #ggplotly(plot3, tooltip = "text")
  })
  
  # Bar chart [plot_SLA]
  output$plot_sla <- renderPlot({
    #udata <- read_sheet(url)
    req_list3 <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(req_date >= input$tglst & req_date <= input$tglen) %>% 
      group_by(stat_SLA) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "Division : {stat_SLA}
        Total Submitted : {cnt} times
        "
      ) )
    
    ggplot(req_list3, mapping = aes(x = reorder(stat_SLA, -cnt),y =cnt  ))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#f0a202", high = "#581f18")+
      ylim(0,max(req_list3$cnt)+2)+
      geom_text(aes(label = cnt), vjust = -1, colour = "black",fontface = "bold", size = 5)+
      labs(title = "BY SLA",
           y = NULL,
           x = NULL)+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(size = 18, colour = "#d63d2d", face = "bold", hjust = 0.4),
            #axis.title.y = element_text(size = 12,color = "#d63d2d", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"))
    
    #ggplotly(plot3, tooltip = "text")
  })
  
  # PANEL DIVISI
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
      geom_point(col="black", size=3)+
      scale_x_date(date_breaks = "3 days", date_labels = "%b %d")+
      ylim(0,max(req_count$cnt)+2)+
      geom_text(aes(label = cnt), colour = "black", fontface = "bold", size = 5, vjust = -1)+
      labs(title = "SUBMITTED REQUEST BY TIME",
           x = NULL,
           y = NULL)+
      theme_minimal()+
      theme(legend.position = "none",
            plot.title = element_text(size = 18, color = "#d63d2d", face = "bold", hjust = 0.5),
            #axis.title = element_text(size = 14,color = "#d63d2d", face = "bold"),
            axis.text = element_text(size = 11, face = "bold"))
    
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
    
    pie <- data.frame(status = factor(c("In Progress","Done","Cancel/Blocked"), levels = c("In Progress","Done","Cancel/Blocked") ),
                      cnt = c(length(list_op2[,1]),
                              length(list_rd2[,1]),
                              length(list_rcb2[,1])
                      )) 
    
    ggplot(pie, mapping = aes(y = cnt,x = status))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#FCBF49", high = "#581f18")+
      ylim(0,sum(pie$cnt)+10)+
      geom_text(aes(label = cnt), vjust = -1, colour = "black", fontface="bold", size = 7)+
      labs(title = "REQUESTS STATUS",
           y = NULL,
           x = NULL)+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(size = 18,colour = "#d63d2d", face = "bold", hjust = 0.4),
            #axis.title.y = element_text(size = 14,color = "#d63d2d", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"))
    
    #ggplotly(plot6, tooltip = "text")
  })
  
  # Bar plot [plot7]
  output$plot7 <- renderPlot({
    #udata <- read_sheet(url)
    req_list7 <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(tim %in% input$team1) %>%
      filter(req_date >= input$tgl1[1] & req_date <= input$tgl1[2]) %>% 
      group_by(request) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "{request}
        Submitted {cnt} times"
      ))
    
    ggplot(req_list7, mapping = aes(y = reorder(request, cnt),x = cnt))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#FCBF49", high = "#581f18")+
      xlim(0, max(req_list7$cnt)+10)+
      geom_text(aes(label = cnt), hjust = -1, colour = "black", fontface="bold", size = 5)+
      labs(title = "SUBMITTED REQUEST BY TYPE",
           y = NULL,
           x = NULL)+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(size = 18,colour = "#d63d2d", face = "bold", hjust = 0.4),
            #axis.title.x = element_text(size = 14,color = "#d63d2d", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"))
    
    #ggplotly(plot7,tooltip = "text")
  })
  
  # Bar char [plot 8]
  output$plot8 <- renderPlot({
    #udata <- read_sheet(url)
    req_list8 <- udata %>%
      mutate(req_date = ymd(req_date)) %>% 
      filter(tim %in% input$team1) %>%
      filter(req_date >= input$tgl1[1] & req_date <= input$tgl1[2]) %>% 
      filter( request == "Merchandise Request") %>% 
      group_by(item) %>% 
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      arrange(-cnt) %>% 
      mutate(tulisan = glue(
        "{item}
        Submitted {cnt} times"
      ))
    
    ggplot(req_list8, mapping = aes(y = reorder(item, cnt),x = cnt))+
      geom_col(mapping = aes(fill = cnt))+
      scale_fill_gradient(low = "#FCBF49", high = "#581f18")+
      xlim(0,max(req_list8$cnt)+10)+
      geom_text(aes(label = cnt), hjust = -1, colour = "black", fontface="bold", size = 5)+
      labs(title = "SUBMITTED MERCHANDISE REQUEST",
           y = NULL,
           x = NULL)+
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(size = 18,colour = "#d63d2d", face = "bold", hjust = 0.4),
            #axis.title.x = element_text(size = 12,color = "#d63d2d", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"))
    
    #ggplotly(plot4,tooltip = "text")
  })
  
  
}) # close of shiny server
