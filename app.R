Sys.setlocale("LC_ALL")
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyauthr)
library(htmltools)
library(DT)
library(ggplot2)
library(tidyverse)
library(googlesheets4)
library(plotly)
library(dygraphs)
library(readxl) 
library(xts)
source('funs.R')
gs4_auth(cache = ".secrets", email = "hgc.msu@gmail.com")

options(shiny.trace = FALSE)
options(shiny.fullstacktrace = FALSE)

stl <- "display:inline-block; vertical-align:top"
btn <- "margin-top:25px"

placeholder_txt <- function() div(
  "Выберите станцию мониторинга и показатель",
  style = "color:#888; padding:20px; font-size:16px; text-align:center;"
)

# --- Нормализатор ICP-таблицы -----------------------------------------------
normalize_icp <- function(raw) {
  raw <- as.data.frame(raw, check.names = FALSE)
  nms <- names(raw)
  dt <- intersect(c("date","Date","Дата","sample_date"), nms)[1]
  st <- intersect(c("station_id","Station ID","station"), nms)[1]
  el <- intersect(c("elem","element","var"), nms)[1]
  fm <- intersect(c("form","Form","форма"), nms)[1]
  if (is.na(dt) || is.na(st) || is.na(el)) {
    return(data.frame(date = as.Date(character()),
                      station_id = character(), elem = character(),
                      form = character(), val = numeric(),
                      stringsAsFactors = FALSE))
  }
  id_cols <- c(dt, st, el, fm)
  if ("val" %in% nms) {
    vl <- "val"
  } else if ("value" %in% nms) {
    vl <- "value"
  } else {
    num_cols <- nms[sapply(raw, is.numeric)]
    vl <- setdiff(num_cols, id_cols)[1]
    if (is.na(vl)) vl <- setdiff(nms, id_cols)[1]
  }
  f_vals <- if (!is.na(fm)) as.character(raw[[fm]]) else rep("—", nrow(raw))
  f_vals[is.na(f_vals) | f_vals == ""] <- "—"
  data.frame(
    date       = as.Date(raw[[dt]]),
    station_id = as.character(raw[[st]]),
    elem       = as.character(raw[[el]]),
    form       = f_vals,
    val        = as.numeric(raw[[vl]]),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
ui <- dashboardPage(skin = 'black', 
                    dashboardHeader(title = "Мониторинг ПТЭиС",
                                    tags$li(class = "dropdown", style = "padding-top: 15px; padding-right: 15px; 
                  padding-bottom: 15px; color: #fff;", 
                                            div(class = "pull-right", 
                                                uiOutput("header_loadData_btn", inline = TRUE),
                                                logoutUI(id = "logout", label = "Выход", icon = NULL, 
                                                         class = "btn-danger", style = "color: white;")))),
                    dashboardSidebar(collapsed = T,
                                     sidebarMenu(
                                       menuItem("Обзор", tabName = "dashboard", icon = icon("map-location-dot")),
                                       menuItem("Мониторинг", tabName = "monitoring", icon = icon("bottle-water")),
                                       menuItem("Анализ", tabName = "analysis", icon = icon("flask"))
                                     )
                    ),
                    dashboardBody(
                      loginUI(id = "login", 
                              title = "Пожалуйста, войдите в систему", 
                              user_title = "Пользователь",
                              pass_title = "Пароль", 
                              login_title = "Вход",
                              error_message = "Неверный логин или пароль!",
                              additional_ui = NULL),
                      tabItems(
                        tabItem("dashboard", uiOutput('dashboard')),
                        tabItem("monitoring", fluidPage(title = 'Мониторинг', uiOutput('post_tabs'))), 
                        tabItem("analysis",
                                title = 'Анализ',
                                tabsetPanel(
                                  id = "analysis_tabs",
                                  tabPanel("Лаборатория МГУ", icon = icon("chart-line"),
                                           uiOutput('analysis')),
                                  tabPanel("ICP-анализ", icon = icon("vial"),
                                           uiOutput('analysis_icp'))
                                )
                        )
                      )
                    )
)

# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  # ==== auth ====
  credentials <- loginServer(
    id = "login", data = user_base,
    user_col = user, pwd_col = password,
    log_out = reactive(logout_init())
  )
  output$header_loadData_btn <- renderUI({
    if (credentials()$user_auth)
      actionButton("loadData", "Обновить данные", class = "btn-info")
  })
  logout_init <- shinyauthr::logoutServer(
    id = "logout", active = reactive(credentials()$user_auth))
  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  user_info <- reactive({ credentials()$info })
  output$welcome <- renderText({
    req(credentials()$user_auth)
    paste("Приветствуем, ","<font color=\"#f3b404\"><b>", user_info()$permissions,
          "</b></font>","!")
  })
  
  # ==== загрузка ====
  showPageSpinner(caption = 'Загрузка данных мониторинга')
  url <- 'https://docs.google.com/spreadsheets/d/1P0yhwLmuskqrG0KR0fHpu3VwIyGSyeKIk67WEs-hIx8/'
  sn <- sheet_names(url)
  f <- read_sheet(url, sheet = sn[9])
  timespan <- max(f$`Дата отбора`) - min(f$`Дата отбора`)
  rm(f)
  
  observeEvent(input$loadData, {
    req(credentials()$user_auth)
    showPageSpinner(caption = 'Обновление данных мониторинга')
    url <- 'https://docs.google.com/spreadsheets/d/1P0yhwLmuskqrG0KR0fHpu3VwIyGSyeKIk67WEs-hIx8/'
    sn <- sheet_names(url)
    gs <- lapply(sn, read_sheet, ss = url, na = c('-'))
    names(gs) <- sn
    saveRDS(gs, file = 'google_data.rds')
    chem_url <- 'https://docs.google.com/spreadsheets/d/17k9IPX2mfQPlFsUhKmW5NxZnQ-V-pndhvNXn946iQqM'
    chem_gs <- lapply("Сводная итог", read_sheet, ss = chem_url, na = c('-'))
    saveRDS(chem_gs, file = 'chem_data.rds')
    hidePageSpinner()
    showNotification("Обновление успешно завершено!", type = "message")
  })
  
  gs <- readRDS('google_data.rds')
  chem_df <- data.frame(readRDS('chem_data.rds'), check.names = F) %>%
    pivot_longer(!c(Date, `Station ID`), names_to = 'var', values_to = 'val')
  
  # ICP
  icp_df <- local({
    raw <- tryCatch(readRDS('icp_data.rds'), error = function(e) NULL)
    if (is.null(raw)) {
      data.frame(date = as.Date(character()), station_id = character(),
                 elem = character(), form = character(), val = numeric(),
                 stringsAsFactors = FALSE)
    } else normalize_icp(raw)
  })
  
  hidePageSpinner()
  
  # ==== dashboard ====
  output$dashboard <- renderUI({
    req(credentials()$user_auth)
    fluidRow(
      valueBoxOutput("numstat"),
      valueBoxOutput("dbsize"),
      valueBoxOutput("staff")
    )
    fluidRow(title = 'Мониторинг')
    fluidRow(
      box(title = "Информация", solidHeader = TRUE, height = 'auto',
          tableOutput('stations')),
      box(title = "Последние обновления", solidHeader = TRUE,
          htmlOutput("news"))
    )
  })
  stat_stat <- data.frame(stat = gsub(".\\..", "", sn), nsample = sapply(gs, nrow))
  output$stations <- renderTable(
    stat_stat,
    colnames(stat_stat) <- c('Станция мониторинга', 'Количество отобранных проб'),
    width = '100%')
  output$news <- renderText({
    req(credentials()$user_auth)
    news <- sapply(gs, function(x) dplyr::arrange(x, desc(`Дата отбора`)) %>% 
                     select(`Дата отбора`, `Время отбора`, `Погодные условия`, 
                            `Состояние реки`) %>% slice_head(n = 1))
    news <- as.data.frame(t(news))
    news$`Дата отбора` <- unlist(news$`Дата отбора`)
    news$name <- gsub(".\\..", "", sn)
    news <- news[order(news$`Дата отбора`, decreasing = T),]
    news[is.na(news)] <- 'Нет данных'
    mutate(news, to_print = paste("<h4>", name, " &mdash; ", 
                                  strftime(`Дата отбора`, "%d.%m.%y"),
                                  "</h4><p style=\"background-color: lightgrey\"><b>Погода:</b> ",
                                  `Погодные условия`, '<br><b>Состояние реки: </b>', 
                                  `Состояние реки`, '</p><hr>')) %>% pull(to_print)
  })
  output$numstat <- renderValueBox({
    req(credentials()$user_auth)
    valueBox(value = length(sn), subtitle = "Количество станций мониторинга",
             icon = icon("map-location-dot"))
  })
  output$dbsize <- renderValueBox({
    valueBox(value = unclass(timespan)[1],
             subtitle = "Продолжительность мониторинга, дней", icon = icon("calendar"))
  })
  output$staff <- renderValueBox({
    valueBox(value = 92, subtitle = "Количество сотрудников", icon = icon("users"))
  })
  tab_names <- reactive({ req(sn); sn })
  output$post_tabs <- renderUI({
    req(credentials()$user_auth)
    tabs <- lapply(tab_names(), function(x)
      tabPanel(title = gsub(".\\..", "", x), 
               renderPlotly(station_graph(gs, x)),
               renderDT(station_table(gs, x), 
                        options = list(scrollX = TRUE,
                                       language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json")))))
    do.call(tabsetPanel, tabs)
  })
  
  # ========== Общие настройки пикеров =====================================
  picker_opts <- list(
    `actions-box` = TRUE,
    `live-search` = TRUE,
    size = 10,
    title = "Выберите...",
    multiple = TRUE
  )
  msu_built <- reactiveVal(FALSE)
  icp_built <- reactiveVal(FALSE)
  
  # Хелпер: добавить серию к dygraph
  add_series <- function(g, name, label, color, pattern) {
    if (is.null(pattern))
      g %>% dySeries(name, label = label, color = color)
    else
      g %>% dySeries(name, label = label, color = color, strokePattern = pattern)
  }
  
  # ===================== ВКЛАДКА 1: Лаборатория МГУ =====================
  output$station_list_msu <- renderUI({
    stations <- sort(unique(chem_df$`Station ID`))
    pickerInput(inputId = "ui_stations_msu", label = "Створ наблюдений:",
                choices = stations, selected = NULL, multiple = TRUE,
                options = picker_opts)
  })
  output$variable_list_msu <- renderUI({
    vars <- sort(unique(chem_df$var))
    pickerInput(inputId = "ui_variables_msu", label = "Показатель:",
                choices = vars, selected = NULL, multiple = TRUE,
                options = picker_opts)
  })
  
  plot_df <- reactive({
    chem_df %>%
      filter(`Station ID` %in% input$ui_stations_msu &
               var %in% input$ui_variables_msu) %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 3)))
  })
  
  output$analysis <- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      title = 'Анализ данных',
      wellPanel(
        div(uiOutput('station_list_msu'), style = stl),
        div(uiOutput('variable_list_msu'), style = stl),
        div(actionButton("plot_graph", "Создать", 
                         icon = icon("chart-line"), style = btn),
            downloadButton('download', "Скачать таблицу", style = btn),
            style = stl)
      ),
      wellPanel(uiOutput('msu_content'))
    )
  })
  
  output$msu_content <- renderUI({
    if (!msu_built()) return(placeholder_txt())
    tagList(
      withSpinner(htmlOutput('plotdata'),       type = 4, color = "#0dc5c1"),
      withSpinner(dataTableOutput("datatable"), type = 4, color = "#0dc5c1")
    )
  })
  
  observeEvent(input$plot_graph, {
    req(credentials()$user_auth)
    msu_built(TRUE)
    output$plotdata <- renderUI({
      withProgress(message = "Построение графиков...", value = 0, {
        df <- plot_df()
        if (is.null(df) || nrow(df) == 0) {
          showNotification("Нет данных для отображения", 
                           type = "warning", duration = 5)
          return(div("Нет данных для отображения",
                     style = "color:red; padding:20px;"))
        }
        plot_list <- lapply(input$ui_variables_msu, function(current_var) {
          var_data <- df %>% filter(var == current_var) %>%
            filter(`Station ID` %in% input$ui_stations_msu)
          if (nrow(var_data) == 0) return(NULL)
          station_data <- lapply(input$ui_stations_msu, function(station) {
            station_df <- var_data %>% filter(`Station ID` == station) %>%
              select(Date, val)
            if (nrow(station_df) > 0) {
              ts <- xts::as.xts(station_df$val, order.by = station_df$Date)
              colnames(ts) <- station
              return(ts)
            }
            NULL
          })
          station_data <- station_data[!sapply(station_data, is.null)]
          if (length(station_data) == 0) return(NULL)
          combined_ts <- do.call(cbind, station_data)
          cols <- if (length(station_data) <= 9)
            RColorBrewer::brewer.pal(max(3, length(station_data)), "Set1")[seq_along(station_data)]
          else colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(station_data))
          incProgress(1 / length(input$ui_variables_msu),
                      detail = paste("Показатель:", current_var))
          dygraph(combined_ts, main = current_var, width = '100%') %>%
            dyRangeSelector() %>%
            dyAxis("x", label = "Дата") %>%
            dyAxis("y", label = current_var) %>%
            dyOptions(colors = cols, drawPoints = TRUE, pointSize = 3,
                      strokeWidth = 2, connectSeparatedPoints = TRUE) %>%
            dyLegend(show = "always", hideOnMouseOut = FALSE,
                     labelsSeparateLines = TRUE) %>%
            dyHighlight(highlightCircleSize = 4,
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = TRUE)
        })
        plot_list <- plot_list[!sapply(plot_list, is.null)]
        if (length(plot_list) == 0) {
          showNotification("Нет данных для построения графиков", 
                           type = "warning", duration = 5)
          return(div("Нет данных для построения графиков",
                     style = "color:red; padding:20px;"))
        }
        htmltools::tagList(plot_list)
      })
    })
    output$datatable <- renderDT(
      plot_df(),
      options = list(pageLength = 100, scrollX = TRUE,
                     language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json")))
  })
  
  output$download <- downloadHandler(
    req(credentials()$user_auth),
    filename = function() "msu_analysis_data.csv",
    content  = function(fname) write.table(plot_df(), fname, sep = ";", quote = F,
                                           row.names = F, na = '-32968')
  )
  
  # ===================== ВКЛАДКА 2: ICP-анализ =========================
  output$station_list_icp <- renderUI({
    stations <- sort(unique(icp_df$station_id))
    pickerInput(inputId = "ui_stations_icp", label = "Створ наблюдений:",
                choices = stations, selected = NULL, multiple = TRUE,
                options = picker_opts)
  })
  output$variable_list_icp <- renderUI({
    vars <- sort(unique(icp_df$elem))
    pickerInput(inputId = "ui_variables_icp", label = "Показатель:",
                choices = vars, selected = NULL, multiple = TRUE,
                options = picker_opts)
  })
  
  plot_df_icp <- reactive({
    icp_df %>%
      filter(station_id %in% input$ui_stations_icp &
               elem       %in% input$ui_variables_icp) %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 3)))
  })
  
  output$analysis_icp <- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      title = 'Анализ данных',
      wellPanel(
        div(uiOutput('station_list_icp'), style = stl),
        div(uiOutput('variable_list_icp'), style = stl),
        div(actionButton("plot_graph_icp", "Создать",
                         icon = icon("chart-line"), style = btn),
            downloadButton('download_icp', "Скачать таблицу", style = btn),
            style = stl)
      ),
      wellPanel(uiOutput('icp_content'))
    )
  })
  
  output$icp_content <- renderUI({
    if (!icp_built()) return(placeholder_txt())
    tagList(
      withSpinner(htmlOutput('plotdata_icp'),       type = 4, color = "#0dc5c1"),
      withSpinner(dataTableOutput("datatable_icp"), type = 4, color = "#0dc5c1")
    )
  })
  
  observeEvent(input$plot_graph_icp, {
    req(credentials()$user_auth)
    icp_built(TRUE)
    
    # Диагностика
    n_elem_file <- length(unique(icp_df$elem))
    n_elem_sel  <- length(input$ui_variables_icp)
    showNotification(
      paste0("Элементов в данных: ", n_elem_file,
             " | выбрано: ", n_elem_sel,
             " | будет построено графиков: ", n_elem_sel),
      type = "message", duration = 6
    )
    
    output$plotdata_icp <- renderUI({
      withProgress(message = "Построение графиков...", value = 0, {
        df <- plot_df_icp()
        if (is.null(df) || nrow(df) == 0) {
          showNotification("Нет данных для отображения",
                           type = "warning", duration = 5)
          return(div("Нет данных для отображения",
                     style = "color:red; padding:20px;"))
        }
        plot_list <- lapply(input$ui_variables_icp, function(current_var) {
          var_data <- df %>% filter(elem == current_var) %>%
            filter(station_id %in% input$ui_stations_icp)
          if (nrow(var_data) == 0) return(NULL)
          
          forms <- sort(unique(var_data$form))
          if (length(forms) == 0) forms <- "—"
          
          series_xts  <- list()
          series_meta <- list()
          for (st in input$ui_stations_icp) {
            for (fm in forms) {
              sub <- var_data %>%
                filter(station_id == st, form == fm) %>%
                arrange(date)
              if (nrow(sub) == 0) next
              i <- length(series_xts) + 1L
              nm_safe <- paste0("s", i)
              label <- if (length(forms) > 1)
                paste0(current_var, " | ", st, " [", fm, "]")
              else
                paste0(current_var, " | ", st)
              ts <- xts::as.xts(sub$val, order.by = sub$date)
              colnames(ts) <- nm_safe
              series_xts[[i]]  <- ts
              series_meta[[i]] <- list(name    = nm_safe,
                                       label   = label,
                                       station = st,
                                       form    = fm)
            }
          }
          if (length(series_xts) == 0) return(NULL)
          combined_ts <- do.call(cbind, series_xts)
          
          stations_present <- unique(sapply(series_meta, `[[`, "station"))
          n_st <- length(stations_present)
          cols <- if (n_st <= 9)
            RColorBrewer::brewer.pal(max(3, n_st), "Set1")[seq_len(n_st)]
          else
            colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_st)
          station_colors <- setNames(cols, stations_present)
          
          patterns <- list(NULL, "dashed", "dotted", "dotdash")
          form_patterns <- setNames(
            patterns[((seq_along(forms) - 1) %% length(patterns)) + 1],
            forms
          )
          
          incProgress(1 / length(input$ui_variables_icp),
                      detail = paste("Элемент:", current_var))
          
          g <- dygraph(combined_ts,
                       main = paste("Элемент:", current_var),
                       width = '100%') %>%
            dyRangeSelector() %>%
            dyAxis("x", label = "Дата") %>%
            dyAxis("y", label = current_var)
          
          for (info in series_meta) {
            g <- add_series(g,
                            name    = info$name,
                            label   = info$label,
                            color   = station_colors[[info$station]],
                            pattern = form_patterns[[info$form]])
          }
          g %>% dyOptions(drawPoints = TRUE, pointSize = 3,
                          strokeWidth = 2, connectSeparatedPoints = TRUE) %>%
            dyLegend(show = "always", hideOnMouseOut = FALSE,
                     labelsSeparateLines = TRUE) %>%
            dyHighlight(highlightCircleSize = 4,
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = TRUE)
        })
        plot_list <- plot_list[!sapply(plot_list, is.null)]
        if (length(plot_list) == 0) {
          showNotification("Нет данных для построения графиков",
                           type = "warning", duration = 5)
          return(div("Нет данных для построения графиков",
                     style = "color:red; padding:20px;"))
        }
        htmltools::tagList(plot_list)
      })
    })
    output$datatable_icp <- renderDT(
      plot_df_icp() %>% select(date, station_id, elem, form, val),
      options = list(pageLength = 100, scrollX = TRUE,
                     language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json")))
  })
  
  output$download_icp <- downloadHandler(
    req(credentials()$user_auth),
    filename = function() "icp_analysis_data.csv",
    content  = function(fname) write.table(
      plot_df_icp() %>% select(date, station_id, elem, form, val),
      fname, sep = ";", quote = F, row.names = F, na = 'NA')
  )
}

shinyApp(ui = ui, server = server)