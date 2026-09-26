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

options(shiny.trace=FALSE)
options(shiny.fullstacktrace=FALSE)

stl <- "display:inline-block; vertical-align:top"
btn <- "margin-top:25px"
ui <- dashboardPage(skin = 'black', 
                    dashboardHeader(title = "Мониторинг ПТЭиС",
                                    # кнопка обновления
                                    
                                    # кнопка выхода пользователя
                                    tags$li(class = "dropdown", style = "padding-top: 15px; padding-right: 15px; 
                                  padding-bottom: 15px; color: #fff;", 
                                            div(class = "pull-right", 
                                                uiOutput("header_loadData_btn", 
                                                         inline = TRUE),
                                                logoutUI(id = "logout", 
                                                         label = "Выход", 
                                                         icon = NULL, 
                                                         class = "btn-danger",
                                                         style = "color: white;")))),
                    dashboardSidebar(collapsed = T,
                      sidebarMenu(
                        menuItem("Обзор", tabName = "dashboard", icon = icon("map-location-dot")), # fontawesome.com
                        menuItem("Мониторинг", tabName = "monitoring", icon = icon("bottle-water")),
                        menuItem("Анализ", tabName = "analysis", icon = icon("flask"))
                      )
                    ),
                    dashboardBody(
                      # панель аутентификации
                      loginUI(id = "login", 
                                          title = "Пожалуйста, войдите в систему", 
                                          user_title = "Пользователь",
                                          pass_title = "Пароль", 
                                          login_title = "Вход",
                                          error_message = "Неверный логин или пароль!",
                                          additional_ui = NULL),
                      tabItems(
                        tabItem("dashboard", 
                                uiOutput('dashboard')
                        ),
                        tabItem("monitoring", 
                                fluidPage(
                                  title = 'Мониторинг',
                                  uiOutput('post_tabs')
                                )
                                
                        ), 
                        tabItem("analysis", 
                                        title = 'Анализ',
                                uiOutput('analysis')
                                )
                        )
                    )
)
                             

# server ----
server <- function(input, output, session) {
  # логин пользователя ----
  credentials <- loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  # кнопка обновления ----
  output$header_loadData_btn <- renderUI({
    if(credentials()$user_auth){
      actionButton("loadData", "Обновить данные", class = "btn-info")
    }
  })
  # кнопка выхода ----
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # разворачивание меню при входе пользователя ----
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    credentials()$info
  })
  
  # приветствие пользователя
  user_info <- reactive({credentials()$info})
  
  output$welcome <- renderText({ 
    req(credentials()$user_auth)
    paste("Приветствуем, ","<font color=\"#f3b404\"><b>", {user_info()$permissions}, "</b></font>","!") 
  })
  # предварительная загрузка данных из гугл таблиц ----
  showPageSpinner(caption = 'Загрузка данных мониторинга')
  url <- 'https://docs.google.com/spreadsheets/d/1P0yhwLmuskqrG0KR0fHpu3VwIyGSyeKIk67WEs-hIx8/'
  sn <- sheet_names(url)
  f <- read_sheet(url, sheet = sn[9])
  timespan <- max(f$`Дата отбора`) - min(f$`Дата отбора`)
  rm(f)
  
  # загрузка обновления из гугл таблиц ----
  observeEvent(input$loadData, {
    req(credentials()$user_auth)
    showPageSpinner(caption = 'Обновление данных мониторинга')
    # main url
    url <- 'https://docs.google.com/spreadsheets/d/1P0yhwLmuskqrG0KR0fHpu3VwIyGSyeKIk67WEs-hIx8/'
    sn <- sheet_names(url)
    
    # все данные в один список
    gs <- lapply(sn, read_sheet, ss = url, na = c('-'))
    names(gs) <- sn
    # saveRDS(gs, file = 'google_data.rds')
    
    # main url
    chem_url <- 'https://docs.google.com/spreadsheets/d/17k9IPX2mfQPlFsUhKmW5NxZnQ-V-pndhvNXn946iQqM'
    chem_sn <- sheet_names(chem_url)
    
    # все данные в один список
    chem_gs <- lapply("Сводная итог", read_sheet, ss = chem_url, na = c('-'))
    saveRDS(chem_gs, file = 'chem_data.rds')
    hidePageSpinner()
    showNotification("Обновление успешно завершено!", type = "message")
  })
  
  # загрузка локальных данных ----
  # мониторинг 
  gs <- readRDS('google_data.rds')
  # лаборатория МГУ
  # chem_df <- read_xlsx('Мониторинг-лаборатория.xlsx') %>%
  chem_df <- data.frame(readRDS('chem_data.rds'), check.names = F) %>%
    pivot_longer(!c(Date, `Station ID`), names_to = 'var', values_to = 'val')
  hidePageSpinner()
  
  # UI дашборда
  output$dashboard <- renderUI({
    req(credentials()$user_auth)
    fluidRow(
                                  valueBoxOutput("numstat"),
                                  valueBoxOutput("dbsize"),
                                  valueBoxOutput("staff")
                                )
                                fluidRow(
                                  title = 'Мониторинг'
                                )
                                fluidRow(
                                  box(title = "Информация", 
                                      solidHeader = TRUE, height = 'auto',
                                      tableOutput('stations')
                                  ),
                                  box(title = "Последние обновления", solidHeader = TRUE,
                                    htmlOutput("news")
                                  )
                                )
  })
  
  
  # Таблица станций ----
  stat_stat <- data.frame(stat = gsub(".\\..", "", sn), 
                          nsample = sapply(gs, nrow))
  output$stations <- renderTable(
    stat_stat,
    colnames(stat_stat) <- c('Станция мониторинга', 
                             'Количество отобранных проб'),
    width = '100%')
  
  # Новости ----
  output$news <- renderText({
    req(credentials()$user_auth)
    news <- sapply(gs, function(x) dplyr::arrange(x, desc(`Дата отбора`)) %>% 
                     select(`Дата отбора`, `Время отбора`, `Погодные условия`, 
                            `Состояние реки`) %>%
                     slice_head(n = 1))
    news <- as.data.frame(t(news))
    news$`Дата отбора` <- unlist(news$`Дата отбора`)
    news$name <- gsub(".\\..", "", sn)
    news <- news[order(news$`Дата отбора`, decreasing = T),]
    news[is.na(news)] <- 'Нет данных'
    mutate(news,
           to_print = paste("<h4>", name, " &mdash; ", 
                            strftime(`Дата отбора`, "%d.%m.%y"),
                            "</h4><p style=\"background-color: lightgrey\"><b>Погода:</b> ",`Погодные условия`, 
                            '<br><b>Состояние реки: </b>', 
                            `Состояние реки`, '</p><hr>')
    ) %>% pull(to_print)
    
  })
  
  # Верхние картинки ----
  output$numstat <- renderValueBox({
    req(credentials()$user_auth)
    valueBox(
      value = length(sn),
      subtitle = "Количество станций мониторинга",
      icon = icon("map-location-dot"),
    )
  })
  
  output$dbsize <- renderValueBox({
    valueBox(
      value = unclass(timespan)[1],
      subtitle = "Продолжительность мониторинга, дней",
      icon = icon("calendar")
    )
  })
  
  output$staff <- renderValueBox({
    valueBox(
      value = 92,
      subtitle = "Количество сотрудников",
      icon = icon("users")
    )
  })
  
  tab_names <- reactive({
    req(sn)
    return(sn)
  })
  
  # Вкладки с графиками мониторинга ----
  output$post_tabs <- renderUI({
    req(credentials()$user_auth)
    tabs <- lapply(tab_names(), function(x)
          tabPanel(title = gsub(".\\..", "", x), 
                   renderPlotly(station_graph(gs, x)),
                   renderDT(station_table(gs, x), 
                                   options = list(scrollX = TRUE,
                                   language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json")))))
    # print(tabs)
    do.call(tabsetPanel, tabs)
    
  })
  
  # Страница анализа лаборатории МГУ ----
  output$station_list_msu <- get_station_list_ui(chem_df)
  output$variable_list_msu <- get_variable_list_ui(chem_df)
  
  # данные для графика
  plot_df <- reactive({
    req(input$ui_stations_msu, input$ui_variables_msu)
    df <- chem_df %>%
      filter(`Station ID` %in% input$ui_stations_msu & 
               var %in% input$ui_variables_msu) %>%
      pivot_wider(id_cols = Date,  
                  names_from = c('Station ID', 'var'), names_sep = ' : ', 
                  values_from = 'val') %>%
      mutate(across(where(is.numeric), round, 3))
  })
  
  # График анализа лаборатории МГУ ----
  output$analysis <- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      title = 'Анализ',
      wellPanel(
        div(uiOutput('station_list_msu'), style = stl),
        div(uiOutput('variable_list_msu'), style = stl),
        div(actionButton("plot_graph", "Создать", 
                         icon = icon("chart-line"), style = btn),
            downloadButton('download',"Скачать таблицу", style = btn),
            style = stl)
      ),
      wellPanel(
        div(style='overflow: scroll', 
            htmlOutput('plotdata')),
        div(dataTableOutput("datatable"), style = "font-size:80%; overflow-y:scroll; max-height: 600px")
      )
    )
  })

  observeEvent(input$plot_graph, {
    req(credentials()$user_auth)
    # график старая версия ---- 
    output$plotdata <- renderUI({
      withProgress(expr = {
        ts <- as.xts(plot_df()[,-1],
                     order.by = plot_df()$Date)
        lst <- lapply(ts, function (x) dygraph(x, main = colnames(x),
                                               group = 'plots',
                                               width = 'auto',
                                               height = 200) %>%
                        dyRangeSelector() %>%
                        dyAxis("x", label = "Дата",
                               valueFormatter = jsValueFormatter(),
                               rangePad=5) |>
                        dyAxis("y", drawGrid = FALSE) %>%
                        dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")))
        # print(lst)
        res <- htmltools::tagList(lst)
        return(res)
      }, message = "Загрузка...")
    })
    
    
    
    # график новая версия ----
    # output$plotdata <- renderUI({
    #   withProgress(message = "Построение графиков...", value = 0, {
    #     ts <- as.xts(plot_df()[,-1], 
    #                  order.by = plot_df()$Date)
    #     if(is.null(ts) || nrow(ts) == 0) {
    #       return(div("Нет данных для отображения", style = "color:red; padding:20px;"))
    #     }
    #     
    #     # Создаем отдельный график для каждого показателя
    #     plot_list <- lapply(input$pick_var, function(current_var) {
    #       # Фильтруем данные по текущему показателю
    #       var_data <- df %>% 
    #         filter(value_type == current_var) %>%
    #         filter(name_station %in% input$ui_stations)
    #       
    #       if(nrow(var_data) == 0) return(NULL)
    #       
    #       # Создаем временной ряд для каждой станции
    #       station_data <- lapply(input$ui_stations, function(station) {
    #         station_df <- var_data %>% 
    #           filter(name_station == station) %>%
    #           select(datetime, value)
    #         
    #         if(nrow(station_df) > 0) {
    #           ts <- xts::as.xts(station_df$value, order.by = station_df$datetime)
    #           colnames(ts) <- station
    #           return(ts)
    #         }
    #         return(NULL)
    #       })
    #       
    #       # Удаляем NULL элементы
    #       station_data <- station_data[!sapply(station_data, is.null)]
    #       
    #       if(length(station_data) == 0) return(NULL)
    #       
    #       # Объединяем все станции для текущего показателя
    #       combined_ts <- do.call(cbind, station_data)
    #       
    #       # Генерируем уникальные цвета для станций
    #       colors <- RColorBrewer::brewer.pal(max(3, length(station_data)), "Set1")[1:length(station_data)]
    #       
    #       incProgress(1/length(input$pick_var), 
    #                   detail = paste("Показатель:", current_var))
    #       
    #       # Строим график
    #       dygraph(combined_ts, main = current_var, width = '100%') %>%
    #         dyRangeSelector() %>%
    #         dyAxis("x", label = "Дата") %>%
    #         dyAxis("y", label = current_var) %>%
    #         dyOptions(
    #           colors = colors,
    #           drawPoints = TRUE,
    #           pointSize = 3,
    #           strokeWidth = 2,
    #           connectSeparatedPoints = TRUE
    #         ) %>%
    #         dyLegend(
    #           show = "always",
    #           hideOnMouseOut = FALSE,
    #           labelsSeparateLines = TRUE
    #         ) %>%
    #         dyHighlight(
    #           highlightCircleSize = 4,
    #           highlightSeriesBackgroundAlpha = 0.2,
    #           hideOnMouseOut = TRUE
    #         )
    #     })
    #     
    #     # Удаляем NULL элементы
    #     plot_list <- plot_list[!sapply(plot_list, is.null)]
    #     
    #     if(length(plot_list) == 0) {
    #       return(div("Нет данных для построения графиков", style = "color:red; padding:20px;"))
    #     }
    #     
    #     htmltools::tagList(plot_list)
    #   })
    # })
    
    # Вывод ----
    output$datatable <- renderDT(
      plot_df(),
      options = list(pageLength = 100,
                     scrollX = TRUE,
                     language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
    )
  })
  
  
  # Файл для скачивания ----
  output$download <- downloadHandler(
    req(credentials()$user_auth),
    filename = function(){"msu_analysis_data.csv"}, 
    content = function(fname){
      write.table(plot_df(), 
                  fname, sep = ";", quote = F, row.names = F, na = '-32968')
    }
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
