# Sys.setlocale("LC_ALL")
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
library(here)
source(here::here('funs.R'))
gs4_auth(cache = here::here(".secrets"), email = "hgc.msu@gmail.com")

options(shiny.trace = FALSE)
options(shiny.fullstacktrace = FALSE)

stl <- "display:inline-block; vertical-align:top"
btn <- "margin-top:25px"
ui <- shinydashboard::dashboardPage(
  skin = 'black',
  shinydashboard::dashboardHeader(
    title = "Мониторинг ПТЭиС",
    # кнопка выхода пользователя
    tags$li(
      class = "dropdown",
      style = "padding-top: 15px; padding-right: 15px; 
                                  padding-bottom: 15px; color: #fff;",
      htmltools::div(
        class = "pull-right",
        shinyauthr::logoutUI(
          id = "logout",
          label = "Log out",
          icon = NULL,
          class = "btn-danger",
          style = "color: white;"
        )
      )
    )
  ),
  shinydashboard::dashboardSidebar(
    collapsed = T,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        "Обзор",
        tabName = "dashboard",
        icon = shiny::icon("map-location-dot")
      ), # fontawesome.com
      shinydashboard::menuItem(
        "Мониторинг",
        tabName = "monitoring",
        icon = shiny::icon("bottle-water")
      ),
      shinydashboard::menuItem(
        "Анализ",
        tabName = "analysis",
        icon = shiny::icon("flask")
      )
    )
  ),
  shinydashboard::dashboardBody(
    # панель аутентификации
    shinyauthr::loginUI(
      id = "login",
      title = "Пожалуйста, войдите в систему",
      user_title = "Пользователь",
      pass_title = "Пароль",
      login_title = "Вход",
      error_message = "Неверный логин или пароль!",
      additional_ui = NULL
    ),
    shinydashboard::tabItems(
      shinydashboard::tabItem("dashboard", shiny::uiOutput('dashboard')),
      shinydashboard::tabItem(
        "monitoring",
        shiny::fluidPage(
          title = 'Мониторинг',
          shiny::uiOutput('post_tabs')
        )
      ),
      shinydashboard::tabItem(
        "analysis",
        title = 'Анализ',
        shiny::uiOutput('analysis')
      )
    )
  )
)


# server ----
server <- function(input, output, session) {
  # логин пользователя ----
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = shiny::reactive(logout_init())
  )
  # кнопка выхода ----
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = shiny::reactive(credentials()$user_auth)
  )

  # разворачивание меню при входе пользователя ----
  shiny::observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })

  output$user_table <- shiny::renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    shiny::req(credentials()$user_auth)
    credentials()$info
  })

  # приветствие пользователя
  user_info <- shiny::reactive({
    credentials()$info
  })

  output$welcome <- shiny::renderText({
    shiny::req(credentials()$user_auth)
    paste(
      "Приветствуем, ",
      "<font color=\"#f3b404\"><b>",
      {
        user_info()$permissions
      },
      "</b></font>",
      "!"
    )
  })

  shinycssloaders::showPageSpinner(caption = 'Загрузка данных мониторинга')
  url <- 'https://docs.google.com/spreadsheets/d/1P0yhwLmuskqrG0KR0fHpu3VwIyGSyeKIk67WEs-hIx8/'
  sn <- googlesheets4::sheet_names(url)
  f <- googlesheets4::read_sheet(url, sheet = sn[9])
  timespan <- max(f$`Дата отбора`) - min(f$`Дата отбора`)
  rm(f)

  # загрузка локальных данных ----
  # мониторинг
  gs <- readRDS('google_data.rds')
  # лаборатория МГУ
  chem_df <- readxl::read_xlsx('Мониторинг-лаборатория.xlsx') %>%
    tidyr::pivot_longer(
      !c(Date, `Station id`),
      names_to = 'var',
      values_to = 'val'
    )
  shinycssloaders::hidePageSpinner()

  # UI дашборда
  output$dashboard <- shiny::renderUI({
    shiny::req(credentials()$user_auth)
    shiny::fluidRow(
      shinydashboard::valueBoxOutput("numstat"),
      shinydashboard::valueBoxOutput("dbsize"),
      shinydashboard::valueBoxOutput("staff")
    )
    shiny::fluidRow(
      title = 'Мониторинг'
    )
    shiny::fluidRow(
      shinydashboard::box(
        title = "Информация",
        solidHeader = TRUE,
        height = 'auto',
        shiny::tableOutput('stations')
      ),
      shinydashboard::box(
        title = "Последние обновления",
        solidHeader = TRUE,
        shiny::htmlOutput("news")
      )
    )
  })

  # Таблица станций ----
  stat_stat <- data.frame(
    stat = gsub(".\\..", "", sn),
    nsample = sapply(gs, nrow)
  )
  output$stations <- shiny::renderTable(
    stat_stat,
    colnames(stat_stat) <- c(
      'Станция мониторинга',
      'Количество отобранных проб'
    ),
    width = '100%'
  )

  # Новости ----
  output$news <- shiny::renderText({
    shiny::req(credentials()$user_auth)
    news <- sapply(gs, function(x) {
      dplyr::arrange(x, dplyr::desc(`Дата отбора`)) %>%
        plotly::select(
          `Дата отбора`,
          `Время отбора`,
          `Погодные условия`,
          `Состояние реки`
        ) %>%
        dplyr::slice_head(n = 1)
    })
    news <- as.data.frame(t(news))
    news$`Дата отбора` <- unlist(news$`Дата отбора`)
    news$name <- gsub(".\\..", "", sn)
    news <- news[order(news$`Дата отбора`, decreasing = T), ]
    news[is.na(news)] <- 'Нет данных'
    plotly::mutate(
      news,
      to_print = paste(
        "<h4>",
        name,
        " &mdash; ",
        strftime(`Дата отбора`, "%d.%m.%y"),
        "</h4><p style=\"background-color: lightgrey\"><b>Погода:</b> ",
        `Погодные условия`,
        '<br><b>Состояние реки: </b>',
        `Состояние реки`,
        '</p><hr>'
      )
    ) %>%
      dplyr::pull(to_print)
  })

  # Верхние картинки ----
  output$numstat <- shinydashboard::renderValueBox({
    shiny::req(credentials()$user_auth)
    shinydashboard::valueBox(
      value = length(sn),
      subtitle = "Количество станций мониторинга",
      icon = shiny::icon("map-location-dot"),
    )
  })

  output$dbsize <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = unclass(timespan)[1],
      subtitle = "Продолжительность мониторинга, дней",
      icon = shiny::icon("calendar")
    )
  })

  output$staff <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = 92,
      subtitle = "Количество сотрудников",
      icon = shiny::icon("users")
    )
  })

  tab_names <- shiny::reactive({
    shiny::req(sn)
    return(sn)
  })

  # Вкладки с графиками мониторинга ----
  output$post_tabs <- shiny::renderUI({
    shiny::req(credentials()$user_auth)
    tabs <- lapply(tab_names(), function(x) {
      shiny::tabPanel(
        title = gsub(".\\..", "", x),
        plotly::renderPlotly(station_graph(gs, x)),
        DT::renderDT(
          station_table(gs, x),
          options = list(
            scrollX = TRUE,
            language = list(
              url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"
            )
          )
        )
      )
    })
    # print(tabs)
    do.call(tabsetPanel, tabs)
  })

  # Страница анализа лаборатории МГУ ----
  output$station_list_msu <- get_station_list_ui(chem_df)
  output$variable_list_msu <- get_variable_list_ui(chem_df)

  # данные для графика
  plot_df <- shiny::reactive({
    shiny::req(input$ui_stations_msu, input$ui_variables_msu)
    df <- chem_df %>%
      plotly::filter(
        `Station id` %in%
          input$ui_stations_msu &
          var %in% input$ui_variables_msu
      ) %>%
      tidyr::pivot_wider(
        id_cols = Date,
        names_from = c('Station id', 'var'),
        names_sep = ' : ',
        values_from = 'val'
      ) %>%
      plotly::mutate(dplyr::across(dplyr::where(is.numeric), round, 3))
  })

  # График анализа лаборатории МГУ ----
  output$analysis <- shiny::renderUI({
    shiny::req(credentials()$user_auth)
    shiny::fluidPage(
      title = 'Анализ',
      shiny::wellPanel(
        htmltools::div(shiny::uiOutput('station_list_msu'), style = stl),
        htmltools::div(shiny::uiOutput('variable_list_msu'), style = stl),
        htmltools::div(
          shiny::actionButton(
            "plot_graph",
            "Создать",
            icon = shiny::icon("chart-line"),
            style = btn
          ),
          shiny::downloadButton('download', "Скачать таблицу", style = btn),
          style = stl
        )
      ),
      shiny::wellPanel(
        htmltools::div(
          style = 'overflow: scroll',
          shiny::htmlOutput('plotdata')
        ),
        htmltools::div(
          DT::dataTableOutput("datatable"),
          style = "font-size:80%; overflow-y:scroll; max-height: 600px"
        )
      )
    )
  })

  shiny::observeEvent(input$plot_graph, {
    shiny::req(credentials()$user_auth)
    output$plotdata <- shiny::renderUI({
      shiny::withProgress(
        expr = {
          ts <- xts::as.xts(plot_df()[, -1], order.by = plot_df()$Date)
          lst <- lapply(ts, function(x) {
            dygraphs::dygraph(
              x,
              main = colnames(x),
              group = 'plots',
              width = 'auto',
              height = 200
            ) %>%
              dygraphs::dyRangeSelector() %>%
              dygraphs::dyAxis(
                "x",
                label = "Дата",
                valueFormatter = jsValueFormatter(),
                rangePad = 5
              ) %>%
              dygraphs::dyAxis("y", drawGrid = FALSE) %>%
              dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
          })
          # print(lst)
          res <- htmltools::tagList(lst)
          return(res)
        },
        message = "Загрузка..."
      )
    })
    # Вывод ----
    output$datatable <- DT::renderDT(
      plot_df(),
      options = list(
        pageLength = 100,
        scrollX = TRUE,
        language = list(
          url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"
        )
      )
    )
  })
  # Файл для скачивания ----
  output$download <- shiny::downloadHandler(
    shiny::req(credentials()$user_auth),
    filename = function() {
      "msu_analysis_data.csv"
    },
    content = function(fname) {
      utils::write.table(
        plot_df(),
        fname,
        sep = ";",
        quote = F,
        row.names = F,
        na = '-32968'
      )
    }
  )
}

# Run the application
shiny::shinyApp(
  ui = ui,
  server = server,
  options = list("port" = 8180, "host" = "0.0.0.0")
)
