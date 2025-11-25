Sys.setlocale("LC_ALL")
library(ggplot2)
library(tidyverse)
library(googlesheets4)
library(plotly)
library(lubridate)

# график с одной станции
station_graph <- function(dataset, sheet_name){
  # sheet_name <- sn[2]
  # df <- read_sheet(url, sheet = sheet_name)
  df <- dataset[sheet_name][[1]]
  # ggplotly(
  #   df %>%
  #     ggplot(aes(x=`Дата отбора`)) + 
  #     geom_path(aes(y=`t воздуха`, col='T воздуха')) + 
  #     geom_point(aes(y=`t воздуха`, col='T воздуха')) +
  #     geom_line(aes(y=`t воды`, col='T воды')) +
  #     geom_point(aes(y=`t воды`, col='T воды')) +
  #     labs(x='Дата', y='', col='') + 
  #     scale_x_datetime(date_breaks = '1 month', 
  #                      date_labels = '%d.%m.%y') +
  #     theme_light(base_size = 14) +
  #     theme(legend.position = 'none')
  #   )
  print('here')
  # print(head(df))
  plot_ly(df, x = ~`Дата отбора`, y = ~`t воздуха`, name = 'Т воздуха', 
          type = 'scatter', mode = 'lines+markers', 
          line = list(color = 'orange'), marker = list(color='orange')) %>%
    add_trace(y = ~`t воды`, mode = 'lines+markers', name = 'Т воды', 
              line = list(color = 'purple'), marker = list(color='purple')) %>%
    layout(title = "",
           xaxis = list(title = "Дата"),
           yaxis = list (title = ""))
  }
#  таблица с одной станции
station_table <- function(dataset, sheet_name){
  df <- dataset[sheet_name][[1]]
}

# получение из таблицы анализов МГУ списка станций и рендер в дропдаун ----
get_station_list_ui <- function(chem_df) {
  renderUI({
    st_choice <- as.list(unique(chem_df$`Station id`))
    # names(st_choice) <- st_list$name
    pickerInput(
      inputId = 'ui_stations_msu',
      label = 'Станции мониторинга',
      width = '250px',
      choices = st_choice,
      selected = NULL,
      multiple = T,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Отменить",
        `select-all-text` = "Выбрать всё",
        `none-selected-text` = "Выберите..."
      )
    )
  })
}


# получение из таблицы анализов МГУ списка переменных и рендер в дропдаун ----
get_variable_list_ui <- function(chem_df) {
  renderUI({
    st_choice <- as.list(unique(chem_df$var))
    # names(st_choice) <- st_list$name
    pickerInput(
      inputId = 'ui_variables_msu',
      label = 'Показатели',
      width = '250px',
      choices = st_choice,
      selected = NULL,
      multiple = T,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Отменить",
        `select-all-text` = "Выбрать всё",
        `none-selected-text` = "Выберите..."
      )
    )
  })
}

# форматирование даты для dygraphs
jsValueFormatter <- function() {
  f <- "function(ms) {
          var today  = new Date(ms);
          var options = { year: 'numeric', month: 'numeric', day: 'numeric', hour: 'numeric', minute: 'numeric' };
          return(today.toLocaleDateString(\"ru-RU\", options));
   }"
  return(f)
}

# БД пользователей
user_base <- read.csv('user_base.csv', colClasses = c(rep('character', 4)))
