Sys.setlocale("LC_ALL")
library(ggplot2)
library(tidyverse)
library(googlesheets4)
library(plotly)
library(lubridate)
library(readxl)
# authenticate once with email ----
# options(gargle_oauth_cache = ".secrets")
# Authenticate manually
# gs4_auth()
# If successful, the previous step stores a token file.
# Check that a file has been created with:
# list.files(".secrets/")
# Check that the non-interactive authentication works by first deauthorizing:
# gs4_deauth()
# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "hgc.msu@gmail.com")

# main url ----
url <- 'https://docs.google.com/spreadsheets/d/1P0yhwLmuskqrG0KR0fHpu3VwIyGSyeKIk67WEs-hIx8/'
sn <- sheet_names(url)

# все данные в один список
gs <- lapply(sn, read_sheet, ss = url, na = c('-'))
names(gs) <- sn
saveRDS(gs, file = 'google_data.rds')
gs <- readRDS('google_data.rds')
# статистика по наполнению
stat_stat <- data.frame(stat = gsub(".\\..", "", sn), nsample = sapply(gs, nrow))

# последние записи
news <- sapply(gs, function(x) dplyr::arrange(x, desc(`Дата отбора`)) %>% 
                 select(`Дата отбора`, `Погодные условия`, `Состояние реки`) %>%
                 slice_head(n = 1))
news <- as.data.frame(t(news))
news$`Дата отбора` <- unlist(news$`Дата отбора`)
news$name <- gsub(".\\..", "", sn)
news <- news[order(news$`Дата отбора`, decreasing = T),]
news[is.na(news)] <- 'Нет данных'

timespan <- max(f$`Дата отбора`) - min(f$`Дата отбора`)
unclass(timespan)[1]
# Верхние картинки ----
# график с одной станции
station_graph <- function(sheet_name){
  # sheet_name <- sn[2]
  # df <- read_sheet(url, sheet = sheet_name)
  df <- gs[sheet_name][[1]]
  ggplotly(
    df %>%
    ggplot(aes(x=`Дата отбора`)) + 
    geom_path(aes(y=`t воздуха`, col='T воздуха')) + 
    geom_point(aes(y=`t воздуха`, col='T воздуха')) +
    # geom_line(aes(y=`t воды`, col='T воды')) + 
    # geom_point(aes(y=`t воды`, col='T воды')) + 
    labs(x='Дата', y='', col='') + 
    scale_x_datetime(date_labels = '%d.%m.%y') +
    theme_light(base_size = 20) +
    theme(legend.position = 'none')
    )
}

station_graph(sn[1])

# карта с расположением точек отбора

# файл из нашей лаборатории
chem_df <- read_xlsx('Мониторинг-лаборатория.xlsx')
chem_df <- chem_df %>%
  pivot_longer(!c(Date, `Station id`), names_to = 'var', values_to = 'val')

# график
ts <- as.xts(chem_df[,-1], order.by = chem_df$Date)
dygraph(ts, main = colnames(ts), 
                                       group = 'plots', 
                                       width = 'auto', 
                                       height = 200) %>% 
                dyRangeSelector() %>%
                dyAxis("x", label = "Дата") |>
                dyAxis("y", drawGrid = FALSE)


# объединение мониторинга и анализа