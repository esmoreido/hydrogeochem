Sys.setlocale("LC_ALL")
library(googlesheets4)
library(lubridate)
gs4_auth(cache = ".secrets", email = "hgc.msu@gmail.com")

# main url ----
url <- 'https://docs.google.com/spreadsheets/d/1P0yhwLmuskqrG0KR0fHpu3VwIyGSyeKIk67WEs-hIx8/'
sn <- sheet_names(url)

# все данные в один список
gs <- lapply(sn, read_sheet, ss = url, na = c('-'))
names(gs) <- sn 
# добавляем колонку с названием станции
gs <- Map(cbind, gs, station = gsub("^[0-9]. ", "", names(gs)))
saveRDS(gs, file = 'google_data.rds')

# main url ----
chem_url <- 'https://docs.google.com/spreadsheets/d/17k9IPX2mfQPlFsUhKmW5NxZnQ-V-pndhvNXn946iQqM'
chem_sn <- sheet_names(chem_url)

# все данные в один список
chem_gs <- lapply("Сводная итог", read_sheet, ss = chem_url, na = c('-'))
saveRDS(chem_gs, file = 'chem_data.rds')

# данные ICP вместе взвесь и раствор
icp_sheets <- sheet_names(chem_url)[2:3]
icp_sheets
icp_gs <- lapply(icp_sheets, read_sheet, ss = chem_url, na = c('-'))
names(icp_gs) <- c('sus', 'dis')
# объединяем разные листы вместе по номеру пробы и дате отбора
icp_gs <- Reduce(function(x, y) left_join(x, y,
                                      by = c('Название пробы' = 'Проба',
                                             'Дата отбора' = 'Дата отбора'),
                                      suffix = c('_sus', '_dis')), icp_gs)

# из мониторинговой таблицы достаем створ и дату по номеру пробы
sample_attr <- lapply(gs, function(x) x[, c('Проба'), drop=F])
sample_attr <- do.call(rbind, sample_attr) 
sample_attr <- sample_attr %>%
  rownames_to_column('Створ отбора') %>%
  rename('Название пробы' = 'Проба') %>%
  mutate(`Створ отбора` = trimws(gsub("^[0-9].|\\..+", "", `Створ отбора`)))
icp_gs <- icp_gs %>%
  left_join(sample_attr, by = c('Название пробы')) %>%
  pivot_longer(cols = !contains(" ")) %>%
  mutate(form = gsub(".+_", "", name),
         elem = gsub("_.+", "", name)) %>%
  select(!name)
colnames(icp_gs) = c('sample_id', 'sample_date', 'mass_g', 'vol_ml', 'turb_mgl',
                     'station_id', 'value', 'form', 'elem')
saveRDS(icp_gs, file = 'icp_data.rds')
