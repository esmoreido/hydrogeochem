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
saveRDS(gs, file = 'google_data.rds')
