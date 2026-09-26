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

# main url ----
chem_url <- 'https://docs.google.com/spreadsheets/d/17k9IPX2mfQPlFsUhKmW5NxZnQ-V-pndhvNXn946iQqM'
chem_sn <- sheet_names(chem_url)

# все данные в один список
chem_gs <- lapply("Сводная итог", read_sheet, ss = chem_url, na = c('-'))
saveRDS(chem_gs, file = 'chem_data.rds')
