# --------------------------------
#
#   IMPORTING DATA 
#
# --------------------------------

# loading packages script
source(here::here("rscript", "00_packages.R"))



urls <- c(
  "https://www.anbima.com.br/data/files/04/E4/3C/AB/CF88B710C83266B7882BA2A8/ed01.zip",
  "https://www.anbima.com.br/data/files/03/91/F0/6E/1F88B710C83266B7882BA2A8/ed02.zip",
  "https://www.anbima.com.br/data/files/86/D1/30/40/3E88B710C83266B7882BA2A8/ed03.zip",
  "https://www.anbima.com.br/data/files/08/06/2F/AF/BD88B710C83266B7882BA2A8/ed04.zip",
  "https://www.anbima.com.br/data/files/7D/07/2A/8D/48AB0810B5890B086B2BA2A8/Raio%20X%20dados%20brutos.zip.zip"
)
  

# # Creates a data frame of the URLs Addresses
# urlx <- 
#   tidyr::expand_grid(urls)

edicoes <- 
  c(01, 02, 03, 04, 05)

# Creates Names for the PDF Files 
zipfiles_names <- 
  tidyr::expand_grid(edicoes) %>%
  glue_data(here::here("data", "raw_data", "anbima_raiox_{edicoes}.zip"))

safe_download <- safely(~ download.file(.x , .y, mode = "wb"))


walk2(urls, zipfiles_names , safe_download)


# --
# --------------------------------------------------------------------------


# --

# https://stackoverflow.com/questions/41954183/how-can-i-extract-multiple-zip-files-and-read-those-csvs-in-r/41954523


# get all the zip files
zipF <- zipfiles_names


unzip(zipfile = zipF[1], exdir = here::here("data", "raw_data", "unziped"))
unzip(zipfile = zipF[2], exdir = here::here("data", "raw_data", "unziped"))
unzip(zipfile = zipF[3], exdir = here::here("data", "raw_data", "unziped"))
unzip(zipfile = zipF[4], exdir = here::here("data", "raw_data", "unziped"))
unzip(zipfile = zipF[5], exdir = here::here("data", "raw_data", "unziped"))



# https://theautomatic.net/2018/07/11/manipulate-files-r/
# With unlink, we can delete the selected files 
# we created above with file.create — also 

sapply(
  paste0(
    here::here("data", "raw_data", "unziped"),"/", 
              "Raio_X_5a_edicao_população", ".pdf"), unlink)



# # get the csv files
# xlsx_files <- list.files(
#   path = here::here("data", "raw_data", "unziped"), 
#   pattern = "*.xlsx")



# --

# # read the csv files
# my_data <- 
#   ldply(.data = paste0(
#     here::here("data", "raw_data", "unziped"),"/", xlsx_files), 
#                  .fun = readxl::read_excel)


# read the csv files and selec the features
# Classe, UF, P20, P21, P22, P1a, P14 e P15

library(readxl)
df01 <- read_excel("data/raw_data/unziped/Edicao_01_RaioX - Base de Dados.xlsx", 
                   sheet = "PM4622_LABELS", col_names = FALSE) %>% 
  janitor::row_to_names( 1, remove_rows_above = FALSE) %>% 
  as_tibble(.name_repair = janitor::make_clean_names) %>% 
  select(
    rclasse2, uf, 17:24, 136:150, 173:175 
  )




df02 <- read_excel("data/raw_data/unziped/Edicao_02_RaioX - Base de Dados.xlsx", 
                   sheet = "LABELS", col_names = FALSE) %>% 
  janitor::row_to_names( 1, remove_rows_above = FALSE) %>% 
  as_tibble(.name_repair = janitor::make_clean_names)




