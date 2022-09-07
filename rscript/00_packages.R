# --------------------------------
#
#   PACKAGES 
#
# --------------------------------


# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

# --- Elegant way to check for missing packages and install them

# Pacotes utilizados
pacotes <- c("plotly","tidyverse","tidyquant",
             "ggrepel","sjPlot","reshape2","knitr",
             "kableExtra","cabootcrs","FactoMineR", 
             "sqldf", "DBI", "lubridate", "pdftools",
             "dbplyr","RSQLite", "glue", "plyr",
             "here","usethis", "janitor")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

rm(pacotes)

