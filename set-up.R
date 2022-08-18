# Pacotes a serem instalados e carregados ---------------------------------

#Pacotes utilizados
pacotes <- c("plotly","tidyverse","tidyquant","ggrepel","sjPlot","reshape2","knitr",
             "kableExtra","cabootcrs","FactoMineR", "sqldf", "DBI",
             "dbplyr","RSQLite","here","usethis")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
