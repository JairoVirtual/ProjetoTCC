###################################################################
#                                                                 #
#     Aluno: Jairo Rodrigues da Silva       Turma: DSA211         #
#                                                                 #
# Descrição: Este script prepara os dados com filtros e           #
#            tratando de variáveis / observações para o           #
#            uso da técnica Análise de Correspondência            #
#            (ANACOR).                                            #
#                                                                 #
# Arquivos: Extraidos da base original, link abaixo, e criados    #
#           os arquivos ClasseProduto19.RData e o                 #
#           ClasseProduto21.RData.                                #
#                                                                 #
###################################################################
#####    Extrair as bases Classe x Produto 2019 e 2021    #########
###################################################################

# Instalação de pacotes, caso não tenha
#install.packages("tidyverse")
#install.packages(readxl)

# Carregar pacotes já instalados para o tratamento.
library("tidyverse")
library(readxl)

# Links das urls para obtenção dos dados originais
# https://www.anbima.com.br/data/files/03/91/F0/6E/1F88B710C83266B7882BA2A8/ed02.zip
# https://www.anbima.com.br/data/files/08/06/2F/AF/BD88B710C83266B7882BA2A8/ed04.zip

# esses dados originais estão na pasta dataAnbima deste script
# é necessário DESCOMPACTAR os para encontrar o arquivo abaixo.


#### ESSA ETAPA PREPARA DOS DOIS ARQUIVOS NA SEQUÊNCIA, DEPOIS TERÁ ETAPA POR ARQUIVO 
# Carregar base original armazenada no projeto (já extraídos do arquivo .zip)
db19 <- read_excel("DataAnbima/ed02/Edicao_02_RaioX - Base de Dados.xlsx", sheet = "LABELS")
db21 <- read_excel("DataAnbima/ed04/Edicao_04_RaioX - Base de Dados.xlsx", sheet = "com labels")



# Extrair variáveis selecionadas da base original para o estudo do projeto.
# 
base19 <- data.frame(db19$p1a, db19$rclasse2)
#View(base19)

base21 <- data.frame(db21$P1A, db21$CLASSE)
#View(base21)

# remove a base COMPLETA original e ficar com apenas as variáveis selecionadas
rm(db19)
rm(db21)


## ===== PREPARAÇÃO DADOS ========

# Renomear as variáveis
trocanomes <- c("Produto",
                "Classe")

# Aplicar novos nomes na base extraída
names(base19) <- trocanomes
names(base21) <- trocanomes

# deletar objeto não mais necessário.
rm(trocanomes)



######################## Parte 1 de 2  ######################################
###   PROPARAÇÃO APENAS DO ARQUIVO CP19 = CLASSE X PRODUTO 2019 #############
#############################################################################

# Selecionas as OBSERVAÇÕES conforme a tabela descrita no TCC.
filtro_cp19 <- c("Ações", "Aplicação/ aplicação/ investimento bancaria/ financeira/ investimento bancário (s/espec.)",
                 "Bolsa de valores", "CDB", "CDI", "Conta poupança/ caderneta de poupança",
                 "LCI", "Moeda digital (Bitcoin)", "Outras ref. aplicações financeiras/ bancárias",
                 "Previdência privada", "Renda fixa/ fundo de investimento",
                 "Tesouro nacional/ direto", "Título de capitalização (pic)")

# Aplicar o filtro para diminuir as observações de 3452 para 273
# retirando que não investe, ou investe em outros propósitos que não
# sejam em Produtos Financeiros estudados nesse projeto.
cp19 <- base19 %>% filter(base19$Produto %in% filtro_cp19)

# remover objetos não mais necessários para economizar memória.
rm(base19)
rm(filtro_cp19)

# Renomear cada grupo de observações para utilizar nos gráficos (variável Produto)
cp19$Produto[cp19$Produto == "Ações"] <- "Acoes"
cp19$Produto[cp19$Produto == "Aplicação/ aplicação/ investimento bancaria/ financeira/ investimento bancário (s/espec.)"] <- "CDB"
cp19$Produto[cp19$Produto == "Tesouro nacional/ direto"] <- "Tesouro"
cp19$Produto[cp19$Produto == "Conta poupança/ caderneta de poupança"] <- "Poupanca"
cp19$Produto[cp19$Produto == "CDI"] <- "CDB"
cp19$Produto[cp19$Produto == "Bolsa de valores"] <- "Acoes"
cp19$Produto[cp19$Produto == "LCI"] <- "LetraCredito"
cp19$Produto[cp19$Produto == "Moeda digital (Bitcoin)"] <- "Bitcoin"
cp19$Produto[cp19$Produto == "Outras ref. aplicações financeiras/ bancárias"] <- "CDB"
cp19$Produto[cp19$Produto == "Previdência privada"] <- "FundoInvest"
cp19$Produto[cp19$Produto == "Título de capitalização (pic)"] <- "PIC"
cp19$Produto[cp19$Produto == "Renda fixa/ fundo de investimento"] <- "CDB"

# Renomear cada grupo de observações para utilizar nos gráficos (variável Classe)
cp19$Classe[cp19$Classe == "CLASSE A"] <- "A"
cp19$Classe[cp19$Classe == "Classe B1"] <- "B1"
cp19$Classe[cp19$Classe == "Classe B2"] <- "B2"
cp19$Classe[cp19$Classe == "Classe C1"] <- "C1"
cp19$Classe[cp19$Classe == "Classe C2"] <- "C2"

# Salvar os dados tratados no arquivo ClasseProduto19.RData neste projeto.
save(cp19, file = "dados/database/ClasseProduto19.RData")

# se quiser ver o resultado da base
view(cp19)

# remover o objeto já salvo.
rm(cp19)


######################## Parte 2 de 2  ######################################
###   PROPARAÇÃO APENAS DO ARQUIVO CP21 = CLASSE X PRODUTO 2021 #############
#############################################################################

# Selecionas as OBSERVAÇÕES conforme a tabela descrita no TCC.
filtro_cp21 <- c("Ações na Bolsa de Valores","Aplicação financeiras/ investimento bancário/ investimento bancário (s/ especificar)",
                 "CDI","CDB","Conta poupança/ caderneta de poupança","Fundos de ações",
                 "Fundos de investimento (sem especificar)","Fundos Imobiliários",
                 "Fundos multimercados","Guarda/ economiza/ deposita no banco",
                 "LCA","LCI","Mercado Financeiro","Moeda digital/ Criptomoeda (Bitcoin)",
                 "Opções binárias","Outras ref. aplicações financeiras/ bancárias",
                 "Previdência privada","Renda Fixa (s/ especificar)/ Fundos de Renda Fixa",
                 "Renda Variável (sem especificar)","Tesouro direto/ nacional",
                 "Título de capitalização (Pic)")

# Aplicar o filtro para diminuir as observações de 3408 para 582
# retirando que não investe, ou investe em outros propósitos que não
# sejam em Produtos Financeiros estudados nesse projeto.
cp21 <- base21 %>% filter(base21$Produto %in% filtro_cp21)

# remover objetos não mais necessários para economizar memória.
rm(base21)
rm(filtro_cp21)

# Renomear cada grupo de observações para utilizar nos gráficos (variável Produto)
cp21$Produto[cp21$Produto == "Ações na Bolsa de Valores"] <- "Acoes"
cp21$Produto[cp21$Produto == "Aplicação financeiras/ investimento bancário/ investimento bancário (s/ especificar)"] <- "CDB"
cp21$Produto[cp21$Produto == "CDI"] <- "CDB"
cp21$Produto[cp21$Produto == "Conta poupança/ caderneta de poupança"] <- "Poupanca"
cp21$Produto[cp21$Produto == "Fundos de ações"] <- "FundoInvest"
cp21$Produto[cp21$Produto == "Fundos de investimento (sem especificar)"] <- "FundoInvest"
cp21$Produto[cp21$Produto == "Fundos Imobiliários"] <- "FundoInvest"
cp21$Produto[cp21$Produto == "Fundos multimercados"] <- "FundoInvest"
cp21$Produto[cp21$Produto == "Guarda/ economiza/ deposita no banco"] <- "Poupanca"
cp21$Produto[cp21$Produto == "LCA"] <- "LetraCredito"
cp21$Produto[cp21$Produto == "LCI"] <- "LetraCredito"
cp21$Produto[cp21$Produto == "Mercado Financeiro"] <- "Acoes"
cp21$Produto[cp21$Produto == "Moeda digital/ Criptomoeda (Bitcoin)"] <- "Bitcoin"
cp21$Produto[cp21$Produto == "Opções binárias"] <- "Acoes"
cp21$Produto[cp21$Produto == "Outras ref. aplicações financeiras/ bancárias"] <- "CDB"
cp21$Produto[cp21$Produto == "Previdência privada"] <- "FundoInvest"
cp21$Produto[cp21$Produto == "Renda Fixa (s/ especificar)/ Fundos de Renda Fixa"] <- "FundoInvest"
cp21$Produto[cp21$Produto == "Renda Variável (sem especificar)"] <- "Acoes"
cp21$Produto[cp21$Produto == "Tesouro direto/ nacional"] <- "Tesouro"
cp21$Produto[cp21$Produto == "Título de capitalização (Pic)"] <- "PIC"


# Renomear cada grupo de observações para utilizar nos gráficos (variável Classe)
cp21$Classe[cp21$Classe == "Classe A"] <- "A"
cp21$Classe[cp21$Classe == "Classe B1"] <- "B1"
cp21$Classe[cp21$Classe == "Classe B2"] <- "B2"
cp21$Classe[cp21$Classe == "Classe C1"] <- "C1"
cp21$Classe[cp21$Classe == "Classe C2"] <- "C2"

# Salvar os dados tratados no arquivo ClasseProduto21.RData neste projeto.
save(cp21, file = "dados/database/ClasseProduto21.RData")

# se quiser ver o resultado da base
view(cp21)

# remover o objeto já salvo.
rm(cp21)



