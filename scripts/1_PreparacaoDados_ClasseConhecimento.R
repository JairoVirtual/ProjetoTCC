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
#           os arquivos ClasseConhecimento19.RData e o            #
#           ClasseConhecimento21.RData.                           #
#                                                                 #
###################################################################
#####    Extrair as bases Classe x Conhecimento 2019 e 2021  ######
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



#### ESSA ETAPA PREPARA DOS DOIS ARQUIVOS NA SEQUÊNCIA.
# Carregar base original armazenada no projeto (já extraídos do arquivo .zip)
db19 <- read_excel("DataAnbima/ed02/Edicao_02_RaioX - Base de Dados.xlsx", sheet = "LABELS")
db21 <- read_excel("DataAnbima/ed04/Edicao_04_RaioX - Base de Dados.xlsx", sheet = "com labels")



# Extrair variáveis selecionadas da base original para o projeto
base19 <- data.frame(db19$p1a, db19$rclasse2,
                     db19$`p20 - (MOSTRE CARTÃO 20) Suponha que você possui R$ 100,00 em investimentos financeiros que rendem 2% ao ano. Depois de 5 anos, quanto você imagina que terá como saldo de sua aplicação se deixar o dinheiro aplicado neste período? (ESTIMULADA E ÚNICA)`,
                     db19$`p21 - (MOSTRE CARTÃO 21) Imagine que o rendimento de seu investimento é de 1% ao ano e a inflação foi de 2% ao ano. Depois de um ano, quanto você imagina que poderá comprar com o dinheiro que ficou aplicado neste período? (ESTIMULADA E ÚNICA)`,
                     db19$`p22 - (MOSTRE CARTÃO 22 - FRASE) Por favor, diga se esta afirmativa é verdadeira ou falsa: “Comprar ações de uma única empresa gera um rendimento mais seguro do que um fundo de ações.” (ESTIMULADA E ÚNICA)`)

#View(base19)

base21 <- data.frame(db21$P1A, db21$CLASSE,
                     db21$P20 ,
                     db21$P21 ,
                     db21$P22)
#View(base21)

# remove a base total original e fica apenas a selecionada
rm(db19)
rm(db21)

## ===== PREPARAÇÃO DADOS ========

# Renomear as variáveis
trocanomes <- c("Produto",
                "classe1",
                "P20",
                "P21",
                "P22")

# Aplicar novos nomes na base extraída
names(base19) <- trocanomes
names(base21) <- trocanomes

# converter Nº Respostas corretas das P20, P21 e P22 em P20aux, P21aux e P22aux
base19 <- mutate(base19, P20aux = recode(P20,
                                         "Mais do que R$ 102,00"=1,
                                         "Não sabe"=0,
                                         "Menos do que R$ 102,00"=0,
                                         "Exatamente R$ 102,00"=0),
                 P21aux = recode(P21,
                                 "Menos do que hoje"=1,
                                 "Não sabe"=0,
                                 "Mais do que hoje"=1,
                                 "Exatamente o mesmo que hoje"=0),
                 P22aux = recode(P22,
                                 "Falso"=1,
                                 "Não sabe"=0,
                                 "Verdadeiro"=0))

# converter Nº Respostas corretas das P20, P21 e P22 em P20aux, P21aux e P22aux
base21 <- mutate(base21, P20aux = recode(P20,
                                         "Mais do que R$ 102,00"=1,
                                         "Não sabe"=0,
                                         "Menos do que R$ 102,00"=0,
                                         "Exatamente R$ 102,00"=0),
                 P21aux = recode(P21,
                                 "Menos do que hoje"=1,
                                 "Não sabe"=0,
                                 "Mais do que hoje"=1,
                                 "Exatamente o mesmo que hoje"=0),
                 P22aux = recode(P22,
                                 "Falso"=1,
                                 "Não sabe"=0,
                                 "Verdadeiro"=0))

# transforma observações sem respostas (NA) em zero para cada pergunta base 2019
base19 <- base19 %>% mutate(P20aux = base19$P20aux %>% replace_na(0))
base19 <- base19 %>% mutate(P21aux = base19$P21aux %>% replace_na(0))
base19 <- base19 %>% mutate(P22aux = base19$P22aux %>% replace_na(0))

# transforma observações sem respostas (NA) em zero para cada pergunta base 2021
base21 <- base21 %>% mutate(P20aux = base21$P20aux %>% replace_na(0))
base21 <- base21 %>% mutate(P21aux = base21$P21aux %>% replace_na(0))
base21 <- base21 %>% mutate(P22aux = base21$P22aux %>% replace_na(0))



# soma total de acertos e cria uma nova variável chamada "soma"
base19 <- base19 %>% mutate(soma = base19$P20aux + base19$P21aux + base19$P22aux)

base21 <- base21 %>% mutate(soma = base21$P20aux + base21$P21aux + base21$P22aux)

#criar uma variável "Conhecimento" e armazena a concatenação da string "Acertou:"
# com o valor do total de acertos por observação, ou do entrevistado para base 2019
base19$Conhecimento <- paste(rep("Acertou:",3452),base19$soma)

#criar uma variável "Conhecimento" e armazena a concatenação da string "Acertou:"
# com o valor do total de acertos por observação, ou do entrevistado para base 2019
base21$Conhecimento <- paste(rep("Acertou:",3408),base21$soma)

# tratar variável Classe retirando do texto a palavra "Classe" da observação.
base19$classe2 <- gsub("Classe ", "", base19$classe1)
base19$Classe <- gsub("CLASSE ", "", base19$classe2)

# o mesmo tratamento das linhas acima para a versão 2021
base21$classe2 <- gsub("Classe ", "", base21$classe1)
base21$Classe <- gsub("CLASSE ", "", base21$classe2)

# Extrair as variáveis prontas 2019 para um novo objeto
Classe <- base19$Classe
Conhecimento <- base19$Conhecimento

# Extrair as variáveis prontas 2021 para um novo objeto
Classe <- base21$Classe
Conhecimento <- base21$Conhecimento

# juntando as duas variáveis prontas num objeto
cc19 <- tibble(Classe, Conhecimento)
cc21 <- tibble(Classe, Conhecimento)

# salvando o resultado de todo o tratamento no arquivo em formato .RData
save(cc19, file = "dados/database/ClasseConhecimento19.RData")
save(cc21, file = "dados/database/ClasseConhecimento21.RData")


