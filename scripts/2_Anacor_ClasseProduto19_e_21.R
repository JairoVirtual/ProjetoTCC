###################################################################
#                                                                 #
#     Aluno: Jairo Rodrigues da Silva       Turma: DSA211         #
#                                                                 #
# Descrição: Este script aplica o modelo não supervisionado       #
#            Análise de Correspondência gerando um gráfico        #
#            Perceptual, antes disso,  é feito o carregamento dos #
#            dados já tratados, aplica-se o teste qui-quadrado e  #                                             #
#            cria-se um Mapa dos Resíduos Padronizados Ajustados. #
# Arquivos:  Este script já carrega os dados tratados, caso queira#
#            verificar como é feito, verifique o script           #
#            0_PreparacaoDados_ClasseProduto.R                    #
#                                                                 #
###################################################################
#####  Aplicar ANACOR nas bases Classe x Produto 2019 e 2021  #####
###################################################################

# Instalação de pacotes, caso não tenha
#install.packages("tidyverse")
#install.packages("sjPlot") # Tabelas simples e cruzadas
#install.packages("FactoMineR") # ANACOR



library("tidyverse")
library("sjPlot") # Tabelas simples e cruzadas
library("FactoMineR") # ANACOR


############################################################
#                                                          #
# Executando testes estatísticos e ANACOR na base de 2019  # 
#                                                          #
############################################################

#Carregando a base de dados já tratada
load(file = "dados/database/ClasseProduto19.RData")


# Tabela de Contingência da variável Classe x Produtos
tab <- table(cp19$Classe, cp19$Produto)
tab

#Exemplo de uma tabela de contingências mais elegante
sjt.xtab(var.row = cp19$Classe,
         var.col = cp19$Produto,
         show.exp = TRUE)

#Teste Qui-Quadrado
qui2 <- chisq.test(tab)
qui2

#Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(produto = 1,
         classe = 2) %>% 
  ggplot(aes(x = fct_rev(produto), y = classe, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 11) +
  scale_fill_gradient2(low = "#a9bcff", 
                       mid = "white", 
                       high = "red",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())


#Elaborando a ANACOR:
anacor <- CA(tab)

#Limpeza destes dados no RStudio
rm(anacor)
rm(cp19)
rm(qui2)
rm(tab)

# ------ FIM da base 2019 -----------#


############################################################
#                                                          #
# Executando testes estatísticos e ANACOR na base de 2021  # 
#                                                          #
############################################################


#Carregando a base de dados já tratada 2021
load(file = "dados/database/ClasseProduto21.RData")


# Tabela de Contingência da variável Classe x Produtos
tab <- table(cp21$Classe, cp21$Produto)
tab

#Exemplo de uma tabela de contingências mais elegante
sjt.xtab(var.row = cp21$Classe,
         var.col = cp21$Produto,
         show.exp = TRUE)

#Teste Qui-Quadrado
qui2 <- chisq.test(tab)
qui2

#Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(produto = 1,
         classe = 2) %>% 
  ggplot(aes(x = fct_rev(produto), y = classe, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 11) +
  scale_fill_gradient2(low = "#a9bcff", 
                       mid = "white", 
                       high = "red",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())


#Elaborando a ANACOR:
anacor <- CA(tab)

#Limpeza destes dados no RStudio
rm(anacor)
rm(cp21)
rm(qui2)
rm(tab)

# ------ FIM da base 2021 -----------#