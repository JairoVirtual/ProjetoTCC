# ====================================================================#
# Arquivo  : 06_ACM_2018.R
# Descricao: O dataset "db.ACM2018.SQLITE" tem todas as observações originais
#            das variáveis Classe e P5. A variável "Conhecimento" é uma auxiliar
#            que juntou as respostas das variáveis P20, P21 e P22, dadas pelo
#            entrevistado, sintetizando se acertou ou não qualquer uma delas.
#            Consideradas nesta pesquisa como educação básica financeira e que
#            estão no arquivo original ed01.zip da Anbima).
#            Este Script executam esses dados em SQLITE, fazem a tabela de
#            Contingência par a par para explorar os dados, em seguida testam o
#            qui-quadrado, executa Analise Correspondência Multipla (ACM)
#            utilizando a matriz de Burt para explicar as associações
#            das categorias. Com as coordenadas obtidas é plotado o Mapa 
#            Perceptual com o ggplot.
#=====================================================================#

# Buscando o arquivo SQLITE para ler a tabela
My.db.sqlite <-         
  here::here(
    "dados",
    "database",         # local onde esta a base de dados salva
    "db.ACM2018.SQLITE"    # base de dados que sera lida
  )


# abrindo a conexao com o sqlite
my_con <- dbConnect(
  drv = SQLite(),       # chamando o sqlite
  My.db.sqlite          # local da base a ser lida
  
)

# fazendo a leitura da tabela
my_df <- DBI::dbReadTable(
  conn = my_con,
  name = dbListTables(my_con)   # nome da tabela em sqlite  
) 


# ver a estrutura da tabela
glimpse(my_df)


# -----    Análise de Correspondência Múltipla (ACM)  --------


# É utilizado a função table() ou a função sjt.xtab() para analisar as
# variáveis duas a duas, já que não tem como fazer com todas juntas para criar
# a tabela de Contingência

# Tabela de Contingência das variáveis P5 x Conhecimento
sjt.xtab(var.row = my_df$P5, 
         var.col = my_df$Conhecimento,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Teste Qui-Quadrado
tab_P5Conhecimento <- table(my_df$P5,
                            my_df$Conhecimento)

qui2_P5Conhecimento <- chisq.test(tab_P5Conhecimento)
options(scipen = 999)# mostra resultados que não sejam nº científicos
qui2_P5Conhecimento


# Tabela de Contingência das variáveis  P5 x Classe
sjt.xtab(var.row = my_df$P5,
         var.col = my_df$Classe,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Teste Qui-Quadrado
tab_P5Classe <- table(my_df$P5,
                      my_df$Classe)

tab_P5Classe

qui2_P5Classe <- chisq.test(tab_P5Classe)
qui2_P5Classe



# Tabela de Contingência das variáveis  Conhecimento x Classe
sjt.xtab(var.row = my_df$Conhecimento, 
         var.col = my_df$Classe,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Teste Qui-Quadrado
tab_ConhecimentoClasse <- table(my_df$Conhecimento,
                                my_df$Classe)

tab_ConhecimentoClasse

qui2_ConhecimentoClasse <- chisq.test(tab_ConhecimentoClasse)
qui2_ConhecimentoClasse


# ---------------- Rodando a ACM com a Matriz de Burt --------------------
ACM <- MCA(my_df[, 1:3], method = "Burt")


# As coordenadas de cada categoria:
round(ACM$var$coord, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)


#-------------------------- O Mapa Perceptual --------------------------


# Definir o número de categorias por variável
categorias <- apply(my_df[,1:3], 
                    MARGIN =  2, 
                    FUN = function(x) nlevels(as.factor(x)))

# Transformar o objeto ACM em um data frame, levando-se em consideração os 
# tipos de coordenadas dadas pela matriz de Burt para plotar.

ACM_mp <- data.frame(ACM$var$coord, Variável = rep(names(categorias), categorias))

# Exibindo o novo data frame com variáveis necessárias para plotar.
ACM_mp %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  mutate(Categoria = gsub("Classe.","", Categoria),
         Categoria = gsub("P5","", Categoria),
         Categoria = gsub("Conhecimento","", Categoria)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Plotando o Mapa Perceptual:
ACM_mp %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Categoria, 
             color = Variável, 
             shape = Variável)) +
  geom_point(shape = 17, color = "#61988E", size = 2) +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey90") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey90") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM$eig[1,2], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM$eig[2,2], 2), "%"))) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray"),
        legend.position = "none")

# disconnect
dbDisconnect(my_con)
#----------------FIM-------------------