# ====================================================================#
# Arquivo  : 02_anacorClasseProdutos19.R
# Descricao: Dados de 2019 "COM filtro"  NÃO PASSOU NO TESTE QUI-QUADRADO.
#            Portanto, ficou  "SEM filtro" com investidores com conhecimento
#            básico em educação financeira (Acertou as perguntas da variável
#            (P20),(P21) e (P22) do arquivo ed02.zip da Anbima), mas com
#            filtro na variável (P1a) retirando o que não for produtos 
#            financeiros. 
#            Os códigos executam esses dados em SQLITE, fazem a tabela de
#            Contingência, testam o qui-quadrado, criam o Mapa de Calor com 
#            os Resíduos Padronizados ajustados e finalizam com a execução
#            do Mapa Perceptual simple.
#=====================================================================#

#Carregando a base de dados 2019
# --- abrindo a base em arquivo .SQLITE


# Buscando o arquivo SQLITE para ler a tabela
My.db.sqlite <-         # nome do dataframe, ex: My.db.sqlite
  here::here(
    "dados",
    "database",         # local onde esta a base de dados salva
    "db.DB19.SQLITE"    # base de dados que sera lida
  )



# abrindo a conexao com o sqlite
my_con <- dbConnect(
  drv = SQLite(),       # chamando o sqlite
  My.db.sqlite          # local da base a ser lida
  
)

# para ver as tabelas existentes em sua base
dbListTables(my_con)

# fazendo a leitura da tabela
my_df <- DBI::dbReadTable(
  conn = my_con,
  name = dbListTables(my_con)   # nome da tabela em sqlite  
) 


# funcao para ver as colunas/variaveis de sua tabela
dbListFields(
  my_con,
  dbListTables(my_con)
)


# ver a estrutura da tabela
glimpse(my_df)

# Tabela de Contingência da variável Classe x Produtos
tab <- table(my_df$Classe, my_df$Produtos)
tab

#Exemplo de uma tabela de contingências mais elegante
sjt.xtab(var.row = my_df$Classe,
         var.col = my_df$Produtos,
         show.exp = TRUE)

#Teste Qui-Quadrado
qui2 <- chisq.test(tab)
qui2

#Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(categoria = 1,
         cpc = 2) %>% 
  ggplot(aes(x = fct_rev(categoria), y = cpc, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "darkblue", 
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



# disconnect
dbDisconnect(my_con)
