#_______________Projeto Analise de Dados do Shopping da Barbie_________________#----
#_______________________________ Elisa ________________________________________#

# 1) Faturamento anual por categoria;
# 2) Variação do preço por marca;
# 3) Relação entre categorias (apenas feminino e masculino) e marca;
# 4) Relação entre preço e avaliação;
# 5) Frequência de cada tipo de devolução por marca;


#_________Carregamento de Dados e Pacotes----

#setwd('C:/Users/elisa/Estatistica/Estat/PS Estat')
#getwd()

pacman::p_load(readr, dplyr, lubridate, ggplot2, stringr, tidyverse, xtable)

# library(readr)
# library(dplyr)
# library(lubridate)
# library(ggplot2)
# library(stringr)
# library(tidyverse)
# library(xtable)

# Carregando dados

dados_dev <- read_csv('devolução.csv', locale = locale(encoding = "UTF-8"))
dados_vendas <- read_csv('vendas.csv', locale = locale(encoding = "UTF-8"))

#_________Funcoes da Estat----

# Tema para grafico da Estat

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966",
                 "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function (...) {
  theme <- ggplot2:: theme_bw () +
    ggplot2:: theme (
      axis.title.y = ggplot2::element_text( colour = "black ",
                                            size = 12),
      axis.title.x = ggplot2::element_text( colour = "black ",
                                            size = 12),
      axis.text = ggplot2:: element_text( colour = " black", size
                                          = 9.5),
      panel.border = ggplot2:: element_blank () ,
      axis.line = ggplot2:: element_line( colour = " black"),
      legend.position = "top",
      ...
    )
  return (
    list(
      theme ,
      scale_fill_manual( values = cores_estat ),
      scale_colour_manual( values = cores_estat )
    )
  )
}


#_________Arrumar os Bancos de Dados----

# Selecionando colunas com informacoes

dados_vendas <- dados_vendas[,c(3:13,15)]

# Transformar as categorias em factor

dados_vendas$Category <- dados_vendas$Category %>%
  factor(levels = c("Men's Fashion", "Women's Fashion", "Kids' Fashion"),
         labels = c("Moda Masculina", "Moda Feminina", "Moda Infantil"))

# Transformar as marcas (lojas) em factor

dados_vendas$Brand <- dados_vendas$Brand %>%
  factor()

levels(dados_vendas$Brand)

# Padronizar "Data de Venda" para ano-mes-dia

dados_vendas$`Data Venda` <- mdy(dados_vendas$`Data Venda`)

# Criar colunda mes e dia

dados_vendas["Dia_Venda"] <- day(dados_vendas$`Data Venda`)
dados_vendas["Mes_Venda"] <- month(dados_vendas$`Data Venda`)

# Transformar Mes em factor

dados_vendas["Mes_Venda"] <- dados_vendas$Mes_Venda %>% factor(levels = 1:12,
                                    labels = c("Jan", "Fev", "Mar", "Abr", "Mai", 
                                               "Jun", "Jul", "Ago", "Set", "Out",
                                               "Nov", "Dez"))
levels(dados_vendas$Mes_Venda)

# Ha dados repetidos?

dados_vendas["Duplicado"] <- duplicated(dados_vendas) # coluna indicadora de dados repetidos
sum(dados_vendas["Duplicado"]) # Soma os dados duplicados (100 dados repetidos)
duplicados <- dados_vendas[dados_vendas$Duplicado == TRUE,] # quem sao os duplicados
dados_vendas <- dados_vendas[dados_vendas$Duplicado == FALSE,] # eliminando os dados duplicados


# _________1) Faturamento Anual por Categoria----


# Faturamento Anual por Categoria

sum(is.na(dados_vendas$Price)) # 10 obs. sem preco especificado
sum(is.na(dados_vendas$Category)) # 10 obs sem especificar categoria
which(is.na(dados_vendas$Category))

vendas <- dados_vendas[!is.na(dados_vendas$Price),] # banco sem na em Price
sum(is.na(vendas$Price))
sum(is.na(vendas$Category)) # continuou com os 10 sem especeficar a categoria

sum(vendas$Price) # Faturamento anual total

fat_Anual <- vendas %>%
  group_by(Category)%>%
  summarise(faturamento = sum(Price)) # nao informaram cat <- 465 de faturamento

sum(fat_Anual$faturamento)


# Transformar Na em outra classe
fat_Anual$Category <- as.character(fat_Anual$Category) # Reiniciar
fat_Anual$Category[which(is.na(fat_Anual$Category))]<- "Não Especificado"
fat_Anual$Category <- factor(fat_Anual$Category, levels = c("Moda Masculina", "Moda Feminina", "Moda Infantil", "Não Especificado"),
                             labels = c("Moda Masculina", "Moda Feminina", "Moda Infantil", "Não Especificado"))


levels(fat_Anual$Category)

fat_Anual <-  fat_Anual %>%
  mutate(freq = round((.$faturamento/sum(.$faturamento)*100), 2))# % que cada cat 
                                                                 # representa no 
                                                                 # faturamento
  
fat_Anual <-  transform(fat_Anual,freq = paste(fat_Anual$freq, "%", sep = ""),
                       label = str_c(fat_Anual$faturamento,"(",fat_Anual$freq,")")) # rodar 2X


# grafico 

graph_fat <- ggplot(fat_Anual,aes(x = fct_reorder(Category , faturamento, .desc=T),
                                  y = faturamento, label = label))+
  geom_bar(stat = "identity", fill = '#A11D21')+
  scale_y_continuous(breaks = seq(0,18000,1500)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust= -0.5,
    size = 3)+
  labs(x = "Categorias", y = "Faturamento (reais)") +
  theme_estat()

ggsave("Grafico_Colunas_Fat_Anual.pdf", graph_fat, width = 158, height = 93, units = "mm")


# faturamento por categoria segundo os meses do ano

nrow(vendas[is.na(vendas$Mes_Venda),]) # 10 nao tem data especificada
vendas[which(is.na(vendas$Mes_Venda)),] #quais que nao tem data de venda especificada

fat_mes<- aggregate(Price ~ Category + Mes_Venda, data = vendas, FUN=sum)

fat_mes <- fat_mes %>%
  group_by(Mes_Venda)%>%
  mutate(fat_anual = sum(Price))%>%
  ungroup()

fat_mes <- rename(fat_mes, "Categoria"="Category")

graph_fat_mes <- ggplot(fat_mes)+
  aes(x = Mes_Venda, y = Price, group = Categoria, colour = Categoria)+
  scale_y_continuous(breaks = seq(0,4000,500)) +
  geom_line(size = 1)+
  geom_point(size = 2)+
  scale_colour_manual(name="Categoria")+
  labs(x="Mês", y="Faturamento (reais)")+
  theme_estat()

ggsave ("Garfico_Linhas_Fat_Mes.pdf", graph_fat_mes, width = 158, height = 93, 
        units = "mm")


#_________2) Variacao de Preco por Marca----

# Analisar frequencia de cada marca

summary(dados_vendas$Brand) # freq de cada marca no banco completo
summary(vendas$Brand) # freq de cada marca no banco sem na em Price
                      # 10 nao especificaram marca

1000/5 # Proporcao esperada para cada marca

which(is.na(vendas$Brand)) # Quais que possuem na em marca
dados.marca <- vendas[!is.na(vendas$Brand),]

# Box-plot e teste anova 

quadro_resumo_marca <- dados.marca%>%
  group_by(Brand)%>%
  summarise(Média = mean(Price), 
            Variância = (sd(Price))^2,
            Mínimo = min(Price),
            `1° Quartil` = round(quantile(Price , probs = .25),2),
            Mediana = round(quantile(Price , probs = .5),2),
            `3° Quartil` = round(quantile(Price , probs = .75),2),
            Maximo = max(Price)) %>% t() %>% as.data.frame() %>%
  mutate(V1 = str_replace(V1,"\\.",","),V2 = str_replace(V2,"\\.",","),
         V3 = str_replace(V3,"\\.",","),V4 = str_replace(V4,"\\.",","),
         V5 = str_replace(V5,"\\.",","))

xtable::xtable(quadro_resumo_marca)


box_plot_preco_marca <- ggplot(dados.marca, aes(x = Brand, y = Price)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Marcas", y = "Preço (reais)") +
  theme_estat() # Nao parece ter grande variacao de preco entre as marcas

ggsave("box_plot_preco_marca.pdf", width = 158, height = 93, units = "mm")


# teste de hipotese -> h0 = nao ha diferenca entre os precos medios para diferentes marcas
#                      ha = ha diferenca entre os precos medios para diferentes marcas


anova_marca<-aov(Price~Brand, data=dados.marca)
summary(anova_marca) # Ao nivel de significancia, 0.05, nao rejeita a hipotese nula


#_________3) Relacao entre Categorias (feminino e masculino) e Marca----












