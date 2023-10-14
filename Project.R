#_______________Projeto Analise de Dados do Shopping da Barbie_________________#----
#_______________________________ Elisa ________________________________________#

# 1) Faturamento anual por categoria;
# 2) Variação do preço por marca;
# 3) Relação entre categorias (apenas feminino e masculino) e marca;
# 4) Relação entre preço e avaliação;
# 5) Frequência de cada tipo de devolução por marca;


#_________Carregamento de Dados e Pacotes----

setwd('C:/Users/elisa/Estatistica/Estat/PS Estat')
getwd()

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidyverse)

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
         labels = c("Men's Fashion", "Women's Fashion", "Kids' Fashion"))

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

vendas <- dados_vendas[!is.na(dados_vendas$Price),] # exlui na em Price
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
fat_Anual$Category <- factor(fat_Anual$Category, levels = c("Men's Fashion", "Women's Fashion", "Kids' Fashion", "Não Especificado"),
                             labels = c("Men's Fashion", "Women's Fashion", "Kids' Fashion", "Não Especificado"))


levels(fat_Anual$Category)

fat_Anual <-  fat_Anual %>%
  mutate(freq = round((.$faturamento/sum(.$faturamento)*100), 2))# % que cada cat 
                                                                 # representa no 
                                                                 # faturamento
  
fat_Anual <-  transform(fat_Anual,freq = paste(fat_Anual$freq, "%", sep = ""),
                       label = str_c(fat_Anual$faturamento,"(",fat_Anual$freq,")")) # rodar 2X


# grafico 

graph_fat <- ggplot(fat_Anual,aes(x = Category, y = faturamento, label = label))+
  geom_bar(stat = "identity", fill = '#A11D21')+
  geom_text(
    position = position_dodge(width = .9),
    vjust= -0.5,
    size = 3)+
  labs(x = "Categorias", y = "Faturamento") +
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
  geom_line(size = 1)+
  geom_point(size = 2)+
  scale_colour_manual(name="Categoria")+
  labs(x="Mês", y="Faturamento")+
  theme_estat()

ggsave ("Garfico_Linhas_Fat_Mes.pdf", graph_fat_mes, width = 158, height = 93, 
        units = "mm")


#_________1) Variacao de Preco por Marca----

# Analisar frequencia de cada marca

summary(dados_vendas$Brand)


# Box-plot e teste anova 

a<-vendas%>%
  group_by(Brand)%>%
  summarise(media = mean(Price), v = (sd(Price))^2)

boxplot(Price~Brand, vendas)





b<-aov(Price~as.factor(Brand), data=vendas)
summary(b)
TukeyHSD(b) # Comparacao entre categorias #Para P-Valor maior que alpha, medias iguais

lm_brand <- lm(Price~as.factor(Brand), data=vendas)
summary(lm_brand)



shapiro.test(vendas$Price)
ggplot(vendas)+
  aes(x=Price)+
  geom_histogram(colour = "white ", fill = "#A11D21", binwidth = 2)+
  theme_estat()



