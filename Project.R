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

pacman::p_load(readr, dplyr, lubridate, ggplot2, stringr, tidyverse, xtable,
               nortest, lmtest)


# library(readr)
# library(dplyr)
# library(lubridate)
# library(ggplot2)
# library(stringr)
# library(tidyverse)
# library(xtable)
# library(nortest)
# library(lmtest)

# Carregando dados

dados_dev <- read_csv('devolução_atualizado.csv', locale = locale(encoding = "UTF-8"))
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
  scale_y_continuous(breaks = seq(0,18000,2500)) +
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
  summarise(Média = round(mean(Price), 2), 
            Variância = round((sd(Price))^2,2),
            `Desvio Padrão` = round(sd(Price), 2),
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


# teste de hipotese -> h0 = Os precos medios para diferentes marcas sao iguais
#                      ha = ha diferenca entre os precos medios para diferentes marcas


anova_marca<-aov(Price~Brand, data=dados.marca)
summary(anova_marca) # Ao nivel de significancia, 0.05, nao rejeita a hipotese nula


#_________3) Relacao entre Categorias (feminino e masculino) e Cor----

unique(dados_vendas$Color) # Sao 6 cores

dados3 <- dados_vendas[!is.na(dados_vendas$`Product ID`),] # Retirando NA's de 
# product ID

dados.moda_cor <- dados3[dados3$Category != "Moda Infantil",] # filtrando as 
# categorias de 
# interesse

dados.moda_cor<- dados.moda_cor[!is.na(dados.moda_cor$Color),]# Retirar na em cor
sum(is.na(dados.moda_cor$Category))


moda_cor <- dados.moda_cor %>%
  group_by(Category , Color) %>%
  summarise (freq = n())# freq absoluta de cada cor segundo as categorias

unique(moda_cor$Color)

# Transformar cor em fator
moda_cor$Color <- factor(moda_cor$Color, levels = 
                           c("Black","Yellow","White","Blue","Green","Red"),
                         labels = c("Preto","Amarelo","Branco","Azul","Verde",
                                    "Vermelho"))
levels(moda_cor$Color)

# Calcular freq relativa
moda_cor <- mutate(moda_cor, freq_relativa = round(freq/sum(freq)*100,2))

sum(moda_cor[1:6,4]) # moda masculina - freq total 100%

porcentagens <- str_c(moda_cor$freq_relativa , "%") %>% str_replace ("\\.",",")
label <- str_squish(str_c(moda_cor$freq , " (", porcentagens ,")"))

# Proporcao cor para cada categoria

grafico_coluna_moda_cor <- ggplot(moda_cor) + 
  geom_bar(aes(x = fct_reorder(Color, freq, .desc=F), y = freq, 
               group = Category, fill = Category), 
           stat = "identity", position = "dodge") +
  labs( x = "Cores",y = "Frequência", fill = "Categorias")+
  ylim(0,75)+
  geom_text(aes(x = Color, y = freq, label = label, 
                group = Category), 
            position =  position_dodge(width = 1), size = 3,
            vjust = 0.4, hjust = 0, angle = 0) + 
  coord_flip() +
  theme_estat()


ggsave("grafico_coluna_moda_cor.pdf", width = 158, height = 93, units = "mm")



################### TESTES: grafico de barras empilhadas #######################

#ggplot(cat_cor,aes(x = Color, fill = Category))+
#  geom_bar(position = "fill")+
#  scale_fill_manual(name="Categoria", values=c("#A11D21", "#003366"))+
#  labs( x = "Cores",y = "Porcentagem", fill = "Categoria")+
#  scale_y_continuous(labels = scales :: percent_format())+
#  theme_bw()+
#  theme(axis.title.y = element_text(colour="black", size=12),
#        axis.title.x = element_text(colour="black", size=12),
#        axis.title = element_text(colour="black", size=9.5),
#        axis.line = element_line(colour="black"),
#        panel.border = element_blank())+
#  theme(axis.text.x = element_text(colour="black",size = 9.5, angle = -20))+
#  theme(legend.position="top")
  

#ggplot(cat_cor,aes(x = str_wrap(Color, width = 4), fill = Category))+
#  geom_bar(position = "fill")+
#  labs( x = "Cores",y = "Porcentagem", fill = "Categoria")+
#  scale_y_continuous(labels = scales :: percent_format())+
#  theme_estat()
###############################################################################

grafico_col_emp_moda_cor <- ggplot(cat_cor,aes(x = Color, fill = Category))+
  geom_bar(position = "fill")+
  labs( x = "Cores",y = "Porcentagem", fill = "Categoria")+
  scale_y_continuous(labels = scales :: percent_format())+
  theme_estat()

ggsave("grafico_col_emp_moda_cor.pdf", width = 158, height = 93, units = "mm")


#_________4) Relacao entre Preço e Avaliacao----


# Para a analise descritiva sera usado o banco dados_vendas

# 1) Analise exploratoria das variaveis

  # Variavel: Preco(Price)

# Usar banco vendas, que nao tem na's em Price

# Box-Plot da variavel Preco:

box_plot_Preco <- ggplot(vendas) +
  aes(x=factor(""), y = Price) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "", y = "Preço (reais)") +
  theme_estat()

ggsave("box_plot_Preco.pdf", width = 158, height = 93, units = "mm")


# Quadro resumo da variavel Preco

quadro_preco <- matrix(nrow = 8, ncol = 2)
colnames(quadro_preco) <- c("Estatística", "Valor")

quadro_preco[,1]<- c("Média", "Variância", "Desvio Padrão", "Mínimo", "1° Quartil",
              "Mediana", "3° Quartil", "Máximo")

quadro_preco[1,2] <- round(mean(vendas$Price), 2)
quadro_preco[2,2] <- round(sd(vendas$Price)^2, 2)
quadro_preco[3,2] <- round(sd(vendas$Price), 2)
quadro_preco[4,2] <- min(vendas$Price)
quadro_preco[5,2] <- round(quantile(vendas$Price , probs = .25),2)
quadro_preco[6,2] <- round(quantile(vendas$Price , probs = .5),2)
quadro_preco[7,2] <- round(quantile(vendas$Price , probs = .75),2)
quadro_preco[8,2] <- max(vendas$Price)


xtable::xtable(quadro_preco)


# Teste de Normalidade

hist_Preco <- ggplot(vendas) +
  aes(x = Price) +
  geom_histogram(colour = "white ", fill = "#A11D21", binwidth = 7) +
  labs(x = "Preço (reais)", y = " Frequência Absoluta ") +
  theme_estat()

ggsave("hist_Preco.pdf", width = 158, height = 93, units = "mm")

# teste de hipotese Shapiro:
  # H0: Segue normal
  # Ha: Nao segue normal

shapiro.test(vendas$Price)

# nao rejeita H0, pvalor = 0.187, acima do nivel de significancia 0.05


# teste de hipotese Shapiro:
  # H0: Segue normal
  # Ha: Nao segue normal

lillie.test(vendas$Price)
# nao rejeita H0, pvalor = 0.158, acima do nivel de significancia 0.05



  # Variavel Resposta: Avaliacao (Rating)

rat <- dados_vendas[!is.na(dados_vendas$Rating),] # Banco sem na's em Rating
sum(is.na(rat$Rating)) # nao ha na's em rat

box_plot_Aval <- ggplot(rat) +
  aes(x=factor(""), y = Rating) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "", y = "Avaliação") +
  theme_estat()

ggsave("box_plot_Aval.pdf", width = 158, height = 93, units = "mm")


aval <- matrix(nrow = 8, ncol = 2)
colnames(aval) <- c("Estatística", "Valor")

aval[,1]<- c("Média", "Variância", "Desvio Padrão", "Mínimo", "1° Quartil",
              "Mediana", "3° Quartil", "Máximo")

aval[1,2] <- round(mean(rat$Rating), 2)
aval[2,2] <- round(sd(rat$Rating)^2, 2)
aval[3,2] <- round(sd(rat$Rating), 2)
aval[4,2] <- round(min(rat$Rating),2)
aval[5,2] <- round(quantile(rat$Rating , probs = .25),2)
aval[6,2] <- round(quantile(rat$Rating , probs = .5),2)
aval[7,2] <- round(quantile(rat$Rating , probs = .75),2)
aval[8,2] <- round(max(rat$Rating), 2)

xtable::xtable(aval)

hist_Rating <- ggplot(rat) +
  aes(x = Rating) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 0.25) +
  labs(x = "Avaliação", y = " Frequência Absoluta ") +
  theme_estat()

ggsave("hist_Rating.pdf", width = 158, height = 93, units = "mm")

# teste de hipotese Shapiro:
# H0: Segue normal
# Ha: Nao segue normal

shapiro.test(rat$Rating)
# nao rejeita H0, pvalor = 0.915, acima do nivel de significancia 0.05


# teste de hipotese Shapiro:
# H0: Segue normal
# Ha: Nao segue normal

lillie.test(rat$Rating)
# nao rejeita H0, pvalor = 0.365, acima do nivel de significancia 0.05


# Criar banco sem na's em Price e em Rating

sum(is.na(vendas$Price)) # nao ha na em Price
sum(is.na(vendas$Rating))# ha 10 na em rating

  # Usando o banco vendas pois nao ha na em Price

dados.reg <- vendas[!is.na(vendas$Rating),]
sum(is.na(dados.reg$Rating)) # nao ha na em Rating nem em Price


  # Usaremos esse banco, dados.reg, para o calculo da correlacao e para
  # a elaboracao da regressao

# Analise bivariada - Correlacao entre as variaveis 

  # Diagrama de dispercao
diagram_prec_rat <- ggplot(dados.reg) +
  aes(y = Rating , x = Price) +
  geom_point( colour = "#A11D21", size = 2) +
  labs(
    x = "Preço (reais)",
    y = "Avaliação") +
  theme_estat()

ggsave("diagram_prec_rat.pdf", width = 158, height = 93, units = "mm")

cor(dados.reg$Rating, dados.reg$Price, method = "pearson")
# Correlação alta - 0.9138


# A ideia eh testar a influencia do preco na avaliacao!


modelo <- lm(data = dados.reg, formula = Rating~Price)
summary(modelo) # Pelo teste t, as estimativas sao diferentes de 0
# a covariavel rating explica 83,5% da variacao da variavel preco
# eh um valor alto


#Avaliando a qualidade do ajuste da reta de regressao

ggplot(dados.reg, aes(y=Rating, x=Price)) + 
  geom_point(colour = "#A11D21", size = 2) +
  geom_smooth(formula = y~x, method = lm, se = FALSE, colour = "black")+
  labs(y = "Avaliação", x = "Preço (em reais)") +
  theme_estat()


# Analise dos residuos

par(mfrow = c(2,3))
plot(modelo, which = c(1:5), pch = 20)


# Grafico residuo X valores ajustados (Homocedasticidade)

graf_resid_fit <- ggplot(modelo, aes(x=.fitted, y=.resid))+
  geom_point(colour = "#A11D21", size = 2) +
  geom_hline(yintercept = 0) +
  labs(x = "Valores Ajustados", y = "Resíduos") +
  theme_estat()

ggsave("graf_resid_fit.pdf", width = 158, height = 93, units = "mm")


# Grafico Normalidade dos Residuos

graf_normal_QQ <- ggplot(modelo, 
                         aes(sample = (.resid - mean(.resid))/
                               sd(.resid)))+
  stat_qq(colour = "#A11D21") +
  stat_qq_line() +
  labs(x= "Quantis Teóricos",
       y = "Resíduos Estudentizados")+
  theme_estat()

  # teste de normalidade dos residuos
shapiro.test(modelo$residuals)
# nao rejeita H0, p-valor = 0.296, superior ao nivel de significancia

ggsave("graf_normal_QQ.pdf", width = 158, height = 93, units = "mm")


# Grafico dos residuos (independendcia)

plot(modelo$residuals) # Independencia -> substitui scale-location

grafico_resid <- ggplot(modelo, aes(x=1:length(.resid), y=.resid))+
  geom_point(colour = "#A11D21", size = 2) +
  labs(x = "Ordem de Observação", y = "Resíduos") +
  theme_estat()

ggsave("grafico_resid.pdf", width = 158, height = 93, units = "mm")


# Teste de homocedasticidade - Brauch-Pagan
  # Hipoteses
  # H0: a var dos erros sao iguais
  # Ha: a var dos erros sao diferentes

bptest(modelo) # nao rej H0, p-valor superior ao nivel de significancia



#_________5) Frequência de Cada Tipo de Devolucao por Marca----

sum(is.na(dados_vendas$`Unique ID`)) # nao ha na em Unique ID

unique(dados_dev$`Motivo devolução`) # Sao 3 tipos de devolucao

table(dados_dev$`Motivo devolução`)
table(dados_vendas$`Motivo devolução`)

devol <- merge(dados_vendas, dados_dev, by = "Unique ID")


# 3 observacoes sem marca registrada
devol <- devol[!is.na(devol$Brand),]

dev_marca <- devol %>%
  group_by(Brand, `Motivo devolução.y`)%>%
  summarise(freq = n())%>%
  mutate(freq_rel = round(freq/sum(freq)*100,2))

sum(dev_marca[dev_marca$Brand=="H&M",]$freq_rel) # soma das % igual a 100


porcentagens <- str_c(dev_marca$freq_rel, "%") %>% str_replace("\\.",",")
label <- str_squish(str_c(dev_marca$freq , " (", porcentagens ,")"))

# Proporcao tipo de dev para cada marca



grafico_coluna_dev_marca <- ggplot(dev_marca) + 
  geom_bar(aes(x = Brand, y = freq, group = `Motivo devolução.y`, 
               fill = `Motivo devolução.y`), 
           stat = "identity", position = "dodge") +
  labs( x = "Marca", y = "Frequência", fill = "Motivo de devolução")+
  ylim(0,45) +
  geom_text(aes(x = Brand, y = freq, label = label, 
                group = `Motivo devolução.y`), 
            position =  position_dodge(width = 1), size = 3,
            vjust = 0.4, hjust = -0.1, angle = 0) + 
  coord_flip() +
  theme_estat()


ggsave("grafico_coluna_dev_marca.pdf", width = 158, height = 93, units = "mm")



#_________6) Avaliacao Media por Marca----

# Verificar se ha na nas variaveis de interesse
sum(is.na(dados_vendas$Rating)) #ha 10 na em rating
sum(is.na(dados_vendas$Brand)) # ha 10 na em marca
sum(is.na(dados_vendas$`Product ID`)) # ha 10 na em produto ID

# Verificar se ha alguma obs com na em comum entre as variaveis
which(is.na(dados_vendas$Rating))
which(is.na(dados_vendas$Brand))
which(is.na(dados_vendas$`Product ID`))

# obs 928 tem na em rating e em brand

# Retirar os na's
aval_marca <- dados_vendas[!is.na(dados_vendas$Rating),]
aval_marca <- aval_marca[!is.na(aval_marca$Brand),]
aval_marca <- aval_marca[!is.na(aval_marca$`Product ID`),]

# Dados com a avaliacao media por marca
media.aval_marca <- aval_marca %>%
  group_by(Brand)%>%
  summarise(aval_media = round(mean(Rating),2))

# grafico 
graf_aval_marcas <- ggplot(media.aval_marca, aes(x = reorder(Brand,aval_media,.desc = T),
                     y = aval_media, label = aval_media))+
  geom_bar(stat = "identity", fill = '#A11D21')+
  ylim(0,3) +
  geom_text(
    position = position_dodge(width = .9),
    vjust= -0.5,
    size = 3)+
  labs(x = "Marcas", y = "Avaliação") +
  theme_estat()


ggsave("graf_aval_marcas.pdf", width = 158, height = 93, units = "mm")


# Box-plot para comparar a avaliacao de cada marca

box_plot_aval_marca <- ggplot(aval_marca, aes(x = Brand, y = Rating)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  scale_y_continuous(breaks = seq(0,5,1)) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Marcas", y = "Rating") +
  theme_estat() # Nao parece ter grande variacao de preco entre as marcas

ggsave("box_plot_aval_marca.pdf", width = 158, height = 93, units = "mm")

