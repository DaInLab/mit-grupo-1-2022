# Universidade Estadual Paulista - UNESP
# PPGMiT FAAC-UNESP
#
# Trabalho Final da Disciplina Ciência de Dados - 2022
# Prof. Joao Pedro Albino
#
# Alunos :  Lívia Inglesis Barcellos
#           Henrique Mercez
#           Marcelo José dos Santos
#           Shelley Navari
#
# Impacto da COVID-19 nos Estudantes Universitários no Brasil
# Fonte de dados : 

# como os alunos vivenciaram a pandemia da COVID-19;
# de que forma se comportaram frente as restrições impostas pelos riscos de contágio;
# (quais suas considerações a respeito das estratégias que foram adotadas pelas instituições superiores; e
# como estes fatores influenciaram sua vida.

# minha proposta
# quantidade de respostas do questionario
# 1.perfil : faixa etaria, genero, onde mora, cursos, nivel ensino
# 2.as pessoas na pandemia : vacinacao, 
#       psicologico : ansiedade, qualidade de vida, convivencia risco relevante
#       condicao financeira : renda, ajuda, endividamento, despesas   
# 3. estrategias das insituicoes superiores
# 4.     

# Import packages
library(tidyverse)
library(ggplot2)

# Importacao arquivo CSV
dataset.csv <-read.csv("./dados/COVID19IES.csv", header = TRUE, sep = ";", quote = "\"", dec = ".")

# Numero Total de Questionarios respondidos
num_questionario = as.numeric(nrow(dataset.csv))
print(paste("Numero de Questionarios Respondidos :", num_questionario))

# Grafico Perfil : Genero (1)
genero <- table(dataset.csv$genero)
df_genero <- as.data.frame(table(dataset.csv$genero))

grafico.genero <- ggplot(dataset.csv, aes(y=genero, fill=genero))+
                  geom_bar()+
                  geom_text(aes(label = ..count..), stat = "count", size = 5, hjust = -0.3, vjust = 0, colour = "black")+theme(legend.position="none")+
                  labs(title="Perfil : Gênero", x = "Quantidade", y = "")  
grafico.genero

ggplot(dataset.csv, aes(genero, fill = genero)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequencia")+
ggtitle("Perfil : Gênero")+
scale_fill_discrete(name="")

# Grafico Perfil : Faixas Etárias (2)
idade <- table(dataset.csv$idade)

# Porcentagem das faixas etárias
pct_idade <- paste0(round(unname(idade) / sum(unname(idade)) * 100,0), "%")

# Amplitude 
valor_min = min(dataset.csv$idade)
valor_max = max(dataset.csv$idade)

df_faixa_etaria <- data.frame(faixa_etaria="", idade=dataset.csv$idade )

for (k in 1:nrow(df_faixa_etaria)) {
  if(df_faixa_etaria$idade[k] <= 24) df_faixa_etaria$faixa_etaria[k] <- "de 17 a 24 anos"
  if(df_faixa_etaria$idade[k] >= 25 & df_faixa_etaria$idade[k] <= 26) df_faixa_etaria$faixa_etaria[k] <- "de 22 a 26 anos"
  if(df_faixa_etaria$idade[k] >= 27 & df_faixa_etaria$idade[k] <= 31) df_faixa_etaria$faixa_etaria[k] <- "de 27 a 31 anos"
  if(df_faixa_etaria$idade[k] >= 32 & df_faixa_etaria$idade[k] <= 36) df_faixa_etaria$faixa_etaria[k] <- "de 32 a 36 anos"
  if(df_faixa_etaria$idade[k] >= 37 & df_faixa_etaria$idade[k] <= 41) df_faixa_etaria$faixa_etaria[k] <- "de 37 a 41 anos"
  if(df_faixa_etaria$idade[k] >= 42 & df_faixa_etaria$idade[k] <= 46) df_faixa_etaria$faixa_etaria[k] <- "de 42 a 46 anos" 
  if(df_faixa_etaria$idade[k] >= 47 & df_faixa_etaria$idade[k] <= 51) df_faixa_etaria$faixa_etaria[k] <- "de 47 a 51 anos" 
  if(df_faixa_etaria$idade[k] >= 52 & df_faixa_etaria$idade[k] <= 56) df_faixa_etaria$faixa_etaria[k] <- "de 52 a 56 anos"
  if(df_faixa_etaria$idade[k] >= 57 & df_faixa_etaria$idade[k] <= 61) df_faixa_etaria$faixa_etaria[k] <- "de 57 a 61 anos"
  if(df_faixa_etaria$idade[k] > 61) df_faixa_etaria$faixa_etaria[k] <- "acima de 61 anos"
}

df_faixa_etaria

## table idade_concat
tbl_faixa_etaria <- table(df_faixa_etaria$faixa_etaria)
tbl_faixa_etaria

## Porcentagem por Faixa Etaria
pct_faixa_etaria <- paste0(round(unname(tbl_faixa_etaria) / sum(unname(tbl_faixa_etaria)) * 100,0), "%")
pct_faixa_etaria

grafico_faixa_etaria <- barplot(tbl_faixa_etaria, 
                              main = "Gráfico Perdil: Faixa etária",
                              xlab = "Faixa Etária", 
                              ylab = "Respondentes",
                              col = c("blue", "orange"),
                              ylim = c(0,max(tbl_faixa_etaria) + 10),
                              cex.axis=1.0, cex.names=0.8)
text(x = grafico_faixa_etaria, y = tbl_faixa_etaria, label = paste(tbl_faixa_etaria, " (", pct_faixa_etaria, ")"), cex=1, pos=3)

grafico_faixa_etaria

# Grafico Instituicao de Ensino (77% Unesp Bauru)     
df_ies <- data.frame(sigla_ies="", ies=dataset.csv$ies )

for (k in 1:nrow(df_ies)) {
  if(df_ies$ies[k] == "UNESP Bauru") df_ies$sigla_ies[k]<- "UNESP Bauru"
  if(df_ies$ies[k] != "UNESP Bauru") df_ies$sigla_ies[k]<- "Demais Instituições"
}

df_ies$sigla_ies

tbl_ies <- table(df_ies$sigla_ies)
tbl_ies

pct_ies <- paste(round(unname(tbl_ies) / sum(unname(tbl_ies)) * 100), "%")
pct_ies

pie(tbl_ies, main = "Perfil : Instituicoes de Ensino", labels = pct_ies, col = c(4,5))
legend("right", c("Demais Instituições", "Unesp"), 
       cex = 1.5, fill = c(4,5))

# Grafico Nivel Ensino ()
df_nivel_ensino <- data.frame(nivel_ensino = dataset.csv$nivel_ensino )

df_nivel_ensino

tbl_nivel_ensino <- table(dataset.csv$nivel_ensino)
tbl_nivel_ensino

pct_nivel_ensino <- paste(round(unname(tbl_nivel_ensino) / sum(unname(tbl_nivel_ensino)) * 100), "%")
pct_nivel_ensino

pie(tbl_nivel_ensino, main = "Perfil : Nivel Ensino", labels = df_nivel_ensino$nivel_ensino)

pie(tbl_nivel_ensino,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black", "blue"),
    labels = paste(names(tbl_nivel_ensino), "-", tbl_nivel_ensino),
    main = "Gráfico 5: Quantidade de respondentes por nível de ensino")

