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

# Grafico Perfil : Genero
genero <- table(dataset.csv$genero)
df_genero <- as.data.frame(table(dataset.csv$genero))

pie( main = "Genero",
    genero,
    edges = 200, 
    radius = 0.8,
    col= rainbow(4),
    labels = genero)

ggplot(dataset.csv, aes(genero, fill = genero))+
geom_bar()
scale_fill_manual("Genero")


                        