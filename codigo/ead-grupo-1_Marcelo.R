# Universidade Estadual Paulista - UNESP
# PPGMiT FAAC-UNESP
#
# Trabalho Final da Disciplina Ciência de Dados - 2022
# Prof. Joao Pedro Albino
#
# Alunos :  Lívia Inglesis Barcellos
#           Henrique Mercez
#           Marcelo Santos
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

# Grafico Dados Pessoais : 1.Idade (Faixa Etária) 
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
                                main = "Gráfico Dados Pessoais: Faixa etária",
                                xlab = "Faixa Etária", 
                                ylab = "Respondentes",
                                col = c("blue", "orange"),
                                ylim = c(0,max(tbl_faixa_etaria) + 10),
                                cex.axis=1.0, cex.names=0.8)
text(x = grafico_faixa_etaria, y = tbl_faixa_etaria, label = paste(tbl_faixa_etaria, " (", pct_faixa_etaria, ")"), cex=1, pos=3)

grafico_faixa_etaria

# Grafico Dados Pessoais : 1.Genero
genero <- table(dataset.csv$genero)
df_genero <- as.data.frame(table(dataset.csv$genero))
df_genero

pct_genero <- paste0(round(unname(genero) / sum(unname(genero)) * 100,0), "%")
pct_genero

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

# Grafico Dados Pessoais : 1.Situacao_conjugal
sit_conjugal <- table(dataset.csv$situacao_conjugal)
sit_conjugal

# Porcentagem 
pct_sit_conjugal <- paste0(round(unname(sit_conjugal) / sum(unname(sit_conjugal)) * 100,0), "%")

grafico_sit_conjugal <- barplot(sit_conjugal, 
                                main = "Gráfico Dados Pessoais : Situação Conjugal",
                                xlab = "Situacao Conjugal", 
                                ylab = "Respondentes",
                                col = "red",
                                ylim = c(0,max(sit_conjugal) + 10),
                                cex.axis=1.0, cex.names=0.8)
text(x = grafico_sit_conjugal, y = sit_conjugal, label = paste(sit_conjugal, " (", pct_sit_conjugal, ")"), cex=1, pos=3)

grafico_sit_conjugal

# Grafico Dados Pessoais : 1.Situacao_empregaticia
sit_emprego <- table(dataset.csv$situacao_empregaticia)
sit_emprego

# Porcentagem 
pct_sit_emprego <- paste0(round(unname(sit_emprego) / sum(unname(sit_emprego)) * 100,0), "%")
pct_sit_emprego

par(las=1) # nomes dos eixos perpendicular
par(mar=c(5,16,1,1)+0.1)  # para aumentar a margem a esquerda 
grafico_sit_empregaticia <- barplot(sit_emprego, 
                                    main="Grafico Dados Pessoas : Situaçao Empregaticia",
                                    las=1,  
                                    beside = TRUE,
                                    horiz=TRUE, 
                                    xlim = c(0,max(sit_emprego) + 10),
                                    legend.text = paste(rownames(sit_emprego)," (",pct_sit_emprego, ")"),
                                    args.legend = list("top", bty="n", cex = 0.7),
                                    col=rainbow(10),
                                    cex.axis = 0.7,  
                                    cex.names = 0.6)

text(grafico_sit_empregaticia, x = sit_emprego, label = sit_emprego, cex=0.8, pos=2)

par(mar=c(5,4,4,2)+0.1) # para retornar a margem default
                                    

# Gratico Dados Pessoais : 1.estado_reside
estado_reside <- table(dataset.csv$estado_reside, exclude = NULL)
estado_reside

# Porcentagem 
pct_estado_reside <- paste0(round(unname(estado_reside) / sum(unname(estado_reside)) * 100,0), "%")
pct_estado_reside

# Tratando Nulo
names(pct_estado_reside) <-c("Não Respondeu", "Amazonas", "São Paulo")

pie(estado_reside,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = rainbow(3),
    labels = paste(names(pct_estado_reside), "-", pct_estado_reside),
    main = "Gráfico Dados Pessoais: Respondentes por estado")

# Grafico Dados Pessoais : 1.Instituicao de Ensino     
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
legend("right", c("Demais Instituições", "UNESP Bauru"), 
       cex = 1.5, fill = c(4,5))

# Grafico Dados Pessoais : 1.Nome_Cursos
nome_cursos <- table(dataset.csv$nome_curso)
nome_cursos

# Reduçao nome dos cursos
names(nome_cursos) <- c("Arquitetura", "Direito", "Ciências Computação", "Sistemas Informação", "Inovação em Moda",
                       "Biologia", "Ciências Computação", "Educação Especial", "Educaçao Física", "Eng Civil",
                       "Eng Produção", "Eng Elétrica", "Eng Elétrica", "Eng Mecânica", "Gestão TI", "Física",
                       "Matemática", "Medicina", "Pos Administração","Pos Biblioteconomia", "Pos Ciências de Materiais",
                       "Pos em Design", "Pos Educação Básica", "Pos Educação para Ciência", "Pos Midia e Tecnologia", 
                       "Pos em Quimica", "Psicologia", "Psicologia", "Tecnico Adm")

pct_nome_cursos <- paste(round(unname(nome_cursos) / sum(unname(nome_cursos)) * 100), "%")
pct_nome_cursos
                       

par(las=1) # nomes dos eixos perpendicular
par(mar=c(5,16,1,1)+0.1)  # para aumentar a margem a esquerda 
grafico_nome_cursos <- barplot(nome_cursos, 
                                    main="Grafico Dados Pessoas : Cursos",
                                    las=1,  
                                    beside = TRUE,
                                    horiz=TRUE, 
                                    xlim = c(0,max(nome_cursos) + 10),
                                    legend.text = paste(rownames(nome_cursos)," (",pct_nome_cursos, ")"),
                                    args.legend = list("top", bty="n", cex = 0.7),
                                    col=rainbow(20),
                                    cex.axis = 0.7,  
                                    cex.names = 0.6)

text(grafico_nome_cursos, x = nome_cursos, label = nome_cursos, cex=0.8, pos=2)

par(mar=c(5,4,4,2)+0.1) # para retornar a margem default

# Grafico 1.Nivel Ensino 
tbl_nivel_ensino <- table(dataset.csv$nivel_ensino)
tbl_nivel_ensino

pct_nivel_ensino <- paste(round(unname(tbl_nivel_ensino) / sum(unname(tbl_nivel_ensino)) * 100), "%")
pct_nivel_ensino

pie(tbl_nivel_ensino,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black", "blue"),
    labels = paste(names(tbl_nivel_ensino), "-", tbl_nivel_ensino),
    main = "Gráfico Dados Pessoais: Quantidade de respondentes por nível de ensino")

# Grafico 1.Tipo Instituicao de Ensino
tipo_ies <- table(dataset.csv$tipo_ies)
tipo_ies

# Autarquia municipal             Privada             Pública 
#               1                   6                  45 

pct_tipo_ies <- paste(round(unname(tipo_ies) / sum(unname(tipo_ies)) * 100), "%")
pct_tipo_ies

# "2 %"  "12 %" "87 %"

pie(tipo_ies,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = rainbow(3),
    labels = paste(names(tipo_ies), "-", tipo_ies),
    main = "Gráfico Dados Pessoais: Quantidade de respondentes por tipo Inst Ensino")

# Grafico Dados Pessoais 1. Local Estudante (você é um estudante nativo local, originário de outra cidade ou um estudante internacional)
local_estudante <- table(dataset.csv$local_estudante)
local_estudante

#        Local Outra cidade 
#          18           34 

pct_local_estudante <- paste(round(unname(local_estudante) / sum(unname(local_estudante)) * 100), "%")
pct_local_estudante

# "35 %" "65 %"

grafico_local_estudante <- pie(local_estudante,
                            edges = 200, radius = 0.8,
                            clockwise = T,
                            density = NULL, angle = 90, col = rainbow(3),
                            labels = paste(names(local_estudante), "-", local_estudante),
                            main = "Gráfico Dados Pessoais: respondentes por origem/localização")

# Grafico Dados Pessoais 1. Residencia Atual do estudante
residencia_atual <- table(dataset.csv$residencia_atual,  exclude = "")
residencia_atual

# Em outra cidade da instituição de ensino mas dentro do mesmo estado        18
# Fora do campus, mas na mesma cidade da instituição de ensino                 33 

# Reduçao nome 
names(residencia_atual) <- c("Em Outra Cidade", "Na mesma Cidade")

pct_residencia_atual <- paste(round(unname(residencia_atual) / sum(unname(residencia_atual)) * 100), "%")
pct_residencia_atual

# "35 %" "65 %"

pie(residencia_atual,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = rainbow(3),
    labels = paste(names(residencia_atual), "-", pct_residencia_atual),
    main = "Gráfico Dados Pessoais: Residencia dos respondentes x Cidade Inst Ensino")

# Grafico Dados Pessoais 1. Moradia Permanente
moradia_atual_perm <- table(dataset.csv$moradia_atual_permanente)
moradia_atual_perm

# Não Sim 
#   5  47 

pct_moradia_atual_perm <- paste(round(unname(moradia_atual_perm) / sum(unname(moradia_atual_perm)) * 100), "%")
pct_moradia_atual_perm


grafico_moradia_atual_perm <- pie(moradia_atual_perm,
                              edges = 200, radius = 0.8,
                              clockwise = T,
                              density = NULL, angle = 90, col = rainbow(3),
                              labels = paste(names(moradia_atual_perm), "-", pct_moradia_atual_perm),
                              main = "Gráfico Dados Pessoais: Moradia Atual permanente dos respondentes")

# Grafico Dados Pessoais 1. Respondentes morando com 
morando_com <- table(dataset.csv$morando_.com)
morando_com

# Colega de quarto/República                    Família                 Sozinho(a) 
#                      3                         36                         13 

pct_morando_com <- paste(round(unname(morando_com) / sum(unname(morando_com)) * 100), "%")
pct_morando_com


grafico_morando_com <- pie(morando_com,
                                  edges = 200, radius = 0.8,
                                  clockwise = T,
                                  density = NULL, angle = 90, col = rainbow(3),
                                  labels = paste(names(morando_com), "-", pct_morando_com),
                                  main = "Gráfico Dados Pessoais: Com quem mora os respondentes ")

# Grafico Dados Instituicao de Ensino  2. Migrou Virtual
# Sua instituição migrou para as aulas virtuais durante a pandemia da COVID-19? 
migrou_virtual <- table(dataset.csv$migrou_virtual)
migrou_virtual

# Não Sim 
#  1  51 

pct_migrou_virtual <- paste(round(unname(migrou_virtual) / sum(unname(migrou_virtual)) * 100), "%")
pct_migrou_virtual

# "2 %"  "98 %"

grafico_migrou_virtual <- pie(migrou_virtual,
                           edges = 200, radius = 0.8,
                           clockwise = T,
                           density = NULL, angle = 90, col = rainbow(2),
                           labels = paste(names(migrou_virtual), "-", pct_migrou_virtual),
                           main = "Gráfico Instituiçao: Sua instituição migrou para as aulas virtuais durante a pandemia da COVID-19? ")

# Grafico Dados Instituicao de Ensino  2. Fechou Dormitorios
# Sua instituição fechou os dormitórios ou residências estudantis devido à crise da COVID-19? 
ies_fechou_dorm <- table(dataset.csv$ies_fechou_dorm)
ies_fechou_dorm

# Não Não se aplica       Não sei           Sim 
#  4             9            32             7 

pct_ies_fechou_dorm <- paste(round(unname(ies_fechou_dorm) / sum(unname(ies_fechou_dorm)) * 100), "%")
pct_ies_fechou_dorm

# "8 %"  "17 %" "62 %" "13 %"

grafico_ies_fechou_dorm <- pie(ies_fechou_dorm,
                           edges = 200, radius = 0.8,
                           clockwise = T,
                           density = NULL, angle = 90, col = rainbow(4),
                           labels = paste(names(ies_fechou_dorm), "-", pct_ies_fechou_dorm),
                           main = "Gráfico Instituiçao: Sua instituição fechou os dormitórios ou residências estudantis devido à crise da COVID-19? ")

# Grafico Dados Instituicao de Ensino 2. Instituicao_acesso_infra
# Com relação às formas de acesso aos recursos de infraestrutura oferecidos pela sua instituição (biblioteca, coordenação, orientação de assuntos acadêmicos, matrícula, etc.) durante a pandemia do COVID-19 você sentiu que: *
ies_infra <- table(dataset.csv$acesso_infra_ies, exclude = "")
ies_infra

# Ficou mais ou menos o mesmo                    Melhorou             N/A ou Não sabe                    Pioraram                      Piorou 
#                      17                           7                           3                          20                           1 
 

pct_ies_infra <- paste(round(unname(ies_infra) / sum(unname(ies_infra)) * 100), "%")
pct_ies_infra

# "35 %" "15 %" "6 %"  "42 %" "2 %" 

grafico_ies_infra <- pie(ies_infra,
                               edges = 200, radius = 0.8,
                               clockwise = T,
                               density = NULL, angle = 90, col = rainbow(5),
                               labels = paste(names(ies_infra), "-", pct_ies_infra),
                               main = "Gráfico Instituiçao: às formas de acesso aos recursos de infra oferecidos durante a COVID-19 você sentiu que:")

# Grafico Dados Instituicao de Ensino 2. Instituicao_reinicio
# A sua instituição já reiniciou todas as atividades presenciais no seu campus e/ou faculdade?
ies_reinicio <- table(dataset.csv$ies_reinicio, exclude = "")
ies_reinicio

# Em parte (apenas algumas atividades  presenciais retornaram)            N/A ou Não sabe 
#                14                                                            1 
#  Não, ainda não retornou nenhuma atividade presencial               Sim (retornou todas as atividades presenciais) 
#           4                                                           29 

# Reduçao nome dos cursos
names(ies_reinicio) <- c("Em parte", "Não Sabe", "Não, Nenhuma", "Sim, Todas")

pct_ies_reinicio <- paste(round(unname(ies_reinicio) / sum(unname(ies_reinicio)) * 100), "%")
pct_ies_reinicio

# "29 %" "2 %"  "8 %"  "60 %"" 

grafico_ies_reinicio <- pie(ies_reinicio,
                         edges = 200, radius = 0.8,
                         clockwise = T,
                         density = NULL, angle = 90, col = rainbow(5),
                         labels = paste(names(ies_reinicio), "-", pct_ies_reinicio),
                         main = "Gráfico Instituiçao: já reiniciou todas as atividades presenciais no seu campus e/ou faculdade?")

# Grafico Dados Instituicao de Ensino 2.decisao_fechar (utilizacao de ferramenta online)
# Com relação à decisão de fechar o campus e de utilizar ferramentas online para as aulas, por conta da pandemia da COVID-19, você sentiu que as decisões na sua instituição foram tomadas: *
ies_decisao_fechar <- table(dataset.csv$decisao_fechar, exclude = "")
ies_decisao_fechar

# De forma oportuna e prudente             Muito lentamente            Muito rapidamente 
#   40                            7                            5 


pct_ies_decisao_fechar <- paste(round(unname(ies_decisao_fechar) / sum(unname(ies_decisao_fechar)) * 100), "%")
pct_ies_decisao_fechar

# "77 %" "13 %" "10 %"" 

grafico_ies_decisao_fechar <- pie(ies_decisao_fechar,
                            edges = 200, radius = 0.8,
                            clockwise = T,
                            density = NULL, angle = 90, col = rainbow(3),
                            labels = paste(names(ies_decisao_fechar), "-", pct_ies_decisao_fechar),
                            main = "Gráfico Instituiçao:  decisão de fechar o campus e de utilizar ferramentas online p/as aulas,devido a COVID-19")


# Grafico 3. Pandemia_convive_risco 
# Você está morando ou convivendo atualmente com alguém na faixa etária  dos 60-70 anos, e/ou que 
# tenha algum fator de risco relevante? (Fatores de risco relevantes são problemas cardíacos, diabetes, hipertensão e/ou obesidade). *

convive_risco <- table(dataset.csv$convive_risco, exclude = "")
convive_risco

# Não Sim 
#  39  13

pct_convive_risco <- paste(round(unname(convive_risco) / sum(unname(convive_risco)) * 100), "%")
pct_convive_risco

# "75 %" "25 %"

grafico_convive_risco <- pie(convive_risco,
                         edges = 200, radius = 0.8,
                         clockwise = T,
                         density = NULL, angle = 90, col = rainbow(2),
                         labels = paste(names(convive_risco), "-", pct_convive_risco),
                         main = "Gráfico Pandemia Risco Relevante: Convivencia com Idoso (60-70 anos) ou alguem com Comorbidades")

# Grafico 3. Pandemia_quarentena imposta
# Você, alguém com quem convive ou que está em sua moradia está ou esteve em quarentena imposta pela COVID-19  ? *

quarentena_imposta <- table(dataset.csv$quarentena_imposta, exclude = "")
quarentena_imposta

# Não Sim 
#  19  33 

pct_quarentena_imposta <- paste(round(unname(quarentena_imposta) / sum(unname(quarentena_imposta)) * 100), "%")
pct_quarentena_imposta

# "37 %" "63 %"

grafico_quarentena_imposta <- pie(quarentena_imposta,
                             edges = 200, radius = 0.8,
                             clockwise = T,
                             density = NULL, angle = 90, col = rainbow(2),
                             labels = paste(names(quarentena_imposta), "-", pct_quarentena_imposta),
                             main = "Gráfico Pandemia Risco Relevante: Convivencia com quem convive ou que está em sua moradia está ou esteve em quarentena imposta pela COVID-19",
                             cex.main=0.9)



























# Nuvem de Palavras

# Nuvem de Palavras : Instituicao de Ensino - situacao durante pandemia
# Ajude-nos a compreender o conjunto e a diversidade da situação durante a pandemia da COVID-19 na sua instituição, compartilhando aqui qualquer informação adicional que julgar importante. (Escreva até 250 palavras).
library(tm)  #text mining
## Loading required package: NLP
library(dplyr)

str(dataset.csv$situação_durante_pandemia)
words <- as.character(dataset.csv$situação_durante_pandemia)

word.corpus <- Corpus(VectorSource(dataset.csv$situação_durante_pandemia))
word.corpus<-word.corpus%>%
  tm_map(removePunctuation)%>% ##eliminar pontuacao
  tm_map(removeNumbers)%>% #sem numeros
  tm_map(stripWhitespace)# sem espacos

word.counts <- as.matrix(TermDocumentMatrix(word.corpus))
word.freq <- sort(rowSums(word.counts), decreasing = TRUE)
head(word.freq)  ##what are the top words?

word.corpus<-word.corpus%>%
  tm_map(tolower)%>% ##make all words lowercase
  tm_map(removeWords, stopwords("por"))

library(wordcloud)  #wordcloud
set.seed(32)  #be sure to set the seed if you want to reproduce the same again

wordcloud(words = names(word.freq), freq = word.freq, scale = c(3, 0.5), max.words = 40, 
          random.order = TRUE, shape =  , color = rainbow(10))

