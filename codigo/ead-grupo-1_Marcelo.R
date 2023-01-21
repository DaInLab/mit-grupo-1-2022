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
#library(tidyverse)
library(ggplot2)
library(stringr)
library(readxl)

# Importacao arquivo CSV
dataset.csv <-read.csv("./dados/COVID19IES.csv", header = TRUE, sep = ";", quote = "\"", dec = ".")

# Importacao arquivo Excel 
dataset.xlsx <-dbf.xlsx <- read_excel("./dados/COVID19IES.xlsx")

# Numero Total de Questionarios respondidos
num_questionario = as.numeric(nrow(dataset.csv))
print(paste("Numero de Questionarios Respondidos :", num_questionario))

# Grafico Dados Pessoais : 1.Idade (Faixa Etária) 
# Qual a sua idade ? 
idade <- table(dataset.csv$idade)
idade

# 17 18 19 20 21 22 23 25 26 28 29 30 34 35 37 40 41 43 46 48 51 54 55 60 
#  1  1  3  4  2  3  4  4  3  1  1  1  1  2  1  3  3  4  1  2  2  2  2  1 

# Porcentagem das faixas etárias
pct_idade <- paste0(round(unname(idade) / sum(unname(idade)) * 100,0), "%")
pct_idade

# "2%" "2%" "6%" "8%" "4%" "6%" "8%" "8%" "6%" "2%" "2%" "2%" "2%" "4%" "2%" "6%" "6%" "8%" "2%" "4%" "4%" "4%" "4%" "2%"

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

# de 17 a 24 anos de 22 a 26 anos de 27 a 31 anos de 32 a 36 anos de 37 a 41 anos de 42 a 46 anos de 47 a 51 anos de 52 a 56 anos de 57 a 61 anos 
#             18               7               3               3               7               5               4               4               1 

## Porcentagem por Faixa Etaria
pct_faixa_etaria <- paste0(round(unname(tbl_faixa_etaria) / sum(unname(tbl_faixa_etaria)) * 100,0), "%")
pct_faixa_etaria

# "35%" "13%" "6%"  "6%"  "13%" "10%" "8%"  "8%"  "2%" 

grafico_faixa_etaria <- barplot(tbl_faixa_etaria, 
                                main = "Gráfico Dados Pessoais: Qual a sua idade ? ",
                                xlab = "Idades", 
                                ylab = "Respondentes",
                                col = rainbow(9),
                                ylim = c(0,max(tbl_faixa_etaria) + 5),
                                cex.axis=1.0, cex.names=0.7)
text(x = grafico_faixa_etaria, y = tbl_faixa_etaria, label = paste(tbl_faixa_etaria, " (", pct_faixa_etaria, ")"), cex=0,8, pos=3)


# Grafico Dados Pessoais : 1.Genero
# Qual o seu gênero ?
genero <- table(dataset.csv$genero)
df_genero <- as.data.frame(table(dataset.csv$genero))
df_genero

# Var1 Freq
# 1               Feminino   24
# 2              Homem gay    1
# 3              Masculino   25
# 4 Transgênero/Transexual    2

pct_genero <- paste0(round(unname(genero) / sum(unname(genero)) * 100,0), "%")
pct_genero
# "46%" "2%"  "48%" "4%"

#grafico.genero2 <- ggplot(dataset.csv, aes(y=genero, fill=genero))+
#                  geom_bar()+
#                  geom_text(aes(label = pct_genero), stat = "count", size = 5, hjust = -0.3, vjust = 0, colour = "black")+theme(legend.position="none")+
#                  labs(title="Perfil : Gênero", x = "Quantidade", y = "")  
#grafico.genero2

#ggplot(dataset.csv, aes(genero, fill = genero)) + 
#  geom_bar(aes(y = (..count..)/sum(..count..))) + 
#  scale_y_continuous(labels=scales::percent) +
#  ylab("Frequencia")+
#ggtitle("Perfil : Gênero")+
#scale_fill_discrete(name="")

grafico_genero <- barplot(genero, 
                                main = "Gráfico Dados Pessoais: Qual o seu gênero ?",
                                ylab = "Respondentes",
                                col = c(rainbow(4)),
                                ylim = c(0,max(genero) + 5),
                                cex.axis=1.0, cex.names=1)
text(x = grafico_genero, y = genero, label = paste(genero, " (", pct_genero, ")"), cex=1, pos=3)


# Grafico Dados Pessoais : 1.Situacao_conjugal
# Qual sua situação conjugal ?
sit_conjugal <- table(dataset.csv$situacao_conjugal)
sit_conjugal

# Casado(a)   Divorciado(a)/Separado(a)                 Solteiro(a) União Estável/Vivendo junto                    Viúvo(a) 
#       16                           2                          28                           5                           1 

# Porcentagem 
pct_sit_conjugal <- paste0(round(unname(sit_conjugal) / sum(unname(sit_conjugal)) * 100,0), "%")
pct_sit_conjugal

# "31%" "4%"  "54%" "10%" "2%" 

grafico_sit_conjugal <- barplot(sit_conjugal, 
                                main = "Gráfico Dados Pessoais : Qual sua situação conjugal ?",
                                xlab = "", 
                                ylab = "Respondentes",
                                col = rainbow(5),
                                ylim = c(0,max(sit_conjugal) + 4),
                                cex.axis=1.0, cex.names=0.8)
text(x = grafico_sit_conjugal, y = sit_conjugal, label = paste(sit_conjugal, " (", pct_sit_conjugal, ")"), cex=1, pos=3)


# Grafico Dados Pessoais : 1.Situacao_empregaticia
# Qual sua situação empregatícia e/ou financeira atual ? 

df_situacao_emp <- data.frame(situacao ="", emp=dataset.csv$situacao_empregaticia)
df_situacao_emp

for (k in 1:nrow(df_situacao_emp)) {
  if(df_situacao_emp$emp[k] == "Aposentada")       df_situacao_emp$situacao[k]<- "Aposentado"
  if(df_situacao_emp$emp[k] == "Bolsista")          df_situacao_emp$situacao[k]<- "Bolsista ou Estagiário(a)"
  if(df_situacao_emp$emp[k] == "Corretor de Imóveis") df_situacao_emp$situacao[k]<- "Empregado(a)"
  if(df_situacao_emp$emp[k] == "Dependente (dos pais) com vínculo empregatício") df_situacao_emp$situacao[k]<- "Empregado(a)"
  if(df_situacao_emp$emp[k] == "Dependente (dos pais, etc.)") df_situacao_emp$situacao[k]<- "Dependente dos Pais"
  if(df_situacao_emp$emp[k] == "Desempregado(a)") df_situacao_emp$situacao[k]<- "Desempregado(a)"
  if(df_situacao_emp$emp[k] == "Empregado(a)") df_situacao_emp$situacao[k]<- "Empregado(a)"
  if(df_situacao_emp$emp[k] == "Empresária") df_situacao_emp$situacao[k]<- "Empregado(a)"
  if(df_situacao_emp$emp[k] == "Estagiário(a)") df_situacao_emp$situacao[k]<- "Bolsista ou Estagiário(a)"
  if(df_situacao_emp$emp[k] == "Micro-empresário instituído por ME") df_situacao_emp$situacao[k]<- "Empregado(a)"
  if(df_situacao_emp$emp[k] == "Servidora pública") df_situacao_emp$situacao[k]<- "Empregado(a)"
  if(df_situacao_emp$emp[k] == "tatuador autônomo, mas conto com ajuda dos pais") df_situacao_emp$situacao[k]<- "Empregado(a)"
}

df_situacao_emp

sit_emprego <- table(df_situacao_emp$situacao)
sit_emprego

# Aposentado Bolsista ou Estagiário(a)       Dependente dos Pais           Desempregado(a)              Empregado(a) 
#         1                        12                         7                         2                        30 


# Porcentagem 
pct_sit_emprego <- paste0(round(unname(sit_emprego) / sum(unname(sit_emprego)) * 100,0), "%")
pct_sit_emprego

# "2%"  "4%"  "19%" "13%" "4%"  "58%"

par(las=1) # nomes dos eixos perpendicular
par(mar=c(5,10,1,1)+0.1)  # para aumentar a margem a esquerda 
grafico_sit_empregaticia <- barplot(sit_emprego, 
                                    main="Grafico Dados Pessoas : Qual sua situação empregatícia e/ou financeira atual ? ",
                                    las=1,  
                                    beside = TRUE,
                                    horiz=TRUE, 
                                    xlim = c(0,max(sit_emprego) + 15),
                                    legend.text = paste(rownames(sit_emprego)," (",pct_sit_emprego, ")"),
                                    args.legend = list("top", bty="n", cex = 0.7),
                                    col=rainbow(5),
                                    cex.axis = 0.7,  
                                    cex.names = 0.8)

text(grafico_sit_empregaticia, x = sit_emprego, label = paste(sit_emprego, "(", pct_sit_emprego, ")"), cex=0.9, pos=4)

par(mar=c(5,4,4,2)+0.1) # para retornar a margem default
                                    
# Gratico Dados Pessoais : 1.estado_reside
# Qual o Estado no qual você reside e/ou estuda? 
estado_reside <- table(dataset.csv$estado_reside, exclude = NULL)
estado_reside

#    AM SP 
#  4  1 47 

# Porcentagem 
pct_estado_reside <- paste0(round(unname(estado_reside) / sum(unname(estado_reside)) * 100,0), "%")
pct_estado_reside

# "8%"  "2%"  "90%"

# Tratando Nulo
names(pct_estado_reside) <-c("Não Respondeu", "Amazonas", "São Paulo")

grafico_estado_reside <- pie(estado_reside,
                          edges = 200, radius = 0.8,
                          clockwise = F,
                          density = NULL, angle = 90, col = rainbow(3),
                          labels = paste(names(pct_estado_reside), "-", pct_estado_reside, " (", estado_reside, ")"),
                          main = "Gráfico Dados Pessoais: Qual o Estado no qual você reside e/ou estuda? ")

# Grafico Dados Pessoais : 1.Instituicao de Ensino     
# Qual o nome da sua instituição de ensino ?
df_ies <- data.frame(sigla_ies="", ies=dataset.csv$ies )
df_ies

for (k in 1:nrow(df_ies)) {
  if(df_ies$ies[k] == "UNESP Bauru") df_ies$sigla_ies[k]<- "UNESP Bauru"
  if(df_ies$ies[k] != "UNESP Bauru") df_ies$sigla_ies[k]<- "Demais Instituições"
}

df_ies$sigla_ies

tbl_ies <- table(df_ies$sigla_ies)
tbl_ies

# Demais Instituições         UNESP Bauru 
#             12                  40 

pct_ies <- paste(round(unname(tbl_ies) / sum(unname(tbl_ies)) * 100), "%")
pct_ies
#  "23 %" "77 %"

grafico_ies <- pie(tbl_ies, main = "Perfil : Qual o nome da sua instituição de ensino ?", labels = paste(df_ies$sigla_ies, " - ", pct_ies, "(", tbl_ies, ")"), col = c(4,5))

# Grafico Dados Pessoais : 1.Nome_Cursos
# Qual o nome/denominação do seu curso?
nome_cursos <- table(dataset.csv$nome_curso)
nome_cursos

#Arquitetura e Urbanismo                                       Bacharel em Direito 
#1                                                        1 
#Bacharelado em Ciências da Computação                   Bacharelado em Sistemas de Informação  
#1                                                        1 
#BEI - Bolsista de estimulo à Inovação em Moda                                                 Biologia 
#1                                                        1 
#Ciência da Computação                            Educação Especial e Inclusiva 
#1                                                        1 
#Educação Física                                          Engenharia Civil 
#1                                                        3 
#Engenharia de Produção                                       Engenharia Elétrica 
#1                                                        1 
#Engenharia Elétrica                                       Engenharia Mecânica 
#1                                                        2 
#Gestão da Tecnologia da Informação                                    Licenciatura em Física 
#1                                                        1 
#Licenciatura em matemática                                                 Medicina 
#1                                                        2 
###Pós-Graduação em Administração de Empresas Pós-Graduação em Biblioteconomia e Ciência da Informação 
###1                                                        2 
#Pós-Graduação em Ciência de Materiais                                  Pós-Graduação em Desing 
#1                                                        1 
#Pós-Graduação em Docência Para a Educação Básica                 Pós-Graduação em Educação para a Ciência 
#2                                                        4 
#Pós-Graduação em Mídia e Tecnologia                     Pós-Graduação em Quimica Tecnologica 
#13                                                        1 
#Psicologia                                              Psicologia  
#3                                                        1 
#Técnico em Administração 
#1 

# Reduçao nome dos cursos
names(nome_cursos) <- c("Arquitetura", "Direito", "Ciências Computação", "Sistemas Informação", "Inovação em Moda",
                       "Biologia", "Ciências Computação", "Educação Especial", "Educaçao Física", "Eng Civil",
                       "Eng Produção", "Eng Elétrica", "Eng Elétrica", "Eng Mecânica", "Gestão TI", "Física",
                       "Matemática", "Medicina", "Pos Administração","Pos Biblioteconomia", "Pos Ciências de Materiais",
                       "Pos em Design", "Pos Educação Básica", "Pos Educação para Ciência", "Pos Midia e Tecnologia", 
                       "Pos em Quimica", "Psicologia", "Psicologia", "Tecnico Adm")

pct_nome_cursos <- paste(round(unname(nome_cursos) / sum(unname(nome_cursos)) * 100), "%")
pct_nome_cursos

# "2 %"  "2 %"  "2 %"  "2 %"  "2 %"  "2 %"  "2 %"  "2 %"  "2 %"  "6 %"  "2 %"  "2 %"  "2 %"  "4 %"  "2 %"  "2 %"  "2 %"  "4 %"  "2 %"  "4 %"  "2 %"  "2 %" 
# "4 %"  "8 %"  "25 %" "2 %"  "6 %"  "2 %"  "2 %" 
                     
par(las=1) # nomes dos eixos perpendicular
par(mar=c(5,16,1,1)+0.1)  # para aumentar a margem a esquerda 
grafico_nome_cursos <- barplot(nome_cursos, 
                                    main="Grafico Dados Pessoas : Qual o nome/denominação do seu curso?",
                                    las=1,  
                                    beside = TRUE,
                                    horiz=TRUE, 
                                    xlim = c(0,max(nome_cursos) + 10),
                                    legend.text = paste(rownames(nome_cursos)," (",nome_cursos, ")"),
                                    args.legend = list("top", bty="n", cex = 0.7),
                                    col=rainbow(7),
                                    cex.axis = 0.7,  
                                    cex.names = 0.7)

text(grafico_nome_cursos, x = nome_cursos, label = pct_nome_cursos, cex=0.8, pos=4)

par(mar=c(5,4,4,2)+0.1) # para retornar a margem default

# Grafico 1.Nivel Ensino 
# Qual nível de ensino você está cursando ? 
tbl_nivel_ensino <- table(dataset.csv$nivel_ensino)
tbl_nivel_ensino

#         Doutorado     Ensino Técnico Especialização/MBA          Graduação           Mestrado      Pós-doutorado 
#              15                  1                  1                 21                 13                  1 

pct_nivel_ensino <- paste(round(unname(tbl_nivel_ensino) / sum(unname(tbl_nivel_ensino)) * 100), "%")
pct_nivel_ensino
# "29 %" "2 %"  "2 %"  "40 %" "25 %" "2 %" 

grafico_nivel_ensino <- pie(tbl_nivel_ensino,
                        edges = 200, radius = 0.8,
                        clockwise = T,
                        density = NULL, angle = 90, col = rainbow(6),
                        labels = paste(names(tbl_nivel_ensino), "-", tbl_nivel_ensino, "(", pct_nivel_ensino, ")"),
                        main = "Gráfico Dados Pessoais: Qual nível de ensino você está cursando ? ")

# Grafico 1.Tipo Instituicao de Ensino
# Qual é o tipo da sua instituição de ensino ? 
tipo_ies <- table(dataset.csv$tipo_ies)
tipo_ies

# Autarquia municipal             Privada             Pública 
#               1                   6                  45 

pct_tipo_ies <- paste(round(unname(tipo_ies) / sum(unname(tipo_ies)) * 100), "%")
pct_tipo_ies

# "2 %"  "12 %" "87 %"

grafico_tipo_ies <- pie(tipo_ies,
                    edges = 200, radius = 0.8,
                    clockwise = T,
                    density = NULL, angle = 90, col = rainbow(3),
                    labels = paste(names(tipo_ies), "-", tipo_ies, "(",pct_tipo_ies,")"),
                    main = "Gráfico Dados Pessoais: Qual é o tipo da sua instituição de ensino ? ")

# Grafico Dados Pessoais 1. Local Estudante (você é um estudante nativo local, originário de outra cidade ou um estudante internacional)
# Com relação à cidade onde está localizada sua instituição de ensino, você é um estudante nativo local, originário de outra cidade ou um estudante internacional ?
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
                            labels = paste(names(local_estudante), "-", local_estudante, "(", pct_local_estudante,")"),
                            main = "Gráfico Dados Pessoais: Com relação à cidade onde está localizada sua instituição de ensino, 
                                  você é um estudante nativo local, originário de outra cidade ou um estudante internacional ?",
                            cex.main = 0.9)

# Grafico Dados Pessoais 1. Residencia Atual do estudante
# Onde você está vivendo ou residindo atualmente?
residencia_atual <- table(dataset.csv$residencia_atual,  exclude = "")
residencia_atual

#Em outra cidade da instituição de ensino mas dentro do mesmo estado        Fora do campus, mas na mesma cidade da instituição de ensino 
#         18                                                                  33 

# Reduçao nome 
names(residencia_atual) <- c("Em Outra Cidade da Instituição", "Na mesma Cidade da Instituição")

pct_residencia_atual <- paste(round(unname(residencia_atual) / sum(unname(residencia_atual)) * 100), "%")
pct_residencia_atual

# "35 %" "65 %"

grafico_residencia_atual <- pie(residencia_atual,
                            edges = 100, radius = 0.8,
                            clockwise = T,
                            density = NULL, angle = 90, col = rainbow(3),
                            labels = paste(names(residencia_atual), "-", pct_residencia_atual, "(", pct_residencia_atual,")"),
                            main = "Gráfico Dados Pessoais: Onde você está vivendo ou residindo atualmente?")

# Grafico Dados Pessoais 1. Moradia Permanente
# Sua moradia atual é uma residência permanente e estável para você?
moradia_atual_perm <- table(dataset.csv$moradia_atual_permanente)
moradia_atual_perm

# Não Sim 
#   5  47 

pct_moradia_atual_perm <- paste(round(unname(moradia_atual_perm) / sum(unname(moradia_atual_perm)) * 100), "%")
pct_moradia_atual_perm
# "10 %" "90 %"

grafico_moradia_atual_perm <- pie(moradia_atual_perm,
                              edges = 200, radius = 0.8,
                              clockwise = T,
                              density = NULL, angle = 90, col = rainbow(3),
                              labels = paste(names(moradia_atual_perm), " ", moradia_atual_perm, "(", pct_moradia_atual_perm, ")"),
                              main = "Gráfico Dados Pessoais: Sua moradia atual é uma residência permanente e estável para você?")

# Grafico Dados Pessoais 1. Respondentes morando com 
# Atualmente, você está morando ou residindo com/em: 
morando_com <- table(dataset.csv$morando_.com)
morando_com

# Colega de quarto/República                    Família                 Sozinho(a) 
#                      3                         36                         13 

pct_morando_com <- paste(round(unname(morando_com) / sum(unname(morando_com)) * 100), "%")
pct_morando_com
# "6 %"  "69 %" "25 %"

grafico_morando_com <- pie(morando_com,
                                  edges = 200, radius = 0.8,
                                  clockwise = T,
                                  density = NULL, angle = 90, col = rainbow(3),
                                  labels = paste(names(morando_com), "  ", morando_com, "(", pct_morando_com, ")"),
                                  main = "Gráfico Dados Pessoais: Atualmente, você está morando ou residindo com/em:  ")

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
                           labels = paste(names(migrou_virtual), "", migrou_virtual,"(", pct_migrou_virtual, ")"),
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
                           labels = paste(names(ies_fechou_dorm), " ", ies_fechou_dorm, "(", pct_ies_fechou_dorm, ")"),
                           main = "Gráfico Instituiçao: Sua instituição fechou os dormitórios ou residências estudantis devido à crise da COVID-19? ")

# Grafico Dados Instituicao de Ensino 2. Instituicao_acesso_infra
# Com relação às formas de acesso aos recursos de infraestrutura oferecidos pela sua instituição (biblioteca, coordenação, orientação de assuntos acadêmicos, matrícula, etc.) durante a pandemia do COVID-19 você sentiu que: *

# Tratamento opcoes
df_infra <- data.frame(infra="", nivel =dataset.csv$acesso_infra_ies)

for (k in 1:nrow(df_infra)) {
  if(df_infra$nivel[k] == "Melhorou")            df_infra$infra[k] <- "Melhoraram"
  if(df_infra$nivel[k] == "Pioraram")            df_infra$infra[k] <- "Pioraram"
  if(df_infra$nivel[k] == "Piorou")              df_infra$infra[k] <- "Pioraram"
  if(df_infra$nivel[k] == "N/A ou Não sabe")     df_infra$infra[k] <- "Não Sabe"
  if(df_infra$nivel[k] == "Ficou mais ou menos o mesmo")  df_infra$infra[k] <- "Não Mudaram"
}  

df_infra

ies_infra <- table(df_infra$infra, exclude = "")
ies_infra

#  Melhoraram Não Mudaram    Não Sabe    Pioraram 
#      7          17           3          21 
 
pct_ies_infra <- paste(round(unname(ies_infra) / sum(unname(ies_infra)) * 100), "%")
pct_ies_infra

# "13 %" "33 %" "13 %" "40 %"

grafico_ies_infra <- pie(ies_infra,
                               edges = 200, radius = 0.8,
                               clockwise = T,
                               density = NULL, angle = 90, col = rainbow(4),
                               labels = paste(names(ies_infra), " ", ies_infra, "(", pct_ies_infra, ")"),
                               main = "Gráfico Instituiçao: Com relação às formas de acesso aos recursos de infraestrutura oferecidos pela sua instituição 
                                              (biblioteca, coordenação, orientação de assuntos acadêmicos, matrícula, etc.) 
                                              durante a pandemia do COVID-19 você sentiu que: ",
                                cex.main =0.9)

# Grafico Dados Instituicao de Ensino 2. Instituicao_reinicio
# A sua instituição já reiniciou todas as atividades presenciais no seu campus e/ou faculdade?
ies_reinicio <- table(dataset.csv$ies_reinicio, exclude = "")
ies_reinicio

# Em parte (apenas algumas atividades  presenciais retornaram)            N/A ou Não sabe 
#                14                                                            5
#  Não, ainda não retornou nenhuma atividade presencial               Sim (retornou todas as atividades presenciais) 
#           4                                                           29 

# Reduçao nome dos cursos
names(ies_reinicio) <- c("Em parte", "Não Sabe", "Não Reiniciou Presencial", "Sim Reiniciou Presencial")

pct_ies_reinicio <- paste(round(unname(ies_reinicio) / sum(unname(ies_reinicio)) * 100), "%")
pct_ies_reinicio

# "27 %" "10 %" "8 %"  "56 %"

grafico_ies_reinicio <- pie(ies_reinicio,
                         edges = 200, radius = 0.8,
                         clockwise = T,
                         density = NULL, angle = 90, col = rainbow(5),
                         labels = paste(names(ies_reinicio), " ", ies_reinicio, "(", pct_ies_reinicio, ")"),
                         main = "Gráfico Instituiçao: já reiniciou todas as atividades presenciais no seu campus e/ou faculdade?")

# Grafico 2. Instituicao ajuda_financeira
# Você recebe/recebeu alguma ajuda financeira da sua instituição educacional ou de outra organização durante a pandemia da COVID-19? *
ajuda_financeira <- table(dataset.csv$ajuda_financeira, exclude = "")
ajuda_financeira

#Não Sim 
# 44   8 

pct_ajuda_financeira <- paste(round(unname(ajuda_financeira) / sum(unname(ajuda_financeira)) * 100), "%")
pct_ajuda_financeira
#  "85 %" "15 %"

grafico_ajuda_financeira <- pie(ajuda_financeira,
                                edges = 200, radius = 0.8,
                                clockwise = T,
                                density = NULL, angle = 90, col = rainbow(2),
                                labels = paste(names(ajuda_financeira), " ", ajuda_financeira, "(", pct_ajuda_financeira, ")"),
                                main = "Grafico Instituicao Ensino : Você recebe/recebeu alguma ajuda financeira da sua instituição educacional ou 
                                            de outra organização durante a pandemia da COVID-19?",
                                cex.main=0.9)

# Grafico Dados Instituicao de Ensino 2.decisao_fechar (utilizacao de ferramenta online)
# Com relação à decisão de fechar o campus e de utilizar ferramentas online para as aulas, por conta da pandemia da COVID-19, você sentiu que as decisões na sua instituição foram tomadas: *
ies_decisao_fechar <- table(dataset.csv$decisao_fechar, exclude = "")
ies_decisao_fechar

# De forma oportuna e prudente             Muito lentamente            Muito rapidamente 
#                  40                            7                            5 

pct_ies_decisao_fechar <- paste(round(unname(ies_decisao_fechar) / sum(unname(ies_decisao_fechar)) * 100), "%")
pct_ies_decisao_fechar

# "77 %" "13 %" "10 %"" 

grafico_ies_decisao_fechar <- pie(ies_decisao_fechar,
                            edges = 200, radius = 0.8,
                            clockwise = T,
                            density = NULL, angle = 90, col = rainbow(3),
                            labels = paste(names(ies_decisao_fechar), " ", ies_decisao_fechar, "(", pct_ies_decisao_fechar, ")"),
                            main = "Gráfico Instituiçao:  Com relação à decisão de fechar o campus e de utilizar ferramentas online para as aulas, por conta da pandemia da COVID-19, 
                                você sentiu que as decisões na sua instituição foram tomadas:",
                            cex.main=0.9)


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
                         labels = paste(names(convive_risco), " ", convive_risco, "(",pct_convive_risco, ")"),
                         main = "Gráfico Pandemia Risco Relevante:Você está morando ou convivendo atualmente com alguém na faixa etária  dos 60-70 anos,e/ou que tenha algum fator de risco relevante?
                           (Fatores de risco relevantes são problemas cardíacos, diabetes, hipertensão e/ou obesidade).",
                         cex.main =0.8)

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
                             labels = paste(names(quarentena_imposta), " ", quarentena_imposta, "(",pct_quarentena_imposta,")"),
                             main = "Gráfico Pandemia Risco Relevante: Você, alguém com quem convive ou que está em sua moradia está ou esteve em quarentena imposta pela COVID-19  ?",
                             cex.main=0.8)


# Grafico 3. Pandemia_quarentena imposta
# Durante a pandemia do COVID-19, você obteve e/ou vivenciou: *
vivenciou <- table(dataset.csv$vivenciou, exclude = "")
vivenciou

# Ajuda ou assistência de pessoas desconhecidas 
#   4 
# Dificuldades devido a alterações em suas condições de vida, incluindo o fechamento de alojamentos, perda de emprego, etc. 
#   9 
# Dificuldades para viajar/se deslocar 
# 19 
# Discriminação por pessoas desconhecidas 
# 1 
# Não se aplica / Não sabe 
# 6 
# Não se aplica / Não sabe / Não se lembra 
# 13

# Tratamento nomes
names(vivenciou) <- c("Assistencia de Desconhecidos", "Alterações de Condiçoes de Vida", "Dificuldade Deslocamento", "Discriminação", "Não Sabe", "Nao se Lembra")

pct_vivenciou <- paste(round(unname(vivenciou) / sum(unname(vivenciou)) * 100), "%")
pct_vivenciou

# "8 %"  "17 %" "37 %" "2 %"  "12 %" "25 %"

par(las=1) # nomes dos eixos perpendicular
par(mar=c(5,16,1,1)+0.1)  # para aumentar a margem a esquerda 
grafico_vivenciou <- barplot(vivenciou, 
                                    main="Grafico Pandemia Vivenciou : Durante a pandemia do COVID-19, você obteve e/ou vivenciou",
                                    cex.main=0.9,
                                    las=1,  
                                    beside = TRUE,
                                    horiz=TRUE, 
                                    xlim = c(0,max(vivenciou) + 5),
                                    legend.text = paste(rownames(vivenciou)," (",vivenciou, ")"),
                                    args.legend = list("top", bty="n", cex = 0.7),
                                    col=rainbow(6),
                                    cex.axis = 0.7,  
                                    cex.names = 0.8)

text(grafico_vivenciou, x = vivenciou, label = pct_vivenciou, cex=0.7, pos=4)

par(mar=c(5,4,4,2)+0.1) # para retornar a margem default

# Grafico 3. Pandemia_acesso_servicos_saude
# Como você classifica o seu acesso aos serviços de saúde, a antes e durante a pandemia COVID-19 ? *

# Tratamento opcoes
df_saude <- data.frame(saude="", nivel =dataset.csv$acesso_servicos_saude)

for (k in 1:nrow(df_saude)) {
  if(df_saude$nivel[k] == "Melhor do que antes")            df_saude$saude[k] <- "Melhorou"
  if(df_saude$nivel[k] == "Muito pior do que antes")        df_saude$saude[k] <- "Piorou"
  if(df_saude$nivel[k] == "O mesmo de antes")               df_saude$saude[k] <- "Não Mudou"
  if(df_saude$nivel[k] == "Pior do que antes")              df_saude$saude[k] <- "Piorou"
  if(df_saude$nivel[k] == "N/A ou Não sabe")              df_saude$saude[k] <- "Não Sabe"
}  

df_saude

acesso_servicos_saude <- table(df_saude$saude, exclude = "")
acesso_servicos_saude

# Melhorou Não Mudou  Não Sabe    Piorou 
#     5        28         1        18 

pct_acesso_servicos_saude <- paste(round(unname(acesso_servicos_saude) / sum(unname(acesso_servicos_saude)) * 100), "%")
pct_acesso_servicos_saude

# "10 %" "54 %" "2 %"  "35 %"

grafico_acesso_servicos_saude <- pie(acesso_servicos_saude,
                                  edges = 200, radius = 0.8,
                                  clockwise = T,
                                  density = NULL, angle = 90, col = rainbow(4),
                                  labels = paste(names(acesso_servicos_saude), " ", acesso_servicos_saude, "(", pct_acesso_servicos_saude, ")"),
                                  main = "Grafico Pandemia Serviços Saude : Como você classifica o seu acesso aos serviços de saúde, a antes e durante a pandemia COVID-19 ? ",
                                  cex.main=0.9)

# Grafico 3. Pandemia_acesso_internet
# Como você compara o seu acesso à Internet antes e durante a pandemia da COVID-19 ? *

# Tratamento opcoes
df_internet <- data.frame(internet="", nivel =dataset.csv$acesso_internet)

for (k in 1:nrow(df_internet)) {
  if(df_internet$nivel[k] == "Melhor do que antes")            df_internet$internet[k] <- "Melhorou"
  if(df_internet$nivel[k] == "Muito melhor do que antes")      df_internet$internet[k] <- "Melhorou"
  if(df_internet$nivel[k] == "Muito pior que antes")        df_internet$internet[k] <- "Piorou"
  if(df_internet$nivel[k] == "O mesmo que antes")               df_internet$internet[k] <- "Não Mudou"
  if(df_internet$nivel[k] == "Pior que antes")              df_internet$internet[k] <- "Piorou"
  if(df_internet$nivel[k] == "N/A ou Não sabe")              df_internet$internet[k] <- "Não Sabe"
}  

df_internet

acesso_internet <- table(df_internet$internet, exclude = "")
acesso_internet

#  Melhorou Não Mudou  Não Sabe    Piorou 
#      10        34         1         7 

pct_acesso_internet <- paste(round(unname(acesso_internet) / sum(unname(acesso_internet)) * 100), "%")
pct_acesso_internet

# "19 %" "65 %" "2 %"  "13 %"

grafico_acesso_internet <- pie(acesso_internet,
                                     edges = 200, radius = 0.8,
                                     clockwise = T,
                                     density = NULL, angle = 90, col = rainbow(4),
                                     labels = paste(names(acesso_internet), " ", acesso_internet, "(", pct_acesso_internet, ")"),
                                     main = "Grafico Pandemia Acesso Internet : Como você compara o seu acesso à Internet antes e durante a pandemia da COVID-19 ?",
                                     cex.main=0.8)

# Grafico 3. Pandemia capacidade_prosseguir_estudos
# Em relação a antes e durante a pandemia da COVID-19, como você classifica sua capacidade em prosseguir seus estudos, 
# incluindo sua graduação e/ou conclusão de curso?

# Tratamento opcoes
df_estudos <- data.frame(estudos="", nivel =dataset.csv$capacidade_.socializacao)

for (k in 1:nrow(df_estudos)) {
  if(df_estudos$nivel[k] == "Melhor do que antes")            df_estudos$estudos[k] <- "Melhorou"
  if(df_estudos$nivel[k] == "Muito melhor do que antes")      df_estudos$estudos[k] <- "Melhorou"
  if(df_estudos$nivel[k] == "Muito pior do que antes")        df_estudos$estudos[k] <- "Piorou"
  if(df_estudos$nivel[k] == "O mesmo de antes")               df_estudos$estudos[k] <- "Não Mudou"
  if(df_estudos$nivel[k] == "Pior do que antes")              df_estudos$estudos[k] <- "Piorou"
  if(df_estudos$nivel[k] == "N/A ou Não Sabe")              df_estudos$estudos[k] <- "Não Sabe"
}  

df_estudos

capacidade_prosseguir_estudos<- table(df_estudos$estudos, exclude = "")
capacidade_prosseguir_estudos

# Melhorou Não Mudou    Piorou 
#      3        19        30 

pct_capacidade_prosseguir_estudos <- paste(round(unname(capacidade_prosseguir_estudos) / sum(unname(capacidade_prosseguir_estudos)) * 100), "%")
pct_capacidade_prosseguir_estudos

# "6 %"  "37 %" "58 %"

grafico_capacidade_prosseguir_estudos <- pie(capacidade_prosseguir_estudos,
                               edges = 200, radius = 0.8,
                               clockwise = T,
                               density = NULL, angle = 90, col = rainbow(3),
                               labels = paste(names(capacidade_prosseguir_estudos), " ", capacidade_prosseguir_estudos, "(", pct_capacidade_prosseguir_estudos, ")"),
                               main = "Grafico Pandemia Prosseguir Estudos : Em relação a antes e durante a pandemia da COVID-19, como você classifica sua capacidade em prosseguir seus estudos, 
                                      incluindo sua graduação e/ou conclusão de curso?",
                              cex.main=0.8)

# Grafico 3. Pandemia_capacidade_socializacao
# Em relação a antes e durante a pandemia da COVID-19 , como você classificaria a sua capacidade de socialização? 
# (Socialização aqui definida como a efetiva vivência com demais pessoas em sociedade). *

# Tratamento opcoes
df_socializacao <- data.frame(socializacao="", nivel =dataset.csv$capacidade_.socializacao)

for (k in 1:nrow(df_socializacao)) {
  if(df_socializacao$nivel[k] == "Melhor do que antes")            df_socializacao$socializacao[k] <- "Melhorou"
  if(df_socializacao$nivel[k] == "Muito pior do que antes")        df_socializacao$socializacao[k] <- "Piorou"
  if(df_socializacao$nivel[k] == "O mesmo de antes")               df_socializacao$socializacao[k] <- "Não Mudou"
  if(df_socializacao$nivel[k] == "Pior do que antes")              df_socializacao$socializacao[k] <- "Piorou"
}  

df_socializacao

capacidade_socializacao <- table(df_socializacao$socializacao, exclude = "")
capacidade_socializacao

# Melhorou Não Mudou    Piorou 
#     3        19        30 

pct_capacidade_socializacao <- paste(round(unname(capacidade_socializacao) / sum(unname(capacidade_socializacao)) * 100), "%")
pct_capacidade_socializacao

# "6 %"  "37 %" "58 %"

grafico_capacidade_socializacao <- pie(capacidade_socializacao,
                               edges = 200, radius = 0.8,
                               clockwise = T,
                               density = NULL, angle = 90, col = rainbow(3),
                               labels = paste(names(capacidade_socializacao), "-", pct_capacidade_socializacao),
                               main = "Grafico Pandemia Socializacao : como você classificaria a sua capacidade de socialização antes e durante a Pandemia? 
                                       Socialização aqui definida como a efetiva vivência com demais pessoas em sociedade",
                               cex.main=0.8)

# Grafico 3. Pandemia bem-estar_psicologico
# Em relação a antes e durante a pandemia da COVID-19, como você classificaria o seu bem-estar psicológico em geral, 
# incluindo sentimentos de ansiedade e/ou depressão? *

# Tratamento opcoes
df_bemestar <- data.frame(bemestar="", nivel =dataset.csv$bem.estar_psicologico)

for (k in 1:nrow(df_bemestar)) {
  if(df_bemestar$nivel[k] == "Melhor do que antes")            df_bemestar$bemestar[k] <- "Melhorou"
  if(df_bemestar$nivel[k] == "Muito melhor do que antes")      df_bemestar$bemestar[k] <- "Melhorou"
  if(df_bemestar$nivel[k] == "Muito pior do que antes")        df_bemestar$bemestar[k] <- "Piorou"
  if(df_bemestar$nivel[k] == "Pior do que antes")              df_bemestar$bemestar[k] <- "Piorou"
  if(df_bemestar$nivel[k] == "O mesmo de antes")               df_bemestar$bemestar[k] <- "Não Mudou"
  if(df_bemestar$nivel[k] == "N/A ou Não Sabe")              df_bemestar$bemestar[k] <- "Não Sabe"
}  

df_bemestar

bemestar_psicologico <- table(df_bemestar$bemestar, exclude = "")
bemestar_psicologico

# Melhorou Não Mudou  Não Sabe    Piorou 
#        4        12         1        35 

pct_bemestar_psicologico <- paste(round(unname(bemestar_psicologico) / sum(unname(bemestar_psicologico)) * 100), "%")
pct_bemestar_psicologico

# "8 %"  "23 %" "2 %"  "67 %"

grafico_bemestar_psicologico <- pie(bemestar_psicologico,
                                    edges = 200, radius = 0.8,
                                    clockwise = T,
                                    density = NULL, angle = 90, col = rainbow(4),
                                    labels = paste(names(bemestar_psicologico), " ", bemestar_psicologico, "(", pct_bemestar_psicologico, ")"),
                                    main = "Grafico Pandemia Bem-estar Psicologico : Em relação a antes e durante a pandemia da COVID-19, como você classificaria o seu bem-estar psicológico em geral, 
                                                    incluindo sentimentos de ansiedade e/ou depressão? *",
                                    cex.main=0.8)

# Grafico 3. Pandemia aulas
# Com relação à forma como as aulas foram ministradas durante a pandemia do COVID-19, você sente que:
# Tratamento opcoes
df_aulas <- data.frame(aulas="", nivel =dataset.csv$aulas_durante_pandemia)
df_aulas

for (k in 1:nrow(df_aulas)) {
  if(df_aulas$nivel[k] == "Melhoraram")                     df_aulas$aulas[k] <- "Melhorou"
  if(df_aulas$nivel[k] == "Pioraram")                       df_aulas$aulas[k] <- "Piorou"
  if(df_aulas$nivel[k] == "Está/É mais ou menos o mesmo")      df_aulas$aulas[k] <- "Não Mudou"
  if(df_aulas$nivel[k] == "Foram mais ou menos o mesmo")    df_aulas$aulas[k] <- "Não Mudou"
  if(df_aulas$nivel[k] == "N/A ou Não sabe")              df_aulas$aulas[k] <- "Não Sabe"
}  

df_aulas

aulas <- table(df_aulas$aulas, exclude = "")
aulas

#  Melhorou Não Mudou  Não Sabe    Piorou 
#        1        25         4        22 

pct_aulas <- paste(round(unname(aulas) / sum(unname(aulas)) * 100), "%")
pct_aulas

# "2 %"  "48 %" "8 %"  "42 %"

grafico_aulas <- pie(aulas,
                                    edges = 200, radius = 0.8,
                                    clockwise = T,
                                    density = NULL, angle = 90, col = rainbow(4),
                                    labels = paste(names(aulas), " ", aulas, "(", pct_aulas, ")"),
                                    main = "Grafico Pandemia Aula : Com relação à forma como as aulas foram ministradas durante a pandemia do COVID-19, você sente que",
                                    cex.main=0.8)

# Grafico 3. Pandemia acesso professores
# Durante a pandemia da COVID-19, você sentiu que as formas de acesso aos seus professores: *

# Tratamento opcoes
df_professores <- data.frame(professores="", nivel =dataset.csv$acesso_professores)

for (k in 1:nrow(df_professores)) {
  if(df_professores$nivel[k] == "Melhorou")                     df_professores$professores[k] <- "Melhorou"
  if(df_professores$nivel[k] == "Piorou")                       df_professores$professores[k] <- "Piorou"
  if(df_professores$nivel[k] == "Está mais ou menos o mesmo")      df_professores$professores[k] <- "Não Mudou"
  if(df_professores$nivel[k] == "Foi mais ou menos o mesmo")    df_professores$professores[k] <- "Não Mudou"
  if(df_professores$nivel[k] == "N/A ou Não sabe")              df_professores$professores[k] <- "Não Sabe"
}  

df_professores

acesso_professores <- table(df_professores$professores, exclude = "")
acesso_professores

# Melhorou Não Mudou  Não Sabe    Piorou 
#      8        13         4        27 

pct_acesso_professores <- paste(round(unname(acesso_professores) / sum(unname(acesso_professores)) * 100), "%")
pct_acesso_professores

# "15 %" "25 %" "8 %"  "52 %"

grafico_acesso_professores <- pie(acesso_professores,
                     edges = 200, radius = 0.8,
                     clockwise = T,
                     density = NULL, angle = 90, col = rainbow(4),
                     labels = paste(names(acesso_professores), " ", acesso_professores, "(", pct_acesso_professores, ")"),
                     main = "Grafico Pandemia Acesso Professores : Durante a pandemia da COVID-19, você sentiu que as formas de acesso aos seus professores:",
                     cex.main=0.9)


# Grafico 3. Pandemia espaco_fisico
# Com relação ao espaço físico (ambiente/localidade) utilizado por você, durante a pandemia de COVID-19, 
# para estudar e participar das aulas e/ou atividades, você sentiu que: *

# Tratamento das Opcoes
df_espaco <- data.frame(espaco="", nivel =dataset.csv$espaco_físico)

for (k in 1:nrow(df_espaco)) {
  if(df_espaco$nivel[k] == "Melhorou")                     df_espaco$espaco[k] <- "Melhorou"
  if(df_espaco$nivel[k] == "Piorou")                     df_espaco$espaco[k] <- "Piorou"
  if(df_espaco$nivel[k] == "É mais ou menos o mesmo")      df_espaco$espaco[k] <- "Não Mudou"
  if(df_espaco$nivel[k] == "Era mais ou menos o mesmo")    df_espaco$espaco[k] <- "Não Mudou"
  if(df_espaco$nivel[k] == "N/A ou Não sabe")              df_espaco$espaco[k] <- "Não Sabe"
  if(df_espaco$nivel[k] == "")                      df_espaco$espaco[k] <- "Não Sabe"
  
}
df_espaco

espaco_fisico <- table(df_espaco$espaco, exclude = "")
espaco_fisico

# Melhorou Não Mudou  Não Sabe    Piorou 
#       7        25         5        15 

pct_espaco_fisico <- paste(round(unname(espaco_fisico) / sum(unname(espaco_fisico)) * 100), "%")
pct_espaco_fisico

#  "13 %" "48 %" "10 %" "29 %"

grafico_espaco_fisico <- pie(espaco_fisico,
                                  edges = 200, radius = 0.8,
                                  clockwise = T,
                                  density = NULL, angle = 90, col = rainbow(4),
                                  labels = paste(names(espaco_fisico), " ", espaco_fisico, "(", pct_espaco_fisico, ")"),
                                  main = "Grafico Pandemia Espaço Fisico : Ambiente/localidade utilizado por você, durante a pandemia de COVID-19, 
                                                            para estudar e participar das aulas e/ou atividades, você sentiu que:",
                                  cex.main=0.9)

# Grafico 3. Pandemia disposicao_atividades
# Você sentiu que, durante a pandemia do COVID-19, de forma geral, sua disposição para participar das aulas/atividades, estudar e aprender: 

# Tratamento das Opcoes
df_disposicao <- data.frame(disposicao="", nivel =dataset.csv$disposicao_atividades )

for (k in 1:nrow(df_disposicao)) {
  if(df_disposicao$nivel[k] == "Aumentou")                     df_disposicao$disposicao[k] <- "Aumentou"
  if(df_disposicao$nivel[k] == "Diminuiu")                     df_disposicao$disposicao[k] <- "Diminuiu"
  if(df_disposicao$nivel[k] == "É mais ou menos o mesmo")      df_disposicao$disposicao[k] <- "Não Mudou"
  if(df_disposicao$nivel[k] == "Ficou mais ou menos a mesma")    df_disposicao$disposicao[k] <- "Não Mudou"
  if(df_disposicao$nivel[k] == "N/A ou Não sabe")              df_disposicao$disposicao[k] <- "Não Sabe"
  
}
df_disposicao

disposicao_atividades <- table(df_disposicao$disposicao, exclude = "")
disposicao_atividades

# Aumentou  Diminuiu Não Mudou  Não Sabe 
#    11        26        14         1 

pct_disposicao_atividades <- paste(round(unname(disposicao_atividades) / sum(unname(disposicao_atividades)) * 100), "%")
pct_disposicao_atividades

#  "21 %" "50 %" "27 %" "2 %"

grafico_disposicao_atividades <- pie(disposicao_atividades,
                             edges = 200, radius = 0.8,
                             clockwise = T,
                             density = NULL, angle = 90, col = rainbow(4),
                             labels = paste(names(disposicao_atividades), " ", disposicao_atividades, "(", pct_disposicao_atividades, ")"),
                             main = "Grafico Pandemia Disposicao Atividades : Você sentiu que, durante a pandemia do COVID-19, de forma geral, 
                                    sua disposição para participar das aulas/atividades, estudar e aprender: ",
                             cex.main=0.9)

# Grafico 3. Pandemia desempenho_escolar
# Você percebeu que o seu desempenho escolar durante a pandemia do COVID-19 (suas notas, aproveitamento escolar, etc.):

# Tratamento das Opcoes
df_desempenho_escolar <- data.frame(desempenho_escolar="", nivel =dataset.csv$desempenho_escolar )

for (k in 1:nrow(df_desempenho_escolar)) {
  if(df_desempenho_escolar$nivel[k] == "Aumentou")                     df_desempenho_escolar$desempenho_escolar[k] <- "Aumentou"
  if(df_desempenho_escolar$nivel[k] == "Diminuiu")                     df_desempenho_escolar$desempenho_escolar[k] <- "Diminuiu"
  if(df_desempenho_escolar$nivel[k] == "Está mais ou menos o mesmo")   df_desempenho_escolar$desempenho_escolar[k] <- "Não Mudou"
  if(df_desempenho_escolar$nivel[k] == "Foi mais ou menos o mesmo")    df_desempenho_escolar$desempenho_escolar[k] <- "Não Mudou"
  if(df_desempenho_escolar$nivel[k] == "N/A ou Não sabe")              df_desempenho_escolar$desempenho_escolar[k] <- "Não Sabe"
  
}
df_desempenho_escolar

desempenho_escolar <- table(df_desempenho_escolar$desempenho_escolar, exclude = "")
desempenho_escolar

# Aumentou  Diminuiu Não Mudou  Não Sabe 
#     14        16        20         2 

pct_desempenho_escolar <- paste(round(unname(desempenho_escolar) / sum(unname(desempenho_escolar)) * 100), "%")
pct_desempenho_escolar

#  "27 %" "31 %" "38 %" "4 %"

grafico_desempenho_escolar <- pie(desempenho_escolar,
                                     edges = 200, radius = 0.8,
                                     clockwise = T,
                                     density = NULL, angle = 90, col = rainbow(5),
                                     labels = paste(names(desempenho_escolar), " ", desempenho_escolar, "(", pct_desempenho_escolar, ")"),
                                     main = "Grafico Pandemia Desempenho Escolar : Você percebeu que o seu desempenho escolar durante a 
                                              pandemia do COVID-19 (suas notas, aproveitamento escolar, etc.):",
                                     cex.main=0.9)


# Grafico 1. Vacinado
# Você já foi vacinado contra a COVID-19 ?
vacinado <- table(dataset.csv$vacinado, exclude = "")
vacinado

names(vacinado) <- c("Sim, duas doses ou dose unica", 
                     "Sim, duas doses, dose única e reforço", 
                     "Sim, completamente duas doses ou dose unica ", 
                     "Sim, duas dose ou dose unica e doses reforço")
vacinado

#Sim, duas doses ou dose unica        Sim, duas doses, dose única e reforço Sim, completamente duas doses ou dose unica  
#                  7                                           21                                            1 
# Sim, duas dose ou dose unica e doses reforço 
#     19 

pct_vacinado <- paste(round(unname(vacinado) / sum(unname(vacinado)) * 100), "%")
pct_vacinado
#  "15 %" "44 %" "2 %"  "40 %" 

grafico_vacinado <- pie(vacinado,
                                  edges = 200, radius = 0.8,
                                  clockwise = T,
                                  density = NULL, angle = 90, col = rainbow(5),
                                  labels = paste(names(vacinado), " ", vacinado, "(", pct_vacinado, ")"),
                                  main = "Grafico Vacinação : Você já foi vacinado contra a COVID-19 ?",
                                  cex.main=1)

# Grafico 4. Financas despesas
# Com relação aos seus gastos e/ou despesas durante a pandemia da COVID-19, o que mudou para você  ? *v
# Tratamento das Opcoes
df_despesas <- data.frame(despesas="", nivel =dataset.csv$despesas )

for (k in 1:nrow(df_despesas)) {
  if(df_despesas$nivel[k] == "Aumentaram")       df_despesas$despesas[k] <- "Aumentaram"
  if(df_despesas$nivel[k] == "Diminuiram")       df_despesas$despesas[k] <- "Diminuiram"
  if(df_despesas$nivel[k] == "Foram mais ou menos os mesmos")   df_despesas$despesas[k] <- "Não Mudaram"
  if(df_despesas$nivel[k] == "São mais ou menos os mesmos")           df_despesas$despesas[k] <- "Não Mudaram"
  
}

despesas <- table(df_despesas$despesas, exclude = "")
despesas

# Aumentaram  Diminuiram Não Mudaram 
#     17          14          21 

pct_despesas <- paste(round(unname(despesas) / sum(unname(despesas)) * 100), "%")
pct_despesas

#  "33 %" "27 %" "40 %" 

grafico_despesas <- pie(despesas,
                                  edges = 200, radius = 0.8,
                                  clockwise = T,
                                  density = NULL, angle = 90, col = rainbow(5),
                                  labels = paste(names(despesas), " ", despesas, "(", pct_despesas, ")"),
                                  main = "Grafico Situacao Financeira : Com relação aos seus gastos e/ou despesas durante a pandemia da COVID-19, o que mudou para você  ?",
                                  cex.main=0.8)


# Grafico 4. Financas renda_financeira
# Durante a pandemia da COVID-19 houve alguma alteração com relação à sua renda financeira? *
renda_financeira <- table(dataset.csv$renda_financeira, exclude = "")
renda_financeira

#                   Aumentou                   Diminuiu Está mais ou menos a mesma N/A ou Não quero responder 
#                          3                         25                         23                          1 

pct_renda_financeira <- paste(round(unname(renda_financeira) / sum(unname(renda_financeira)) * 100), "%")
pct_renda_financeira

#  "6 %"  "48 %" "44 %" "2 %" 

grafico_renda_financeira <- pie(renda_financeira,
                        edges = 200, radius = 0.8,
                        clockwise = T,
                        density = NULL, angle = 90, col = rainbow(4),
                        labels = paste(names(renda_financeira), " ", renda_financeira, "(", pct_renda_financeira, ")"),
                        main = "Grafico Situacao Financeira : Durante a pandemia da COVID-19 houve alguma alteração com relação à sua renda financeira?",
                        cex.main=0.8)

# Grafico 4. Financas nivel_endividamento
# Com relação às suas dívidas (nível de endividamento), durante a pandemia da COVID-19, elas: *

# Tratamento das Opcoes
df_endividamento <- data.frame(endividamento="", nivel =dataset.csv$nivel_endividamento )

for (k in 1:nrow(df_endividamento)) {
  if(df_endividamento$nivel[k] == "Aumentaram")       df_endividamento$endividamento[k] <- "Aumentaram"
  if(df_endividamento$nivel[k] == "Diminuiram")       df_endividamento$endividamento[k] <- "Diminuiram"
  if(df_endividamento$nivel[k] == "Estão mais ou menos as mesmas")   df_endividamento$endividamento[k] <- "Não Mudaram"
  if(df_endividamento$nivel[k] == "N/A ou Não sabe")           df_endividamento$endividamento[k] <- "Não Sabe"
  if(df_endividamento$nivel[k] == "São mais ou menos as mesmas")         df_endividamento$endividamento[k] <- "Não Mudaram"
}

nivel_endividamento <- table(df_endividamento$endividamento, exclude = "")
nivel_endividamento

# Aumentaram  Diminuiram Não Mudaram    Não Sabe 
#        7           6          36           3 

pct_nivel_endividamento <- paste(round(unname(nivel_endividamento) / sum(unname(nivel_endividamento)) * 100), "%")
pct_nivel_endividamento

#  "13 %" "12 %" "69 %" "6 %" 

grafico_nivel_endividamento <- pie(nivel_endividamento,
                                edges = 200, radius = 0.8,
                                clockwise = T,
                                density = NULL, angle = 90, col = rainbow(5),
                                labels = paste(names(nivel_endividamento), " ", nivel_endividamento, "(", pct_nivel_endividamento, ")"),
                                main = "Grafico Finanças : Com relação às suas dívidas (nível de endividamento), durante a pandemia da COVID-19, elas:",
                                cex.main=1)



# Grafico 4. Financas despesas_cresceram
# Quais das despesas do seu dia-a-dia aqui relacionadas você acredita que cresceram no ano/semestre, 
# após o pico da pandemia ? (Marque todas que achar necessárias)

# Relacionadas com saúde
# Viagens/deslocamentos
# Transporte urbano
# Aluguel
# Internet
# Alimentação
# Outras (não relacionadas)

# selecionando todos os casos da variável estilo de uso
despesas_cresceram <- dataset.xlsx$`despesas_ cresceram`
despesas_cresceram

# Relacionadas com saúde
# Viagens/deslocamentos
# Transporte urbano
# Aluguel
# Internet
# Alimentação
# Outras (não relacionadas)

relacionadas_saude <- length(na.omit(str_match(despesas_cresceram, "Relacionadas com saúde")))
viagens_deslocamentos <- length(na.omit(str_match(despesas_cresceram, "Viagens/deslocamentos")))
transporte_urbano <- length(na.omit(str_match(despesas_cresceram, "Transporte urbano")))
aluguel <- length(na.omit(str_match(despesas_cresceram, "Aluguel")))
internet <- length(na.omit(str_match(despesas_cresceram, "Internet")))
alimentacao <- length(na.omit(str_match(despesas_cresceram, "Alimentação")))  
outras <- length(na.omit(str_match(despesas_cresceram, "Outras")))

casos_despesas_cresceram <- c(relacionadas_saude, viagens_deslocamentos, transporte_urbano, aluguel, internet, alimentacao, outras)
names(casos_despesas_cresceram) <- c("Relac.Saúde", "Deslocamento", "Transp.Urbano", "Aluguel", "Internet", "Alimentação", "Outras")
casos_despesas_cresceram

# Relac. saúde   Deslocamento Transp. urbano        Aluguel       Internet    Alimentação         Outras 
#       16             21             22             18             18             37             15 

pct_despesas_cresceram <- paste0(round(unname(casos_despesas_cresceram) / sum(unname(casos_despesas_cresceram)) * 100,0), "%")
pct_despesas_cresceram  
#[1] "11%" "14%" "15%" "12%" "12%" "25%" "10%"

# Grafico
par(las=1) # nomes dos eixos perpendicular
par(mar=c(5,10,7,1)+0.1)  # para aumentar a margem a esquerda 
grafico_despesas_cresceram <- barplot(casos_despesas_cresceram, 
                               main="Grafico Finanças : Quais das despesas do seu dia-a-dia aqui relacionadas você acredita que cresceram no ano/semestre, 
                                                 após o pico da pandemia ? (Marque todas que achar necessárias)",
                               las=1,  
                               beside = TRUE,
                               horiz=TRUE, 
                               xlim = c(0,max(casos_despesas_cresceram) + 12),
                               col=rainbow(7),
                               cex.axis = 0.6,  
                               cex.names = 0.8,
                               cex.main = 0.9)

text(grafico_despesas_cresceram, x = casos_despesas_cresceram, label = paste(casos_despesas_cresceram, "(", pct_despesas_cresceram, ")"), cex=0.8, pos=4)

par(mar=c(5,4,4,2)+0.1) # para retornar a margem default


# Grafico 5. Ansiedade nivel_ansiedade
# Comparado a antes da pandemia do COVID-19, qual o seu nível de ansiedade com relação ao que poderá acontecer com você nos próximos meses? *

# Tratamento das Opcoes (Melhorou, Piorou ou Nada Mudou)
df_nivel_ansiedade <- data.frame(nivel_ansiedade="", nivel =dataset.csv$nivel_ansiedade )

for (k in 1:nrow(df_nivel_ansiedade)) {
  if(df_nivel_ansiedade$nivel[k] == "Melhor do que antes")       df_nivel_ansiedade$nivel_ansiedade[k] <- "Melhorou"
  if(df_nivel_ansiedade$nivel[k] == "Muito melhor do que antes") df_nivel_ansiedade$nivel_ansiedade[k] <- "Melhorou"
  if(df_nivel_ansiedade$nivel[k] == "Muito pior do que antes")   df_nivel_ansiedade$nivel_ansiedade[k] <- "Piorou"
  if(df_nivel_ansiedade$nivel[k] == "Pior do que antes")         df_nivel_ansiedade$nivel_ansiedade[k] <- "Piorou"
  if(df_nivel_ansiedade$nivel[k] == "O mesmo de antes")          df_nivel_ansiedade$nivel_ansiedade[k] <- "Nada Mudou"
  if(df_nivel_ansiedade$nivel[k] == "N/A ou Não Sabe")           df_nivel_ansiedade$nivel_ansiedade[k] <- "Não Sabe"
}


nivel_ansiedade <- table(df_nivel_ansiedade$nivel_ansiedade, exclude = "")
nivel_ansiedade

#Melhorou Nada Mudou   Não Sabe     Piorou 
#       5         17          2         28 

pct_nivel_ansiedade <- paste(round(unname(nivel_ansiedade) / sum(unname(nivel_ansiedade)) * 100), "%")
pct_nivel_ansiedade
#  "10 %" "33 %" "4 %"  "54 %"

grafico_nivel_ansiedade <- pie(nivel_ansiedade,
                                   edges = 200, radius = 0.8,
                                   clockwise = T,
                                   density = NULL, angle = 90, col = rainbow(6),
                                   labels = paste(names(nivel_ansiedade), " ", nivel_ansiedade, "(", pct_nivel_ansiedade, ")"),
                                   main = "Grafico Ansiedade : Comparado a antes da pandemia do COVID-19, qual o seu nível de ansiedade 
                                                      com relação ao que poderá acontecer com você nos próximos meses? ",
                                   cex.main=1)

# Grafico 5. Ansiedade ansiedade_planejamento
# Comparado a antes da pandemia do COVID-19, qual o seu nível de ansiedade com relação ao seu planejamento pessoal (férias, viagens, feriados, etc). 

# Tratamento das Opcoes (Melhorou, Piorou ou Nada Mudou)
df_ansiedade_planejamento <- data.frame(ansiedade_planejamento="", aplan =dataset.csv$ansiedade_planejamento )


for (k in 1:nrow(df_ansiedade_planejamento)) {
  if(df_ansiedade_planejamento$aplan[k] == "Melhor do que antes")       df_ansiedade_planejamento$ansiedade_planejamento[k] <- "Melhorou"
  if(df_ansiedade_planejamento$aplan[k] == "Muito melhor do que antes") df_ansiedade_planejamento$ansiedade_planejamento[k] <- "Melhorou"
  if(df_ansiedade_planejamento$aplan[k] == "Muito pior do que antes")   df_ansiedade_planejamento$ansiedade_planejamento[k] <- "Piorou"
  if(df_ansiedade_planejamento$aplan[k] == "Pior do que antes")         df_ansiedade_planejamento$ansiedade_planejamento[k] <- "Piorou"
  if(df_ansiedade_planejamento$aplan[k] == "O mesmo de antes")         df_ansiedade_planejamento$ansiedade_planejamento[k] <- "Nada Mudou"
}

ansiedade_planejamento <- table(df_ansiedade_planejamento$ansiedade_planejamento, exclude = "")
ansiedade_planejamento

#   Melhorou Nada Mudou     Piorou 
#          9         13         30 

pct_ansiedade_planejamento <- paste(round(unname(ansiedade_planejamento) / sum(unname(ansiedade_planejamento)) * 100), "%")
pct_ansiedade_planejamento
#  "17 %" "25 %" "58 %"

grafico_ansiedade_planejamento <- pie(ansiedade_planejamento,
                               edges = 200, radius = 0.8,
                               clockwise = T,
                               density = NULL, angle = 90, col = rainbow(5),
                               labels = paste(names(ansiedade_planejamento), " ", ansiedade_planejamento, "(",pct_ansiedade_planejamento, ")"),
                               main = "Grafico Ansiedade : Comparado a antes da pandemia do COVID-19, qual o seu nível de ansiedade com relação ao seu 
                                          planejamento pessoal (férias, viagens, feriados, etc). ",
                               cex.main=0.9)

# Grafico 5. Ansiedade ansiedade_longo_prazo
# Comparado a antes da pandemia do COVID-19, qual o seu nível de ansiedade com relação aos planos de longo prazo (sua formatura, o término do curso atual, novos projetos etc.)

# Tratamento das Opcoes (Melhorou, Piorou ou Nada Mudou)
df_ansiedade_longo_prazo <- data.frame(ansiedade_longo_prazo="", alp=dataset.csv$ansiedade_longo_prazo )

for (k in 1:nrow(df_ansiedade_longo_prazo)) {
  if(df_ansiedade_longo_prazo$alp[k] == "Melhor do que antes")       df_ansiedade_longo_prazo$ansiedade_longo_prazo[k] <- "Melhorou"
  if(df_ansiedade_longo_prazo$alp[k] == "Muito melhor do que antes") df_ansiedade_longo_prazo$ansiedade_longo_prazo[k] <- "Melhorou"
  if(df_ansiedade_longo_prazo$alp[k] == "Muito pior do que antes")   df_ansiedade_longo_prazo$ansiedade_longo_prazo[k] <- "Piorou"
  if(df_ansiedade_longo_prazo$alp[k] == "Pior do que antes")         df_ansiedade_longo_prazo$ansiedade_longo_prazo[k] <- "Piorou"
  if(df_ansiedade_longo_prazo$alp[k] == "O mesmo de antes")         df_ansiedade_longo_prazo$ansiedade_longo_prazo[k] <- "Nada Mudou"
}

ansiedade_longo_prazo <- table(df_ansiedade_longo_prazo$ansiedade_longo_prazo, exclude = "")
ansiedade_longo_prazo

# Melhorou Nada Mudou     Piorou 
#        5         19         28 

pct_ansiedade_longo_prazo <- paste(round(unname(ansiedade_longo_prazo) / sum(unname(ansiedade_longo_prazo)) * 100), "%")
pct_ansiedade_longo_prazo
#  "10 %" "37 %" "54 %"

grafico_ansiedade_longo_prazo <- pie(ansiedade_longo_prazo,
                                      edges = 200, radius = 0.8,
                                      clockwise = T,
                                      density = NULL, angle = 90, col = rainbow(5),
                                      labels = paste(names(ansiedade_longo_prazo), " ", ansiedade_longo_prazo, "(", pct_ansiedade_longo_prazo, ")"),
                                      main = "Grafico Ansiedade : Comparado a antes da pandemia do COVID-19, qual o seu nível de ansiedade com relação aos 
                                        planos de longo prazo (sua formatura, o término do curso atual, novos projetos etc.)",
                                      cex.main=0.9)


# Nuvem de Palavras
#Load the packages
if(!"wordcloud" %in% installed.packages()) install.packages("wordcloud")
library(wordcloud)
if(!"wordcloud2" %in% installed.packages()) install.packages("wordcloud2")
library(wordcloud2)
if(!"RColorBrewer" %in% installed.packages()) install.packages("RColorBrewer")
library(RColorBrewer)
if(!"tm" %in% installed.packages()) install.packages("tm")
library(tm)
# Para utilizar o comando "pipe" (%>%) ou operador "tee pipe" (%T>%) , pode-se "carregar" o pacote magrittr
if(!"magrittr" %in% installed.packages()) install.packages("magrittr")
library(magrittr)



# Nuvem de Palavras 1: situacao_durante-pandemia
# Ajude-nos a compreender o conjunto e a diversidade da situação durante a pandemia da COVID-19 na sua instituição, 
# compartilhando aqui qualquer informação adicional que julgar importante. (Escreva até 250 palavras).

#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dataset.csv$situação_durante_pandemia)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
# Uma matriz de termos de documento é uma matriz matemática que descreve a frequência 
# dos termos que ocorrem em uma coleção de documentos.
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,      
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Nuvem de Palavras 2: qualidade_de_vida 
# Ajude-nos a compreender a variedade e a diversidade de experiências com relação à sua qualidade de vida, 
# compartilhando aqui qualquer outra informação que julgar importante. 

#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dataset.csv$qualidade_de_vida)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
# Uma matriz de termos de documento é uma matriz matemática que descreve a frequência 
# dos termos que ocorrem em uma coleção de documentos.
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,      
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Nuvem de Palavras 3: dificuldades_academicas
# Por favor, ajude-nos a entender melhor a variedade e a diversidade de suas dificuldades acadêmicas, fruto da pandemia de COVID-19, 
# compartilhando aqui qualquer outra informação que julgar importante. 

#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dataset.csv$dificuldades_academicas)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
# Uma matriz de termos de documento é uma matriz matemática que descreve a frequência 
# dos termos que ocorrem em uma coleção de documentos.
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,      
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Nuvem de Palavras 4: dificuldades_financeiras
# Por favor, ajude-nos a entender a variedade e diversidade das suas dificuldades financeiras atuais, 
# compartilhando aqui qualquer outra informação que julgar pertinente. 

#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dataset.csv$dificuldades_financeiras)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
# Uma matriz de termos de documento é uma matriz matemática que descreve a frequência 
# dos termos que ocorrem em uma coleção de documentos.
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,      
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Nuvem de Palavras 5: ies_positivo
# Por favor, use este espaço para nos dizer o que sua instituição fez de positivo 
# em resposta à pandemia:  Reconheço que minha instituição fez ...

#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dataset.csv$ies_positivo)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
# Uma matriz de termos de documento é uma matriz matemática que descreve a frequência 
# dos termos que ocorrem em uma coleção de documentos.
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,      
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Nuvem de Palavras 6: ies_melhorar
# Por favor, use este espaço para nos dizer em quais aspectos sua instituição poderia melhorar 
# em resposta à pandemia:  Gostaria que minha instituição tivesse feito ... 

#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dataset.csv$ies_.melhorar)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
# Uma matriz de termos de documento é uma matriz matemática que descreve a frequência 
# dos termos que ocorrem em uma coleção de documentos.
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,      
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Nuvem de Palavras 7: ies_ajudar
# Por favor, use este espaço para nos dizer se há ou haveria alguma ação específica que a instituição 
# poderia realizar para ajudar ainda mais em resposta à pandemia:  Além do que foi feito, ajudaria muito se minha instituição ...

#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dataset.csv$ies_ajudar)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
# Uma matriz de termos de documento é uma matriz matemática que descreve a frequência 
# dos termos que ocorrem em uma coleção de documentos.
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,      
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Nuvem de Palavras 8: detalhes_finais
# Agradecemos seu tempo e sua paciência para responder a essas questões! Portanto, antes de encerrar e enviar suas respostas, você poderia, se assim desejar,  nos contar de forma mais detalhada e livre sua experiência com a COVID-19. Por favor,
# não inclua nenhuma informações de identificação, como seu nome, seu e-mail e/ou sua localização.

#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dataset.csv$detalhes_finais)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
# Uma matriz de termos de documento é uma matriz matemática que descreve a frequência 
# dos termos que ocorrem em uma coleção de documentos.
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,      
          max.words=100, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

