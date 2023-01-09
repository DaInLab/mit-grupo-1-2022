#teste grupo 1 
author: "liviaibarcellos"

library(readxl)
COVID19IES <- read_excel("dados/COVID19IES.xlsx")
View(COVID19IES)    

# Instalando biblioteca readxl para importar para o R arquivos com extensão .xls ou .xlsx.
if (!("readxl") %in% installed.packages()) install.packages("readxl")
# Carregando a biblioteca
library(readxl)
COVID19IES <- read_excel("dados/COVID19IES.xlsx")

View(COVID19IES)
head(COVID19IES)

#analizar e vizualizar os dados
coluna <- COVID19IES$idade
install.packages("ggplot2")
library(ggplot2)

ggplot(data = COVID19IES, aes(x = idade)) + geom_histogram()


hist(coluna$idade)

#importar e vizualizar os dados
COVID19IES <- read_excel("dados/COVID19IES.xlsx", skip = 1, col_names = c ("data/hora", "idade", "gênero", "situacao_conjugal" , "situacao_empregaticia" , "estado_reside" , "ies",	"nome_curso",	"nível_ensino",	"data_inicio_ curso", "tipo_ies",	"local_estudante", 	"migrou_virtual", "ies_fechou_dorm", 	"situação_durante_pandemia", 	"data_fechamento", 	"residencia_atual",	"moradia_atual_permanente", 	"morando_ com",	 "convive_risco_relevante",	"quarentena_imposta",	"vivenciou", "acesso_servicos_saude",	"acesso_internet",	"capacidade_prosseguir_estudos",	"capacidade_ socializacao",  	"bem-estar_psicologico",	"qualidade_de_vida",	"aulas_durante_pandemia",	"acesso_professores",	"acesso_infra_ies",	"espaco_físico",	"disposicao_atividades",	"desempenho_escolar",	"ies_reinicio",	"data_reinico",	"vacinado",	"dificuldades_academicas", "despesas",	"renda_financeira",	"ajuda_financeira",	"nivel_endividamento", "despesas_ cresceram",	"dificuldades_financeiras",	 "decisao_fechar", 	"ies_positivo",	"ies_ melhorar",	"ies_ajudar", "nivel_ansiedade",	"ansiedade_planejamento", "ansiedade_longo_prazo",	"detalhes_finais"), col_types = c ("numeric", "numeric", "text", "text", "text", "text", "text", "text", "numeric","text", "text", "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))

dados <- read.csv("dados/COVID19IES.xlsx")
coluna <- dados$nome_da_coluna

          