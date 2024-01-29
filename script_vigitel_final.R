##  Dados Vigitel

# Limpando todos os objetos atualmente ativos no Ambiente (Environment)

rm(list=ls(all=T))

# Definindo diretorio de trabalho

setwd("S:/Meu Drive")

############################ CARREGANDO BASE DE DADOS #########################
# Carregando os pacotes necessarios
if(!require(dplyr)){install.packages("dplyr")};library(dplyr)
if(!require(plyr)){install.packages("plyr")};library(plyr)
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)
if(!require(bigrquery)){install.packages("bigrquery")};library(bigrquery)
if(!require(survey)){install.packages("survey")};library(survey)
if(!require(odbc)){install.packages("odbc")};library(odbc)
if(!require(srvyr)){install.packages("srvyr")};library(srvyr)
if(!require(magrittr)){install.packages("magrittr")};library(magrittr)

# Configurando a conexao com o BigQuery

con_cord <- DBI::dbConnect(odbc::odbc(),
                           Driver         = "",
                           Catalog        = "",
                           Email          = "",
                           KeyFilePath    = "",
                           OAuthMechanism = 0)

# Resumo dos dados

tab_fato_vigitel <- dbFetch(dbSendQuery(con_cord,"SELECT * FROM ``.``.``"))

str(tab_fato_vigitel)
summary(tab_fato_vigitel)

############################## PRÉ-PROCESSAMENTO ##############################

# Para utilizar os pesos, é necessário antes criar um design amostral para o banco de dados
survey.vigitel <- svydesign(ids=~1, data=tab_fato_vigitel,
                     weights=tab_fato_vigitel$pesorake)

# Verificando Colunas
table(tab_fato_vigitel$obesid_i)# 0 = nao 1 = sim
table(tab_fato_vigitel$ano) #2006 a 2023, com exceção de 2022
table(tab_fato_vigitel$sexo) #1 = homem 2 = mulher
table(tab_fato_vigitel$faixaeta) # 1 = 18 a 24 , 2 = 25 a 34 anos , 3 = 35 a 44 anos,4 = 45 a 54 anos, 5 = 55 a 64 anos, 6 = 65 anos e mais
table(tab_fato_vigitel$cidade) # 1 = aracaju, 2 = belem, 3 =	belo horizonte, 4	= boa vista, 5	= campo grande, 6	= cuiaba, 7	= curitiba, 8	= florianopolis, 9	= fortaleza, 10	= goiania, 11	= joao pessoa, 12	= macapa, 13 = maceio, 14 = manaus, 15 =	natal, 16	= palmas, 17	= porto alegre, 18	= porto velho, 19	= recife, 20	= rio branco, 21	= rio de janeiro, 22	= salvador, 23	= sao luis, 24	= sao paulo, 25	= teresina, 26	= vitoria, 27	= distrito federal
table(tab_fato_vigitel$cor_raca, useNA = "always")
table(tab_fato_vigitel$escolar, useNA = "always") #1 = curso primário, 2 = admissão, 3 = curso ginasial ou ginásio, 4 = 1º grau ou fundamental ou supletivo de 1º grau, 5 = 2º grau ou colégio ou técnico ou normal ou científico científico ou ensino médio ou supletivo de 2º grau, 6 = 3º grau ou curso superior, 7= pós-graduação (especialização, mestrado, doutorado), 8 = nunca estudou, 777 = não sabe, 888 = não quis responder
table(tab_fato_vigitel$classifica_imc_i)

#Transformando Dados
# Faixa Etária
nivel_faixaeta <- c("18 a 24 anos", "25 a 34 anos", "35 a 44 anos", "45 a 54 anos", "55 a 64 anos",
                    "65 anos e mais")

tab_fato_vigitel$faixaeta <- factor(tab_fato_vigitel$faixaeta, levels = 1:6, labels = nivel_faixaeta)
tab_fato_vigitel$faixaeta <- as.character(tab_fato_vigitel$faixaeta)

# Cidades
nivel_cidade <- c(
  "Aracaju", "Belém", "Belo Horizonte", "Boa Vista", "Campo Grande",
  "Cuiabá", "Curitiba", "Florianópolis", "Fortaleza", "Goiânia",
  "João Pessoa", "Macapá", "Maceió", "Manaus", "Natal", "Palmas",
  "Porto Alegre", "Porto Velho", "Recife", "Rio Branco", "Rio de Janeiro",
  "Salvador", "São Luís", "São Paulo", "Teresina", "Vitória", "Distrito Federal"
)

tab_fato_vigitel$cidade <- factor(tab_fato_vigitel$cidade, levels = 1:27, labels = nivel_cidade)
tab_fato_vigitel$cidade <- as.character(tab_fato_vigitel$cidade)

# Cor/Raça
tab_fato_vigitel <- tab_fato_vigitel %>%
  mutate(cor_raca = case_when(
    cor_raca %in% c('não sabe', 'não quis informar', 'Outros') ~ 'Ignorado',
    cor_raca == 'amarela' ~ 'Amarela',
    cor_raca == 'branca' ~ 'Branca',
    cor_raca == 'indigena' ~ 'Indígena',
    cor_raca == 'parda' ~ 'Parda',
    cor_raca == 'preta' ~ 'Preta',
    TRUE ~ cor_raca
  )) %>%
  mutate(cor_raca = if_else(is.na(cor_raca), 'Ignorado', cor_raca))

# Transformando Dados
tab_fato_vigitel <- tab_fato_vigitel %>%
  mutate(escolar = case_when(
    escolar %in% c('1', '2', '3', '4') ~ 'Fundamental',
    escolar == '5' ~ 'Ensino Médio',
    escolar == '6' ~ 'Curso Superior',
    escolar == '7' ~ 'Pós Graduação',
    escolar == '8' ~ 'Sem Instrução',
    escolar %in% c('9', '10', '777','888') ~ 'Ignorado',
    TRUE ~ as.character(escolar)
  )) %>%
  mutate(escolar = if_else(is.na(escolar), 'Ignorado', escolar))

# Estado Nutricional
tab_fato_vigitel$classifica_imc_i <- recode(tab_fato_vigitel$classifica_imc_i,
                                            'Peso normal ou adequado' = 'Eutrofia',
                                            'Sem Classificação' = 'Ignorado',
                                            'Peso baixo' = 'Peso_Baixo'
                                            # Adicione mais recodificações conforme necessário
)

# Criando colunas binárias para cada estado nutricional
tab_fato_vigitel <- tab_fato_vigitel %>%
  mutate(sobrepeso = ifelse(classifica_imc_i == 'Sobrepeso', 1, 0),
         eutrofia = ifelse(classifica_imc_i == 'Eutrofia', 1, 0),
         obesidade_grau_1 = ifelse(classifica_imc_i == 'Obesidade Grau I', 1, 0),
         obesidade_grau_2 = ifelse(classifica_imc_i == 'Obesidade Grau II', 1, 0),
         obesidade_grau_3 = ifelse(classifica_imc_i == 'Obesidade Grau III', 1, 0),
         peso_baixo = ifelse(classifica_imc_i == 'Peso_Baixo', 1, 0),
         ignorado = ifelse(classifica_imc_i == 'Ignorado', 1, 0))


################################## OBESIDADE ##################################
##################################### ano #####################################
# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Criar o nome da variável para o subset
  subset_name <- paste("vig.", tolower(ano_atual), sep="")
  
  # Criar o subset para cada ano
  assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual))
  
  # Criar o nome da variável para o objeto de design de pesquisa
  survey_name <- paste("survey.", tolower(ano_atual), sep="")
  
  # Criar o objeto de design de pesquisa para cada ano
  assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                weights=get(subset_name)$pesorake))
  
  # Criar o nome da variável para a tabela ponderada por obesidade
  obesidade_name <- paste("obesidade.", tolower(ano_atual), sep="")
  
  # Calcular a tabela ponderada por obesidade para cada ano
  obesid_ano <- svytable(~obesid_i, design=get(survey_name)) %>%
    prop.table() %>%
    multiply_by(100) %>%
    round(digits=2) %>%
    as.data.frame()
  
  # Adicionar o nome do ano ao dataframe
  obesid_ano$ano <- ano_atual
  
  # Salvar o resultado na lista
  lista_anos[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesid_ano
}

# Combinar todos os resultados de cada ano em um dataframe
obesid_anos_df_total <- do.call(rbind, lista_anos)

# Adicionar nomes de coluna se o número de colunas for adequado
colnames(obesid_anos_df_total) <- c("Obesidade", "Obesidade por Ano (%)", "Ano")

# Exibir a tabela total
print(obesid_anos_df_total)

##################################### sexo ####################################
# Vetor de sexo
sexo <- unique(tab_fato_vigitel$sexo)

# Lista para armazenar os nomes das sexo
lista_sexo <- character(length(sexo))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_sexo <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada sexo
  resultados_sexo <- list()
  
  # Loop para cada sexo
  for (sexo_nome in sexo) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada sexo e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & sexo == sexo_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada sexo e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da sexo
    lista_sexo[sexo_nome] <- sexo_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidade
    obesidade_name <- paste("obesidade.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidade para cada sexo e ano
    obesid_sexo <- svytable(~obesid_i, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da sexo ao dataframe
    obesid_sexo$sexo <- sexo_nome
    
    # Salvar o resultado na lista
    resultados_sexo[[sexo_nome]] <- obesid_sexo
  }
  
  # Combinar todos os resultados de cada sexo em um dataframe para o ano atual
  obesid_sexo_df <- do.call(rbind, resultados_sexo)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesid_sexo_df) <- c("Obesidade", "Obesidade por sexo (%)", "sexo")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_sexo[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesid_sexo_df
}

# Unir todas as tabelas em uma única tabela
obesid_sexo_df_total <- do.call(rbind, lista_anos_sexo)

#Acrescentando a coluna de Ano no df
obesid_sexo_df_total$Ano <- rep(rep(anos, each = 4), length.out = nrow(obesid_sexo_df_total))

# Exibir a tabela total
print(obesid_sexo_df_total)

################################## faixa etária ###############################
# Vetor de faixas etárias
faixaeta <- unique(tab_fato_vigitel$faixaeta)

# Lista para armazenar os nomes das faixas etárias
lista_faixaeta <- character(length(faixaeta))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_faixaeta <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada faixa etária
  resultados_faixaeta <- list()
  
  # Loop para cada faixa etária
  for (faixaeta_nome in faixaeta) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada faixaeta e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & faixaeta == faixaeta_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada faixaeta e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da faixaeta
    lista_faixaeta[faixaeta_nome] <- faixaeta_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidade
    obesidade_name <- paste("obesidade.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidade para cada faixaeta e ano
    obesid_faixaeta <- svytable(~obesid_i, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da faixaeta ao dataframe
    obesid_faixaeta$faixaeta <- faixaeta_nome
    
    # Salvar o resultado na lista
    resultados_faixaeta[[faixaeta_nome]] <- obesid_faixaeta
  }
  
  # Combinar todos os resultados de cada faixaeta em um dataframe para o ano atual
  obesid_faixaeta_df <- do.call(rbind, resultados_faixaeta)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesid_faixaeta_df) <- c("Obesidade", "Obesidade por Faixaeta (%)", "Faixaeta")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_faixaeta[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesid_faixaeta_df
}

# Unir todas as tabelas em uma única tabela
obesid_faixaeta_df_total <- do.call(rbind, lista_anos_faixaeta)

#Acrescentando a coluna de Ano no df
obesid_faixaeta_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesid_faixaeta_df_total))

# Exibir a tabela total
print(obesid_faixaeta_df_total)

#################################### cidades ##################################
# Vetor de cidades
cidades <- unique(tab_fato_vigitel$cidade)

# Lista para armazenar os nomes das cidades
lista_cidades <- character(length(cidades))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cidades <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cidade
  resultados_cidades <- list()
  
  # Loop para cada cidade
  for (cidade_nome in cidades) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cidade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cidade == cidade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cidade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cidade
    lista_cidades[cidade_nome] <- cidade_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidade
    obesidade_name <- paste("obesidade.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidade para cada cidade e ano
    obesid_cidades <- svytable(~obesid_i, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cidade ao dataframe
    obesid_cidades$cidade <- cidade_nome
    
    # Salvar o resultado na lista
    resultados_cidades[[cidade_nome]] <- obesid_cidades
  }
  
  # Combinar todos os resultados de cada cidade em um dataframe para o ano atual
  obesid_cidades_df <- do.call(rbind, resultados_cidades)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesid_cidades_df) <- c("Obesidade", "Obesidade por Cidade (%)", "Cidade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cidades[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesid_cidades_df
}

# Unir todas as tabelas em uma única tabela
obesid_cidades_df_total <- do.call(rbind, lista_anos_cidades)

#Acrescentando a coluna de Ano no df
obesid_cidades_df_total$Ano <- rep(rep(anos, each = 54), length.out = nrow(obesid_cidades_df_total))

# Exibir a tabela total
print(obesid_cidades_df_total)

#################################### cor/raça #################################
# Vetor de cor_raca
cor_raca <- unique(tab_fato_vigitel$cor_raca)

# Lista para armazenar os nomes das cor_raca
lista_cor_raca <- character(length(cor_raca))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cor_raca <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cor_raca
  resultados_cor_raca <- list()
  
  # Loop para cada cor_raca
  for (cor_raca_nome in cor_raca) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cor_raca e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cor_raca == cor_raca_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cor_raca e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cor_raca
    lista_cor_raca[cor_raca_nome] <- cor_raca_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidade
    obesidade_name <- paste("obesidade.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidade para cada cor_raca e ano
    obesid_cor_raca <- svytable(~obesid_i, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cor_raca ao dataframe
    obesid_cor_raca$cor_raca <- cor_raca_nome
    
    # Salvar o resultado na lista
    resultados_cor_raca[[cor_raca_nome]] <- obesid_cor_raca
  }
  
  # Combinar todos os resultados de cada cor_raca em um dataframe para o ano atual
  obesid_cor_raca_df <- do.call(rbind, resultados_cor_raca)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesid_cor_raca_df) <- c("Obesidade", "Obesidade por cor_raca (%)", "cor_raca")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cor_raca[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesid_cor_raca_df
}

# Unir todas as tabelas em uma única tabela
obesid_cor_raca_df_total <- do.call(rbind, lista_anos_cor_raca)

#Acrescentando a coluna de Ano no df
obesid_cor_raca_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesid_cor_raca_df_total))

# Exibir a tabela total
print(obesid_cor_raca_df_total)

################################## escolaridade ###############################
# Vetor de escolaridade
escolaridade <- unique(tab_fato_vigitel$escolar)

# Lista para armazenar os nomes das escolaridade
lista_escolaridade <- character(length(escolaridade))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_escolaridade <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada escolaridade
  resultados_escolaridade <- list()
  
  # Loop para cada escolaridade
  for (escolaridade_nome in escolaridade) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada escolaridade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & escolar == escolaridade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada escolaridade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da escolaridade
    lista_escolaridade[escolaridade_nome] <- escolaridade_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidade
    obesidade_name <- paste("obesidade.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidade para cada escolaridade e ano
    obesid_escolaridade <- svytable(~obesid_i, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da escolaridade ao dataframe
    obesid_escolaridade$escolar <- escolaridade_nome
    
    # Salvar o resultado na lista
    resultados_escolaridade[[escolaridade_nome]] <- obesid_escolaridade
  }
  
  # Combinar todos os resultados de cada escolaridade em um dataframe para o ano atual
  obesid_escolaridade_df <- do.call(rbind, resultados_escolaridade)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesid_escolaridade_df) <- c("Obesidade", "Obesidade por escolaridade (%)", "escolaridade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_escolaridade[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesid_escolaridade_df
}

# Unir todas as tabelas em uma única tabela
obesid_escolaridade_df_total <- do.call(rbind, lista_anos_escolaridade)

#Acrescentando a coluna de Ano no df
obesid_escolaridade_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesid_escolaridade_df_total))

# Exibir a tabela total
print(obesid_escolaridade_df_total)

################################## PESO BAIXO #################################
table(tab_fato_vigitel$peso_baixo)
##################################### ano #####################################
# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Criar o nome da variável para o subset
  subset_name <- paste("vig.", tolower(ano_atual), sep="")
  
  # Criar o subset para cada ano
  assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual))
  
  # Criar o nome da variável para o objeto de design de pesquisa
  survey_name <- paste("survey.", tolower(ano_atual), sep="")
  
  # Criar o objeto de design de pesquisa para cada ano
  assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                weights=get(subset_name)$pesorake))
  
  # Criar o nome da variável para a tabela ponderada por pesobaixo
  pesobaixo_name <- paste("pesobaixo.", tolower(ano_atual), sep="")
  
  # Calcular a tabela ponderada por pesobaixo para cada ano
  pesobaixo_ano <- svytable(~peso_baixo, design=get(survey_name)) %>%
    prop.table() %>%
    multiply_by(100) %>%
    round(digits=2) %>%
    as.data.frame()
  
  # Adicionar o nome do ano ao dataframe
  pesobaixo_ano$ano <- ano_atual
  
  # Salvar o resultado na lista
  lista_anos[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- pesobaixo_ano
}

# Combinar todos os resultados de cada ano em um dataframe
pesobaixo_anos_df_total <- do.call(rbind, lista_anos)

# Adicionar nomes de coluna se o número de colunas for adequado
colnames(pesobaixo_anos_df_total) <- c("Peso_Baixo", "Peso_Baixo por Ano (%)", "Ano")

# Exibir a tabela total
print(pesobaixo_anos_df_total)

##################################### sexo ####################################
# Vetor de sexo
sexo <- unique(tab_fato_vigitel$sexo)

# Lista para armazenar os nomes das sexo
lista_sexo <- character(length(sexo))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_sexo <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada sexo
  resultados_sexo <- list()
  
  # Loop para cada sexo
  for (sexo_nome in sexo) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada sexo e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & sexo == sexo_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada sexo e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da sexo
    lista_sexo[sexo_nome] <- sexo_nome
    
    # Criar o nome da variável para a tabela ponderada por pesobaixo
    pesobaixo_name <- paste("pesobaixo.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por pesobaixo para cada sexo e ano
    pesobaixo_sexo <- svytable(~peso_baixo, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da sexo ao dataframe
    pesobaixo_sexo$sexo <- sexo_nome
    
    # Salvar o resultado na lista
    resultados_sexo[[sexo_nome]] <- pesobaixo_sexo
  }
  
  # Combinar todos os resultados de cada sexo em um dataframe para o ano atual
  pesobaixo_sexo_df <- do.call(rbind, resultados_sexo)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(pesobaixo_sexo_df) <- c("Peso_Baixo", "Peso_Baixo por sexo (%)", "sexo")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_sexo[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- pesobaixo_sexo_df
}

# Unir todas as tabelas em uma única tabela
pesobaixo_sexo_df_total <- do.call(rbind, lista_anos_sexo)

#Acrescentando a coluna de Ano no df
pesobaixo_sexo_df_total$Ano <- rep(rep(anos, each = 4), length.out = nrow(pesobaixo_sexo_df_total))

# Exibir a tabela total
print(pesobaixo_sexo_df_total)

################################## faixa etária ###############################
# Vetor de faixas etárias
faixaeta <- unique(tab_fato_vigitel$faixaeta)

# Lista para armazenar os nomes das faixas etárias
lista_faixaeta <- character(length(faixaeta))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_faixaeta <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada faixa etária
  resultados_faixaeta <- list()
  
  # Loop para cada faixa etária
  for (faixaeta_nome in faixaeta) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada faixaeta e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & faixaeta == faixaeta_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada faixaeta e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da faixaeta
    lista_faixaeta[faixaeta_nome] <- faixaeta_nome
    
    # Criar o nome da variável para a tabela ponderada por pesobaixo
    pesobaixo_name <- paste("pesobaixo.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por pesobaixo para cada faixaeta e ano
    pesobaixo_faixaeta <- svytable(~peso_baixo, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da faixaeta ao dataframe
    pesobaixo_faixaeta$faixaeta <- faixaeta_nome
    
    # Salvar o resultado na lista
    resultados_faixaeta[[faixaeta_nome]] <- pesobaixo_faixaeta
  }
  
  # Combinar todos os resultados de cada faixaeta em um dataframe para o ano atual
  pesobaixo_faixaeta_df <- do.call(rbind, resultados_faixaeta)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(pesobaixo_faixaeta_df) <- c("Peso_Baixo", "Peso_Baixo por Faixaeta (%)", "Faixaeta")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_faixaeta[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- pesobaixo_faixaeta_df
}

# Unir todas as tabelas em uma única tabela
pesobaixo_faixaeta_df_total <- do.call(rbind, lista_anos_faixaeta)

#Acrescentando a coluna de Ano no df
pesobaixo_faixaeta_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(pesobaixo_faixaeta_df_total))

# Exibir a tabela total
print(pesobaixo_faixaeta_df_total)

#################################### cidades ##################################
# Vetor de cidades
cidades <- unique(tab_fato_vigitel$cidade)

# Lista para armazenar os nomes das cidades
lista_cidades <- character(length(cidades))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cidades <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cidade
  resultados_cidades <- list()
  
  # Loop para cada cidade
  for (cidade_nome in cidades) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cidade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cidade == cidade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cidade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cidade
    lista_cidades[cidade_nome] <- cidade_nome
    
    # Criar o nome da variável para a tabela ponderada por pesobaixo
    pesobaixo_name <- paste("pesobaixo", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por pesobaixo para cada cidade e ano
    pesobaixo_cidades <- svytable(~peso_baixo, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cidade ao dataframe
    pesobaixo_cidades$cidade <- cidade_nome
    
    # Salvar o resultado na lista
    resultados_cidades[[cidade_nome]] <- pesobaixo_cidades
  }
  
  # Combinar todos os resultados de cada cidade em um dataframe para o ano atual
  pesobaixo_cidades_df <- do.call(rbind, resultados_cidades)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(pesobaixo_cidades_df) <- c("Peso_Baixo", "Peso_Baixo por Cidade (%)", "Cidade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cidades[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- pesobaixo_cidades_df
}

# Unir todas as tabelas em uma única tabela
pesobaixo_cidades_df_total <- do.call(rbind, lista_anos_cidades)

#Acrescentando a coluna de Ano no df
pesobaixo_cidades_df_total$Ano <- rep(rep(anos, each = 54), length.out = nrow(pesobaixo_cidades_df_total))

# Exibir a tabela total
print(pesobaixo_cidades_df_total)

#################################### cor/raça #################################
# Vetor de cor_raca
cor_raca <- unique(tab_fato_vigitel$cor_raca)

# Lista para armazenar os nomes das cor_raca
lista_cor_raca <- character(length(cor_raca))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cor_raca <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cor_raca
  resultados_cor_raca <- list()
  
  # Loop para cada cor_raca
  for (cor_raca_nome in cor_raca) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cor_raca e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cor_raca == cor_raca_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cor_raca e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cor_raca
    lista_cor_raca[cor_raca_nome] <- cor_raca_nome
    
    # Criar o nome da variável para a tabela ponderada por pesobaixo
    pesobaixo_name <- paste("pesobaixo.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por pesobaixo para cada cor_raca e ano
    pesobaixo_cor_raca <- svytable(~peso_baixo, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cor_raca ao dataframe
    pesobaixo_cor_raca$cor_raca <- cor_raca_nome
    
    # Salvar o resultado na lista
    resultados_cor_raca[[cor_raca_nome]] <- pesobaixo_cor_raca
  }
  
  # Combinar todos os resultados de cada cor_raca em um dataframe para o ano atual
  pesobaixo_cor_raca_df <- do.call(rbind, resultados_cor_raca)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(pesobaixo_cor_raca_df) <- c("Peso_Baixo", "Peso_Baixo_por_cor_raca", "cor_raca")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cor_raca[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- pesobaixo_cor_raca_df
}

# Unir todas as tabelas em uma única tabela
pesobaixo_cor_raca_df_total <- do.call(rbind, lista_anos_cor_raca)

#Acrescentando a coluna de Ano no df
pesobaixo_cor_raca_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(pesobaixo_cor_raca_df_total))

# Exibir a tabela total
print(pesobaixo_cor_raca_df_total)

################################## escolaridade ###############################
# Vetor de escolaridade
escolaridade <- unique(tab_fato_vigitel$escolar)

# Lista para armazenar os nomes das escolaridade
lista_escolaridade <- character(length(escolaridade))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_escolaridade <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada escolaridade
  resultados_escolaridade <- list()
  
  # Loop para cada escolaridade
  for (escolaridade_nome in escolaridade) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada escolaridade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & escolar == escolaridade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada escolaridade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da escolaridade
    lista_escolaridade[escolaridade_nome] <- escolaridade_nome
    
    # Criar o nome da variável para a tabela ponderada por pesobaixo
    pesobaixo_name <- paste("pesobaixo", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por pesobaixo para cada escolaridade e ano
    pesobaixo_escolaridade <- svytable(~peso_baixo, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da escolaridade ao dataframe
    pesobaixo_escolaridade$escolar <- escolaridade_nome
    
    # Salvar o resultado na lista
    resultados_escolaridade[[escolaridade_nome]] <- pesobaixo_escolaridade
  }
  
  # Combinar todos os resultados de cada escolaridade em um dataframe para o ano atual
  pesobaixo_escolaridade_df <- do.call(rbind, resultados_escolaridade)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(pesobaixo_escolaridade_df) <- c("Peso_Baixo", "Peso_Baixo_por_escolaridade", "escolaridade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_escolaridade[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- pesobaixo_escolaridade_df
}

# Unir todas as tabelas em uma única tabela
pesobaixo_escolaridade_df_total <- do.call(rbind, lista_anos_escolaridade)

#Acrescentando a coluna de Ano no df
pesobaixo_escolaridade_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(pesobaixo_escolaridade_df_total))

# Exibir a tabela total
print(pesobaixo_escolaridade_df_total)

################################### EUTROFIA ###################################
table(tab_fato_vigitel$eutrofia)
##################################### ano #####################################
# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Criar o nome da variável para o subset
  subset_name <- paste("vig.", tolower(ano_atual), sep="")
  
  # Criar o subset para cada ano
  assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual))
  
  # Criar o nome da variável para o objeto de design de pesquisa
  survey_name <- paste("survey.", tolower(ano_atual), sep="")
  
  # Criar o objeto de design de pesquisa para cada ano
  assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                weights=get(subset_name)$pesorake))
  
  # Criar o nome da variável para a tabela ponderada por eutrofia
  eutrofia_name <- paste("eutrofia.", tolower(ano_atual), sep="")
  
  # Calcular a tabela ponderada por eutrofia para cada ano
  eutrofia_ano <- svytable(~eutrofia, design=get(survey_name)) %>%
    prop.table() %>%
    multiply_by(100) %>%
    round(digits=2) %>%
    as.data.frame()
  
  # Adicionar o nome do ano ao dataframe
  eutrofia_ano$ano <- ano_atual
  
  # Salvar o resultado na lista
  lista_anos[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- eutrofia_ano
}

# Combinar todos os resultados de cada ano em um dataframe
eutrof_anos_df_total <- do.call(rbind, lista_anos)

# Adicionar nomes de coluna se o número de colunas for adequado
colnames(eutrof_anos_df_total) <- c("Eutrofia", "Eutrofia por Ano (%)", "Ano")

# Exibir a tabela total
print(eutrof_anos_df_total)

##################################### sexo ####################################
# Vetor de sexo
sexo <- unique(tab_fato_vigitel$sexo)

# Lista para armazenar os nomes das sexo
lista_sexo <- character(length(sexo))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_sexo <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada sexo
  resultados_sexo <- list()
  
  # Loop para cada sexo
  for (sexo_nome in sexo) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada sexo e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & sexo == sexo_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada sexo e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da sexo
    lista_sexo[sexo_nome] <- sexo_nome
    
    # Criar o nome da variável para a tabela ponderada por eutrofia
    eutrofia_name <- paste("eutrofia.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por eutrofia para cada sexo e ano
    eutrofia_sexo <- svytable(~eutrofia, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da sexo ao dataframe
    eutrofia_sexo$sexo <- sexo_nome
    
    # Salvar o resultado na lista
    resultados_sexo[[sexo_nome]] <- eutrofia_sexo
  }
  
  # Combinar todos os resultados de cada sexo em um dataframe para o ano atual
  eutrofia_sexo_df <- do.call(rbind, resultados_sexo)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(eutrofia_sexo_df) <- c("Eutrofia", "Eutrofia por sexo (%)", "sexo")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_sexo[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- eutrofia_sexo_df
}

# Unir todas as tabelas em uma única tabela
eutrofia_sexo_df_total <- do.call(rbind, lista_anos_sexo)

#Acrescentando a coluna de Ano no df
eutrofia_sexo_df_total$Ano <- rep(rep(anos, each = 4), length.out = nrow(eutrofia_sexo_df_total))

# Exibir a tabela total
print(eutrofia_sexo_df_total)

################################## faixa etária ###############################
# Vetor de faixas etárias
faixaeta <- unique(tab_fato_vigitel$faixaeta)

# Lista para armazenar os nomes das faixas etárias
lista_faixaeta <- character(length(faixaeta))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_faixaeta <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada faixa etária
  resultados_faixaeta <- list()
  
  # Loop para cada faixa etária
  for (faixaeta_nome in faixaeta) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada faixaeta e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & faixaeta == faixaeta_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada faixaeta e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da faixaeta
    lista_faixaeta[faixaeta_nome] <- faixaeta_nome
    
    # Criar o nome da variável para a tabela ponderada por eutrofia
    eutrofia_name <- paste("eutrofia.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por eutrofia para cada faixaeta e ano
    eutrofia_faixaeta <- svytable(~eutrofia, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da faixaeta ao dataframe
    eutrofia_faixaeta$faixaeta <- faixaeta_nome
    
    # Salvar o resultado na lista
    resultados_faixaeta[[faixaeta_nome]] <- eutrofia_faixaeta
  }
  
  # Combinar todos os resultados de cada faixaeta em um dataframe para o ano atual
  eutrofia_faixaeta_df <- do.call(rbind, resultados_faixaeta)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(eutrofia_faixaeta_df) <- c("Eutrofia", "Eutrofia por Faixaeta (%)", "Faixaeta")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_faixaeta[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- eutrofia_faixaeta_df
}

# Unir todas as tabelas em uma única tabela
eutrofia_faixaeta_df_total <- do.call(rbind, lista_anos_faixaeta)

#Acrescentando a coluna de Ano no df
eutrofia_faixaeta_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(eutrofia_faixaeta_df_total))

# Exibir a tabela total
print(eutrofia_faixaeta_df_total)

#################################### cidades ##################################
# Vetor de cidades
cidades <- unique(tab_fato_vigitel$cidade)

# Lista para armazenar os nomes das cidades
lista_cidades <- character(length(cidades))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cidades <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cidade
  resultados_cidades <- list()
  
  # Loop para cada cidade
  for (cidade_nome in cidades) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cidade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cidade == cidade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cidade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cidade
    lista_cidades[cidade_nome] <- cidade_nome
    
    # Criar o nome da variável para a tabela ponderada por eutrofia
    eutrofia_name <- paste("eutrofia", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por eutrofia para cada cidade e ano
    eutrofia_cidades <- svytable(~eutrofia, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cidade ao dataframe
    eutrofia_cidades$cidade <- cidade_nome
    
    # Salvar o resultado na lista
    resultados_cidades[[cidade_nome]] <- eutrofia_cidades
  }
  
  # Combinar todos os resultados de cada cidade em um dataframe para o ano atual
  eutrofia_cidades_df <- do.call(rbind, resultados_cidades)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(eutrofia_cidades_df) <- c("Eutrofia", "Eutrofia por Cidade (%)", "Cidade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cidades[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- eutrofia_cidades_df
}

# Unir todas as tabelas em uma única tabela
eutrofia_cidades_df_total <- do.call(rbind, lista_anos_cidades)

#Acrescentando a coluna de Ano no df
eutrofia_cidades_df_total$Ano <- rep(rep(anos, each = 54), length.out = nrow(eutrofia_cidades_df_total))

# Exibir a tabela total
print(eutrofia_cidades_df_total)

#################################### cor/raça #################################
# Vetor de cor_raca
cor_raca <- unique(tab_fato_vigitel$cor_raca)

# Lista para armazenar os nomes das cor_raca
lista_cor_raca <- character(length(cor_raca))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cor_raca <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cor_raca
  resultados_cor_raca <- list()
  
  # Loop para cada cor_raca
  for (cor_raca_nome in cor_raca) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cor_raca e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cor_raca == cor_raca_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cor_raca e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cor_raca
    lista_cor_raca[cor_raca_nome] <- cor_raca_nome
    
    # Criar o nome da variável para a tabela ponderada por eutrofia
    eutrofia_name <- paste("eutrofia.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por eutrofia para cada cor_raca e ano
    eutrofia_cor_raca <- svytable(~eutrofia, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cor_raca ao dataframe
    eutrofia_cor_raca$cor_raca <- cor_raca_nome
    
    # Salvar o resultado na lista
    resultados_cor_raca[[cor_raca_nome]] <- eutrofia_cor_raca
  }
  
  # Combinar todos os resultados de cada cor_raca em um dataframe para o ano atual
  eutrofia_cor_raca_df <- do.call(rbind, resultados_cor_raca)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(eutrofia_cor_raca_df) <- c("Eutrofia", "Eutrofia por cor_raca (%)", "cor_raca")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cor_raca[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- eutrofia_cor_raca_df
}

# Unir todas as tabelas em uma única tabela
eutrofia_cor_raca_df_total <- do.call(rbind, lista_anos_cor_raca)

#Acrescentando a coluna de Ano no df
eutrofia_cor_raca_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(eutrofia_cor_raca_df_total))

# Exibir a tabela total
print(eutrofia_cor_raca_df_total)

################################## escolaridade ###############################
# Vetor de escolaridade
escolaridade <- unique(tab_fato_vigitel$escolar)

# Lista para armazenar os nomes das escolaridade
lista_escolaridade <- character(length(escolaridade))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_escolaridade <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada escolaridade
  resultados_escolaridade <- list()
  
  # Loop para cada escolaridade
  for (escolaridade_nome in escolaridade) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada escolaridade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & escolar == escolaridade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada escolaridade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da escolaridade
    lista_escolaridade[escolaridade_nome] <- escolaridade_nome
    
    # Criar o nome da variável para a tabela ponderada por eutrofia
    eutrofia_name <- paste("eutrofia", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por eutrofia para cada escolaridade e ano
    eutrofia_escolaridade <- svytable(~eutrofia, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da escolaridade ao dataframe
    eutrofia_escolaridade$escolar <- escolaridade_nome
    
    # Salvar o resultado na lista
    resultados_escolaridade[[escolaridade_nome]] <- eutrofia_escolaridade
  }
  
  # Combinar todos os resultados de cada escolaridade em um dataframe para o ano atual
  eutrofia_escolaridade_df <- do.call(rbind, resultados_escolaridade)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(eutrofia_escolaridade_df) <- c("Eutrofia", "Eutrofia por escolaridade (%)", "escolaridade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_escolaridade[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- eutrofia_escolaridade_df
}

# Unir todas as tabelas em uma única tabela
eutrofia_escolaridade_df_total <- do.call(rbind, lista_anos_escolaridade)

#Acrescentando a coluna de Ano no df
eutrofia_escolaridade_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(eutrofia_escolaridade_df_total))

# Exibir a tabela total
print(eutrofia_escolaridade_df_total)
################################## SOBREPESO ###################################
table(tab_fato_vigitel$sobrepeso)
##################################### ano #####################################
# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Criar o nome da variável para o subset
  subset_name <- paste("vig.", tolower(ano_atual), sep="")
  
  # Criar o subset para cada ano
  assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual))
  
  # Criar o nome da variável para o objeto de design de pesquisa
  survey_name <- paste("survey.", tolower(ano_atual), sep="")
  
  # Criar o objeto de design de pesquisa para cada ano
  assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                weights=get(subset_name)$pesorake))
  
  # Criar o nome da variável para a tabela ponderada por sobrepeso
  sobrepeso_name <- paste("sobrepeso.", tolower(ano_atual), sep="")
  
  # Calcular a tabela ponderada por sobrepeso para cada ano
  sobrepeso_ano <- svytable(~sobrepeso, design=get(survey_name)) %>%
    prop.table() %>%
    multiply_by(100) %>%
    round(digits=2) %>%
    as.data.frame()
  
  # Adicionar o nome do ano ao dataframe
  sobrepeso_ano$ano <- ano_atual
  
  # Salvar o resultado na lista
  lista_anos[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- sobrepeso_ano
}

# Combinar todos os resultados de cada ano em um dataframe
sobrepeso_anos_df_total <- do.call(rbind, lista_anos)

# Adicionar nomes de coluna se o número de colunas for adequado
colnames(sobrepeso_anos_df_total) <- c("Sobrepeso", "Sobrepeso por Ano (%)", "Ano")

# Exibir a tabela total
print(sobrepeso_anos_df_total)

##################################### sexo ####################################
# Vetor de sexo
sexo <- unique(tab_fato_vigitel$sexo)

# Lista para armazenar os nomes das sexo
lista_sexo <- character(length(sexo))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_sexo <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada sexo
  resultados_sexo <- list()
  
  # Loop para cada sexo
  for (sexo_nome in sexo) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada sexo e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & sexo == sexo_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada sexo e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da sexo
    lista_sexo[sexo_nome] <- sexo_nome
    
    # Criar o nome da variável para a tabela ponderada por sobrepeso
    sobrepeso_name <- paste("sobrepeso.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por sobrepeso para cada sexo e ano
    sobrepeso_sexo <- svytable(~sobrepeso, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da sexo ao dataframe
    sobrepeso_sexo$sexo <- sexo_nome
    
    # Salvar o resultado na lista
    resultados_sexo[[sexo_nome]] <- sobrepeso_sexo
  }
  
  # Combinar todos os resultados de cada sexo em um dataframe para o ano atual
  sobrepeso_sexo_df <- do.call(rbind, resultados_sexo)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(sobrepeso_sexo_df) <- c("Sobrepeso", "Sobrepeso por sexo (%)", "sexo")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_sexo[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- sobrepeso_sexo_df
}

# Unir todas as tabelas em uma única tabela
sobrepeso_sexo_df_total <- do.call(rbind, lista_anos_sexo)

#Acrescentando a coluna de Ano no df
sobrepeso_sexo_df_total$Ano <- rep(rep(anos, each = 4), length.out = nrow(sobrepeso_sexo_df_total))

# Exibir a tabela total
print(sobrepeso_sexo_df_total)

################################## faixa etária ###############################
# Vetor de faixas etárias
faixaeta <- unique(tab_fato_vigitel$faixaeta)

# Lista para armazenar os nomes das faixas etárias
lista_faixaeta <- character(length(faixaeta))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_faixaeta <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada faixa etária
  resultados_faixaeta <- list()
  
  # Loop para cada faixa etária
  for (faixaeta_nome in faixaeta) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada faixaeta e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & faixaeta == faixaeta_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada faixaeta e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da faixaeta
    lista_faixaeta[faixaeta_nome] <- faixaeta_nome
    
    # Criar o nome da variável para a tabela ponderada por sobrepeso
    sobrepeso_name <- paste("sobrepeso.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por sobrepeso para cada faixaeta e ano
    sobrepeso_faixaeta <- svytable(~sobrepeso, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da faixaeta ao dataframe
    sobrepeso_faixaeta$faixaeta <- faixaeta_nome
    
    # Salvar o resultado na lista
    resultados_faixaeta[[faixaeta_nome]] <- sobrepeso_faixaeta
  }
  
  # Combinar todos os resultados de cada faixaeta em um dataframe para o ano atual
  sobrepeso_faixaeta_df <- do.call(rbind, resultados_faixaeta)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(sobrepeso_faixaeta_df) <- c("Sobrepeso", "Sobrepeso por Faixaeta (%)", "Faixaeta")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_faixaeta[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- sobrepeso_faixaeta_df
}

# Unir todas as tabelas em uma única tabela
sobrepeso_faixaeta_df_total <- do.call(rbind, lista_anos_faixaeta)

#Acrescentando a coluna de Ano no df
sobrepeso_faixaeta_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(sobrepeso_faixaeta_df_total))

# Exibir a tabela total
print(sobrepeso_faixaeta_df_total)

#################################### cidades ##################################
# Vetor de cidades
cidades <- unique(tab_fato_vigitel$cidade)

# Lista para armazenar os nomes das cidades
lista_cidades <- character(length(cidades))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cidades <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cidade
  resultados_cidades <- list()
  
  # Loop para cada cidade
  for (cidade_nome in cidades) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cidade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cidade == cidade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cidade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cidade
    lista_cidades[cidade_nome] <- cidade_nome
    
    # Criar o nome da variável para a tabela ponderada por sobrepeso
    sobrepeso_name <- paste("sobrepeso", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por sobrepeso para cada cidade e ano
    sobrepeso_cidades <- svytable(~sobrepeso, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cidade ao dataframe
    sobrepeso_cidades$cidade <- cidade_nome
    
    # Salvar o resultado na lista
    resultados_cidades[[cidade_nome]] <- sobrepeso_cidades
  }
  
  # Combinar todos os resultados de cada cidade em um dataframe para o ano atual
  sobrepeso_cidades_df <- do.call(rbind, resultados_cidades)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(sobrepeso_cidades_df) <- c("Sobrepeso", "Sobrepeso por Cidade (%)", "Cidade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cidades[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- sobrepeso_cidades_df
}

# Unir todas as tabelas em uma única tabela
sobrepeso_cidades_df_total <- do.call(rbind, lista_anos_cidades)

#Acrescentando a coluna de Ano no df
sobrepeso_cidades_df_total$Ano <- rep(rep(anos, each = 54), length.out = nrow(sobrepeso_cidades_df_total))

# Exibir a tabela total
print(sobrepeso_cidades_df_total)

#################################### cor/raça #################################
# Vetor de cor_raca
cor_raca <- unique(tab_fato_vigitel$cor_raca)

# Lista para armazenar os nomes das cor_raca
lista_cor_raca <- character(length(cor_raca))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cor_raca <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cor_raca
  resultados_cor_raca <- list()
  
  # Loop para cada cor_raca
  for (cor_raca_nome in cor_raca) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cor_raca e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cor_raca == cor_raca_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cor_raca e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cor_raca
    lista_cor_raca[cor_raca_nome] <- cor_raca_nome
    
    # Criar o nome da variável para a tabela ponderada por sobrepeso
    sobrepeso_name <- paste("sobrepeso.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por sobrepeso para cada cor_raca e ano
    sobrepeso_cor_raca <- svytable(~sobrepeso, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cor_raca ao dataframe
    sobrepeso_cor_raca$cor_raca <- cor_raca_nome
    
    # Salvar o resultado na lista
    resultados_cor_raca[[cor_raca_nome]] <- sobrepeso_cor_raca
  }
  
  # Combinar todos os resultados de cada cor_raca em um dataframe para o ano atual
  sobrepeso_cor_raca_df <- do.call(rbind, resultados_cor_raca)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(sobrepeso_cor_raca_df) <- c("Sobrepeso", "Sobrepeso por cor_raca (%)", "cor_raca")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cor_raca[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- sobrepeso_cor_raca_df
}

# Unir todas as tabelas em uma única tabela
sobrepeso_cor_raca_df_total <- do.call(rbind, lista_anos_cor_raca)

#Acrescentando a coluna de Ano no df
sobrepeso_cor_raca_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(sobrepeso_cor_raca_df_total))

# Exibir a tabela total
print(sobrepeso_cor_raca_df_total)

################################## escolaridade ###############################
# Vetor de escolaridade
escolaridade <- unique(tab_fato_vigitel$escolar)

# Lista para armazenar os nomes das escolaridade
lista_escolaridade <- character(length(escolaridade))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_escolaridade <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada escolaridade
  resultados_escolaridade <- list()
  
  # Loop para cada escolaridade
  for (escolaridade_nome in escolaridade) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada escolaridade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & escolar == escolaridade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada escolaridade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da escolaridade
    lista_escolaridade[escolaridade_nome] <- escolaridade_nome
    
    # Criar o nome da variável para a tabela ponderada por sobrepeso
    sobrepeso_name <- paste("sobrepeso", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por sobrepeso para cada escolaridade e ano
    sobrepeso_escolaridade <- svytable(~sobrepeso, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da escolaridade ao dataframe
    sobrepeso_escolaridade$escolar <- escolaridade_nome
    
    # Salvar o resultado na lista
    resultados_escolaridade[[escolaridade_nome]] <- sobrepeso_escolaridade
  }
  
  # Combinar todos os resultados de cada escolaridade em um dataframe para o ano atual
  sobrepeso_escolaridade_df <- do.call(rbind, resultados_escolaridade)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(sobrepeso_escolaridade_df) <- c("Sobrepeso", "Sobrepeso por Escolaridade (%)", "escolaridade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_escolaridade[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- sobrepeso_escolaridade_df
}

# Unir todas as tabelas em uma única tabela
sobrepeso_escolaridade_df_total <- do.call(rbind, lista_anos_escolaridade)

#Acrescentando a coluna de Ano no df
sobrepeso_escolaridade_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(sobrepeso_escolaridade_df_total))

# Exibir a tabela total
print(sobrepeso_escolaridade_df_total)

############################## OBESIDADE GRAU I ################################
table(tab_fato_vigitel$obesidade_grau_1)
##################################### ano #####################################
# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Criar o nome da variável para o subset
  subset_name <- paste("vig.", tolower(ano_atual), sep="")
  
  # Criar o subset para cada ano
  assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual))
  
  # Criar o nome da variável para o objeto de design de pesquisa
  survey_name <- paste("survey.", tolower(ano_atual), sep="")
  
  # Criar o objeto de design de pesquisa para cada ano
  assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                weights=get(subset_name)$pesorake))
  
  # Criar o nome da variável para a tabela ponderada por obesidgrau1
  obesidgrau1_name <- paste("obesidgrau1.", tolower(ano_atual), sep="")
  
  # Calcular a tabela ponderada por obesidgrau1 para cada ano
  obesidgrau1_ano <- svytable(~obesidade_grau_1, design=get(survey_name)) %>%
    prop.table() %>%
    multiply_by(100) %>%
    round(digits=2) %>%
    as.data.frame()
  
  # Adicionar o nome do ano ao dataframe
  obesidgrau1_ano$ano <- ano_atual
  
  # Salvar o resultado na lista
  lista_anos[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau1_ano
}

# Combinar todos os resultados de cada ano em um dataframe
obesidgrau1_anos_df_total <- do.call(rbind, lista_anos)

# Adicionar nomes de coluna se o número de colunas for adequado
colnames(obesidgrau1_anos_df_total) <- c("Obesid_Grau_1", "Obesid_Grau_1 por Ano (%)", "Ano")

# Exibir a tabela total
print(obesidgrau1_anos_df_total)

##################################### sexo ####################################
# Vetor de sexo
sexo <- unique(tab_fato_vigitel$sexo)

# Lista para armazenar os nomes das sexo
lista_sexo <- character(length(sexo))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_sexo <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada sexo
  resultados_sexo <- list()
  
  # Loop para cada sexo
  for (sexo_nome in sexo) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada sexo e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & sexo == sexo_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada sexo e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da sexo
    lista_sexo[sexo_nome] <- sexo_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau1
    obesidgrau1_name <- paste("obesidgrau1.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau1 para cada sexo e ano
    obesidgrau1_sexo <- svytable(~obesidade_grau_1, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da sexo ao dataframe
    obesidgrau1_sexo$sexo <- sexo_nome
    
    # Salvar o resultado na lista
    resultados_sexo[[sexo_nome]] <- obesidgrau1_sexo
  }
  
  # Combinar todos os resultados de cada sexo em um dataframe para o ano atual
  obesidgrau1_sexo_df <- do.call(rbind, resultados_sexo)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau1_sexo_df) <- c("Obesid_Grau_1", "Obesid_Grau_1 por sexo (%)", "sexo")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_sexo[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau1_sexo_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau1_sexo_df_total <- do.call(rbind, lista_anos_sexo)

#Acrescentando a coluna de Ano no df
obesidgrau1_sexo_df_total$Ano <- rep(rep(anos, each = 4), length.out = nrow(obesidgrau1_sexo_df_total))

# Exibir a tabela total
print(obesidgrau1_sexo_df_total)

################################## faixa etária ###############################
# Vetor de faixas etárias
faixaeta <- unique(tab_fato_vigitel$faixaeta)

# Lista para armazenar os nomes das faixas etárias
lista_faixaeta <- character(length(faixaeta))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_faixaeta <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada faixa etária
  resultados_faixaeta <- list()
  
  # Loop para cada faixa etária
  for (faixaeta_nome in faixaeta) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada faixaeta e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & faixaeta == faixaeta_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada faixaeta e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da faixaeta
    lista_faixaeta[faixaeta_nome] <- faixaeta_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau1
    obesidgrau1_name <- paste("obesidgrau1.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau1 para cada faixaeta e ano
    obesidgrau1_faixaeta <- svytable(~obesidade_grau_1, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da faixaeta ao dataframe
    obesidgrau1_faixaeta$faixaeta <- faixaeta_nome
    
    # Salvar o resultado na lista
    resultados_faixaeta[[faixaeta_nome]] <- obesidgrau1_faixaeta
  }
  
  # Combinar todos os resultados de cada faixaeta em um dataframe para o ano atual
  obesidgrau1_faixaeta_df <- do.call(rbind, resultados_faixaeta)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau1_faixaeta_df) <- c("Obesid_Grau_1", "Obesid_Grau_1 por Faixaeta (%)", "Faixaeta")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_faixaeta[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau1_faixaeta_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau1_faixaeta_df_total <- do.call(rbind, lista_anos_faixaeta)

#Acrescentando a coluna de Ano no df
obesidgrau1_faixaeta_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesidgrau1_faixaeta_df_total))

# Exibir a tabela total
print(obesidgrau1_faixaeta_df_total)

#################################### cidades ##################################
# Vetor de cidades
cidades <- unique(tab_fato_vigitel$cidade)

# Lista para armazenar os nomes das cidades
lista_cidades <- character(length(cidades))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cidades <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cidade
  resultados_cidades <- list()
  
  # Loop para cada cidade
  for (cidade_nome in cidades) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cidade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cidade == cidade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cidade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cidade
    lista_cidades[cidade_nome] <- cidade_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau1
    obesidgrau1_name <- paste("obesidgrau1", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau1 para cada cidade e ano
    obesidgrau1_cidades <- svytable(~obesidade_grau_1, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cidade ao dataframe
    obesidgrau1_cidades$cidade <- cidade_nome
    
    # Salvar o resultado na lista
    resultados_cidades[[cidade_nome]] <- obesidgrau1_cidades
  }
  
  # Combinar todos os resultados de cada cidade em um dataframe para o ano atual
  obesidgrau1_cidades_df <- do.call(rbind, resultados_cidades)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau1_cidades_df) <- c("Obesid_Grau_1", "Obesid_Grau_1 por Cidade (%)", "Cidade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cidades[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau1_cidades_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau1_cidades_df_total <- do.call(rbind, lista_anos_cidades)

#Acrescentando a coluna de Ano no df
obesidgrau1_cidades_df_total$Ano <- rep(rep(anos, each = 54), length.out = nrow(obesidgrau1_cidades_df_total))

# Exibir a tabela total
print(obesidgrau1_cidades_df_total)

#################################### cor/raça #################################
# Vetor de cor_raca
cor_raca <- unique(tab_fato_vigitel$cor_raca)

# Lista para armazenar os nomes das cor_raca
lista_cor_raca <- character(length(cor_raca))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cor_raca <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cor_raca
  resultados_cor_raca <- list()
  
  # Loop para cada cor_raca
  for (cor_raca_nome in cor_raca) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cor_raca e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cor_raca == cor_raca_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cor_raca e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cor_raca
    lista_cor_raca[cor_raca_nome] <- cor_raca_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau1
    obesidgrau1_name <- paste("obesidgrau1.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau1 para cada cor_raca e ano
    obesidgrau1_cor_raca <- svytable(~obesidade_grau_1, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cor_raca ao dataframe
    obesidgrau1_cor_raca$cor_raca <- cor_raca_nome
    
    # Salvar o resultado na lista
    resultados_cor_raca[[cor_raca_nome]] <- obesidgrau1_cor_raca
  }
  
  # Combinar todos os resultados de cada cor_raca em um dataframe para o ano atual
  obesidgrau1_cor_raca_df <- do.call(rbind, resultados_cor_raca)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau1_cor_raca_df) <- c("Obesid_Grau_1", "Obesid_Grau_1 por cor_raca (%)", "cor_raca")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cor_raca[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau1_cor_raca_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau1_cor_raca_df_total <- do.call(rbind, lista_anos_cor_raca)

#Acrescentando a coluna de Ano no df
obesidgrau1_cor_raca_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesidgrau1_cor_raca_df_total))

# Exibir a tabela total
print(obesidgrau1_cor_raca_df_total)

################################## escolaridade ###############################
# Vetor de escolaridade
escolaridade <- unique(tab_fato_vigitel$escolar)

# Lista para armazenar os nomes das escolaridade
lista_escolaridade <- character(length(escolaridade))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_escolaridade <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada escolaridade
  resultados_escolaridade <- list()
  
  # Loop para cada escolaridade
  for (escolaridade_nome in escolaridade) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada escolaridade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & escolar == escolaridade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada escolaridade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da escolaridade
    lista_escolaridade[escolaridade_nome] <- escolaridade_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau1
    obesidgrau1_name <- paste("obesidgrau1", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau1 para cada escolaridade e ano
    obesidgrau1_escolaridade <- svytable(~obesidade_grau_1, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da escolaridade ao dataframe
    obesidgrau1_escolaridade$escolar <- escolaridade_nome
    
    # Salvar o resultado na lista
    resultados_escolaridade[[escolaridade_nome]] <- obesidgrau1_escolaridade
  }
  
  # Combinar todos os resultados de cada escolaridade em um dataframe para o ano atual
  obesidgrau1_escolaridade_df <- do.call(rbind, resultados_escolaridade)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau1_escolaridade_df) <- c("Obesid_Grau_1", "Obesid_Grau_1 por escolaridade (%)", "escolaridade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_escolaridade[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau1_escolaridade_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau1_escolaridade_df_total <- do.call(rbind, lista_anos_escolaridade)

#Acrescentando a coluna de Ano no df
obesidgrau1_escolaridade_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesidgrau1_escolaridade_df_total))

# Exibir a tabela total
print(obesidgrau1_escolaridade_df_total)

############################# OBESIDADE GRAU II ################################
table(tab_fato_vigitel$obesidade_grau_2)
##################################### ano #####################################
# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Criar o nome da variável para o subset
  subset_name <- paste("vig.", tolower(ano_atual), sep="")
  
  # Criar o subset para cada ano
  assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual))
  
  # Criar o nome da variável para o objeto de design de pesquisa
  survey_name <- paste("survey.", tolower(ano_atual), sep="")
  
  # Criar o objeto de design de pesquisa para cada ano
  assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                weights=get(subset_name)$pesorake))
  
  # Criar o nome da variável para a tabela ponderada por obesidgrau2
  obesidgrau2_name <- paste("obesidgrau2.", tolower(ano_atual), sep="")
  
  # Calcular a tabela ponderada por obesidgrau2 para cada ano
  obesidgrau2_ano <- svytable(~obesidade_grau_2, design=get(survey_name)) %>%
    prop.table() %>%
    multiply_by(100) %>%
    round(digits=2) %>%
    as.data.frame()
  
  # Adicionar o nome do ano ao dataframe
  obesidgrau2_ano$ano <- ano_atual
  
  # Salvar o resultado na lista
  lista_anos[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau2_ano
}

# Combinar todos os resultados de cada ano em um dataframe
obesidgrau2_anos_df_total <- do.call(rbind, lista_anos)

# Adicionar nomes de coluna se o número de colunas for adequado
colnames(obesidgrau2_anos_df_total) <- c("Obesid_Grau_2", "Obesid_Grau_2 por Ano (%)", "Ano")

# Exibir a tabela total
print(obesidgrau2_anos_df_total)

##################################### sexo ####################################
# Vetor de sexo
sexo <- unique(tab_fato_vigitel$sexo)

# Lista para armazenar os nomes das sexo
lista_sexo <- character(length(sexo))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_sexo <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada sexo
  resultados_sexo <- list()
  
  # Loop para cada sexo
  for (sexo_nome in sexo) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada sexo e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & sexo == sexo_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada sexo e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da sexo
    lista_sexo[sexo_nome] <- sexo_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau2
    obesidgrau2_name <- paste("obesidgrau2.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau2 para cada sexo e ano
    obesidgrau2_sexo <- svytable(~obesidade_grau_2, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da sexo ao dataframe
    obesidgrau2_sexo$sexo <- sexo_nome
    
    # Salvar o resultado na lista
    resultados_sexo[[sexo_nome]] <- obesidgrau2_sexo
  }
  
  # Combinar todos os resultados de cada sexo em um dataframe para o ano atual
  obesidgrau2_sexo_df <- do.call(rbind, resultados_sexo)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau2_sexo_df) <- c("Obesid_Grau_2", "Obesid_Grau_2 por sexo (%)", "sexo")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_sexo[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau2_sexo_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau2_sexo_df_total <- do.call(rbind, lista_anos_sexo)

#Acrescentando a coluna de Ano no df
obesidgrau2_sexo_df_total$Ano <- rep(rep(anos, each = 4), length.out = nrow(obesidgrau2_sexo_df_total))

# Exibir a tabela total
print(obesidgrau2_sexo_df_total)

################################## faixa etária ###############################
# Vetor de faixas etárias
faixaeta <- unique(tab_fato_vigitel$faixaeta)

# Lista para armazenar os nomes das faixas etárias
lista_faixaeta <- character(length(faixaeta))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_faixaeta <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada faixa etária
  resultados_faixaeta <- list()
  
  # Loop para cada faixa etária
  for (faixaeta_nome in faixaeta) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada faixaeta e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & faixaeta == faixaeta_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada faixaeta e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da faixaeta
    lista_faixaeta[faixaeta_nome] <- faixaeta_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau2
    obesidgrau2_name <- paste("obesidgrau2.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau2 para cada faixaeta e ano
    obesidgrau2_faixaeta <- svytable(~obesidade_grau_2, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da faixaeta ao dataframe
    obesidgrau2_faixaeta$faixaeta <- faixaeta_nome
    
    # Salvar o resultado na lista
    resultados_faixaeta[[faixaeta_nome]] <- obesidgrau2_faixaeta
  }
  
  # Combinar todos os resultados de cada faixaeta em um dataframe para o ano atual
  obesidgrau2_faixaeta_df <- do.call(rbind, resultados_faixaeta)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau2_faixaeta_df) <- c("Obesid_Grau_2", "Obesid_Grau_2 por Faixaeta (%)", "Faixaeta")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_faixaeta[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau2_faixaeta_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau2_faixaeta_df_total <- do.call(rbind, lista_anos_faixaeta)

#Acrescentando a coluna de Ano no df
obesidgrau2_faixaeta_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesidgrau2_faixaeta_df_total))

# Exibir a tabela total
print(obesidgrau2_faixaeta_df_total)

#################################### cidades ##################################
# Vetor de cidades
cidades <- unique(tab_fato_vigitel$cidade)

# Lista para armazenar os nomes das cidades
lista_cidades <- character(length(cidades))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cidades <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cidade
  resultados_cidades <- list()
  
  # Loop para cada cidade
  for (cidade_nome in cidades) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cidade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cidade == cidade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cidade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cidade
    lista_cidades[cidade_nome] <- cidade_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau2
    obesidgrau2_name <- paste("obesidgrau2", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau2 para cada cidade e ano
    obesidgrau2_cidades <- svytable(~obesidade_grau_2, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cidade ao dataframe
    obesidgrau2_cidades$cidade <- cidade_nome
    
    # Salvar o resultado na lista
    resultados_cidades[[cidade_nome]] <- obesidgrau2_cidades
  }
  
  # Combinar todos os resultados de cada cidade em um dataframe para o ano atual
  obesidgrau2_cidades_df <- do.call(rbind, resultados_cidades)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau2_cidades_df) <- c("Obesid_Grau_2", "Obesid_Grau_2 por Cidade (%)", "Cidade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cidades[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau2_cidades_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau2_cidades_df_total <- do.call(rbind, lista_anos_cidades)

#Acrescentando a coluna de Ano no df
obesidgrau2_cidades_df_total$Ano <- rep(rep(anos, each = 54), length.out = nrow(obesidgrau2_cidades_df_total))

# Exibir a tabela total
print(obesidgrau2_cidades_df_total)

#################################### cor/raça #################################
# Vetor de cor_raca
cor_raca <- unique(tab_fato_vigitel$cor_raca)

# Lista para armazenar os nomes das cor_raca
lista_cor_raca <- character(length(cor_raca))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cor_raca <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cor_raca
  resultados_cor_raca <- list()
  
  # Loop para cada cor_raca
  for (cor_raca_nome in cor_raca) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cor_raca e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cor_raca == cor_raca_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cor_raca e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cor_raca
    lista_cor_raca[cor_raca_nome] <- cor_raca_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau2
    obesidgrau2_name <- paste("obesidgrau2.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau2 para cada cor_raca e ano
    obesidgrau2_cor_raca <- svytable(~obesidade_grau_2, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cor_raca ao dataframe
    obesidgrau2_cor_raca$cor_raca <- cor_raca_nome
    
    # Salvar o resultado na lista
    resultados_cor_raca[[cor_raca_nome]] <- obesidgrau2_cor_raca
  }
  
  # Combinar todos os resultados de cada cor_raca em um dataframe para o ano atual
  obesidgrau2_cor_raca_df <- do.call(rbind, resultados_cor_raca)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau2_cor_raca_df) <- c("Obesid_Grau_2", "Obesid_Grau_2 por cor_raca (%)", "cor_raca")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cor_raca[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau2_cor_raca_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau2_cor_raca_df_total <- do.call(rbind, lista_anos_cor_raca)

#Acrescentando a coluna de Ano no df
obesidgrau2_cor_raca_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesidgrau2_cor_raca_df_total))

# Exibir a tabela total
print(obesidgrau2_cor_raca_df_total)

################################## escolaridade ###############################
# Vetor de escolaridade
escolaridade <- unique(tab_fato_vigitel$escolar)

# Lista para armazenar os nomes das escolaridade
lista_escolaridade <- character(length(escolaridade))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_escolaridade <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada escolaridade
  resultados_escolaridade <- list()
  
  # Loop para cada escolaridade
  for (escolaridade_nome in escolaridade) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada escolaridade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & escolar == escolaridade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada escolaridade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da escolaridade
    lista_escolaridade[escolaridade_nome] <- escolaridade_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau2
    obesidgrau2_name <- paste("obesidgrau2", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau2 para cada escolaridade e ano
    obesidgrau2_escolaridade <- svytable(~obesidade_grau_2, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da escolaridade ao dataframe
    obesidgrau2_escolaridade$escolar <- escolaridade_nome
    
    # Salvar o resultado na lista
    resultados_escolaridade[[escolaridade_nome]] <- obesidgrau2_escolaridade
  }
  
  # Combinar todos os resultados de cada escolaridade em um dataframe para o ano atual
  obesidgrau2_escolaridade_df <- do.call(rbind, resultados_escolaridade)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau2_escolaridade_df) <- c("Obesid_Grau_2", "Obesid_Grau_2_por_escolaridade", "escolaridade")

  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_escolaridade[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau2_escolaridade_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau2_escolaridade_df_total <- do.call(rbind, lista_anos_escolaridade)

#Acrescentando a coluna de Ano no df
obesidgrau2_escolaridade_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesidgrau2_escolaridade_df_total))

# Exibir a tabela total
print(obesidgrau2_escolaridade_df_total)
############################ OBESIDADE GRAU III ################################
table(tab_fato_vigitel$obesidade_grau_3)
##################################### ano #####################################
# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Criar o nome da variável para o subset
  subset_name <- paste("vig.", tolower(ano_atual), sep="")
  
  # Criar o subset para cada ano
  assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual))
  
  # Criar o nome da variável para o objeto de design de pesquisa
  survey_name <- paste("survey.", tolower(ano_atual), sep="")
  
  # Criar o objeto de design de pesquisa para cada ano
  assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                weights=get(subset_name)$pesorake))
  
  # Criar o nome da variável para a tabela ponderada por obesidgrau3
  obesidgrau3_name <- paste("obesidgrau3.", tolower(ano_atual), sep="")
  
  # Calcular a tabela ponderada por obesidgrau3 para cada ano
  obesidgrau3_ano <- svytable(~obesidade_grau_3, design=get(survey_name)) %>%
    prop.table() %>%
    multiply_by(100) %>%
    round(digits=2) %>%
    as.data.frame()
  
  # Adicionar o nome do ano ao dataframe
  obesidgrau3_ano$ano <- ano_atual
  
  # Salvar o resultado na lista
  lista_anos[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau3_ano
}

# Combinar todos os resultados de cada ano em um dataframe
obesidgrau3_anos_df_total <- do.call(rbind, lista_anos)

# Adicionar nomes de coluna se o número de colunas for adequado
colnames(obesidgrau3_anos_df_total) <- c("Obesid_Grau_3", "Obesid_Grau_3 por Ano (%)", "Ano")

# Exibir a tabela total
print(obesidgrau3_anos_df_total)

##################################### sexo ####################################
# Vetor de sexo
sexo <- unique(tab_fato_vigitel$sexo)

# Lista para armazenar os nomes das sexo
lista_sexo <- character(length(sexo))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_sexo <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada sexo
  resultados_sexo <- list()
  
  # Loop para cada sexo
  for (sexo_nome in sexo) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada sexo e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & sexo == sexo_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada sexo e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da sexo
    lista_sexo[sexo_nome] <- sexo_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau3
    obesidgrau3_name <- paste("obesidgrau3.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau3 para cada sexo e ano
    obesidgrau3_sexo <- svytable(~obesidade_grau_3, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da sexo ao dataframe
    obesidgrau3_sexo$sexo <- sexo_nome
    
    # Salvar o resultado na lista
    resultados_sexo[[sexo_nome]] <- obesidgrau3_sexo
  }
  
  # Combinar todos os resultados de cada sexo em um dataframe para o ano atual
  obesidgrau3_sexo_df <- do.call(rbind, resultados_sexo)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau3_sexo_df) <- c("Obesid_Grau_3", "Obesid_Grau_3 por sexo (%)", "sexo")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_sexo[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau3_sexo_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau3_sexo_df_total <- do.call(rbind, lista_anos_sexo)

#Acrescentando a coluna de Ano no df
obesidgrau3_sexo_df_total$Ano <- rep(rep(anos, each = 4), length.out = nrow(obesidgrau3_sexo_df_total))

# Exibir a tabela total
print(obesidgrau3_sexo_df_total)

################################## faixa etária ###############################
# Vetor de faixas etárias
faixaeta <- unique(tab_fato_vigitel$faixaeta)

# Lista para armazenar os nomes das faixas etárias
lista_faixaeta <- character(length(faixaeta))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_faixaeta <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada faixa etária
  resultados_faixaeta <- list()
  
  # Loop para cada faixa etária
  for (faixaeta_nome in faixaeta) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada faixaeta e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & faixaeta == faixaeta_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada faixaeta e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da faixaeta
    lista_faixaeta[faixaeta_nome] <- faixaeta_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau3
    obesidgrau3_name <- paste("obesidgrau3.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau3 para cada faixaeta e ano
    obesidgrau3_faixaeta <- svytable(~obesidade_grau_3, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da faixaeta ao dataframe
    obesidgrau3_faixaeta$faixaeta <- faixaeta_nome
    
    # Salvar o resultado na lista
    resultados_faixaeta[[faixaeta_nome]] <- obesidgrau3_faixaeta
  }
  
  # Combinar todos os resultados de cada faixaeta em um dataframe para o ano atual
  obesidgrau3_faixaeta_df <- do.call(rbind, resultados_faixaeta)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau3_faixaeta_df) <- c("Obesid_Grau_3", "Obesid_Grau_3 por Faixaeta (%)", "Faixaeta")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_faixaeta[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau3_faixaeta_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau3_faixaeta_df_total <- do.call(rbind, lista_anos_faixaeta)

#Acrescentando a coluna de Ano no df
obesidgrau3_faixaeta_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesidgrau3_faixaeta_df_total))

# Exibir a tabela total
print(obesidgrau3_faixaeta_df_total)

#################################### cidades ##################################
# Vetor de cidades
cidades <- unique(tab_fato_vigitel$cidade)

# Lista para armazenar os nomes das cidades
lista_cidades <- character(length(cidades))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cidades <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cidade
  resultados_cidades <- list()
  
  # Loop para cada cidade
  for (cidade_nome in cidades) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cidade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cidade == cidade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cidade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cidade
    lista_cidades[cidade_nome] <- cidade_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau3
    obesidgrau3_name <- paste("obesidgrau3", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau3 para cada cidade e ano
    obesidgrau3_cidades <- svytable(~obesidade_grau_3, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cidade ao dataframe
    obesidgrau3_cidades$cidade <- cidade_nome
    
    # Salvar o resultado na lista
    resultados_cidades[[cidade_nome]] <- obesidgrau3_cidades
  }
  
  # Combinar todos os resultados de cada cidade em um dataframe para o ano atual
  obesidgrau3_cidades_df <- do.call(rbind, resultados_cidades)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau3_cidades_df) <- c("Obesid_Grau_3", "Obesid_Grau_3 por Cidade (%)", "Cidade")
 
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cidades[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau3_cidades_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau3_cidades_df_total <- do.call(rbind, lista_anos_cidades)

#Acrescentando a coluna de Ano no df
obesidgrau3_cidades_df_total$Ano <- rep(rep(anos, each = 54), length.out = nrow(obesidgrau3_cidades_df_total))

# Exibir a tabela total
print(obesidgrau3_cidades_df_total)

#################################### cor/raça #################################
# Vetor de cor_raca
cor_raca <- unique(tab_fato_vigitel$cor_raca)

# Lista para armazenar os nomes das cor_raca
lista_cor_raca <- character(length(cor_raca))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cor_raca <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cor_raca
  resultados_cor_raca <- list()
  
  # Loop para cada cor_raca
  for (cor_raca_nome in cor_raca) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cor_raca e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cor_raca == cor_raca_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cor_raca e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cor_raca
    lista_cor_raca[cor_raca_nome] <- cor_raca_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau3
    obesidgrau3_name <- paste("obesidgrau3.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau3 para cada cor_raca e ano
    obesidgrau3_cor_raca <- svytable(~obesidade_grau_3, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cor_raca ao dataframe
    obesidgrau3_cor_raca$cor_raca <- cor_raca_nome
    
    # Salvar o resultado na lista
    resultados_cor_raca[[cor_raca_nome]] <- obesidgrau3_cor_raca
  }
  
  # Combinar todos os resultados de cada cor_raca em um dataframe para o ano atual
  obesidgrau3_cor_raca_df <- do.call(rbind, resultados_cor_raca)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau3_cor_raca_df) <- c("Obesid_Grau_3", "Obesid_Grau_3_por_cor_raca", "cor_raca")
 
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cor_raca[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau3_cor_raca_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau3_cor_raca_df_total <- do.call(rbind, lista_anos_cor_raca)

#Acrescentando a coluna de Ano no df
obesidgrau3_cor_raca_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesidgrau3_cor_raca_df_total))

# Exibir a tabela total
print(obesidgrau3_cor_raca_df_total)

################################## escolaridade ###############################
# Vetor de escolaridade
escolaridade <- unique(tab_fato_vigitel$escolar)

# Lista para armazenar os nomes das escolaridade
lista_escolaridade <- character(length(escolaridade))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_escolaridade <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada escolaridade
  resultados_escolaridade <- list()
  
  # Loop para cada escolaridade
  for (escolaridade_nome in escolaridade) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada escolaridade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & escolar == escolaridade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada escolaridade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da escolaridade
    lista_escolaridade[escolaridade_nome] <- escolaridade_nome
    
    # Criar o nome da variável para a tabela ponderada por obesidgrau3
    obesidgrau3_name <- paste("obesidgrau3", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por obesidgrau3 para cada escolaridade e ano
    obesidgrau3_escolaridade <- svytable(~obesidade_grau_3, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da escolaridade ao dataframe
    obesidgrau3_escolaridade$escolar <- escolaridade_nome
    
    # Salvar o resultado na lista
    resultados_escolaridade[[escolaridade_nome]] <- obesidgrau3_escolaridade
  }
  
  # Combinar todos os resultados de cada escolaridade em um dataframe para o ano atual
  obesidgrau3_escolaridade_df <- do.call(rbind, resultados_escolaridade)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(obesidgrau3_escolaridade_df) <- c("Obesid_Grau_3", "Obesid_Grau_3_por_escolaridade", "escolaridade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_escolaridade[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- obesidgrau3_escolaridade_df
}

# Unir todas as tabelas em uma única tabela
obesidgrau3_escolaridade_df_total <- do.call(rbind, lista_anos_escolaridade)

#Acrescentando a coluna de Ano no df
obesidgrau3_escolaridade_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(obesidgrau3_escolaridade_df_total))

# Exibir a tabela total
print(obesidgrau3_escolaridade_df_total)
################################## IGNORADO ###################################
table(tab_fato_vigitel$ignorado)
##################################### ano #####################################
# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Criar o nome da variável para o subset
  subset_name <- paste("vig.", tolower(ano_atual), sep="")
  
  # Criar o subset para cada ano
  assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual))
  
  # Criar o nome da variável para o objeto de design de pesquisa
  survey_name <- paste("survey.", tolower(ano_atual), sep="")
  
  # Criar o objeto de design de pesquisa para cada ano
  assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                weights=get(subset_name)$pesorake))
  
  # Criar o nome da variável para a tabela ponderada por ignorado
  ignorado_name <- paste("ignorado.", tolower(ano_atual), sep="")
  
  # Calcular a tabela ponderada por ignorado para cada ano
  ignorado_ano <- svytable(~ignorado, design=get(survey_name)) %>%
    prop.table() %>%
    multiply_by(100) %>%
    round(digits=2) %>%
    as.data.frame()
  
  # Adicionar o nome do ano ao dataframe
  ignorado_ano$ano <- ano_atual
  
  # Salvar o resultado na lista
  lista_anos[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- ignorado_ano
}

# Combinar todos os resultados de cada ano em um dataframe
ignorado_anos_df_total <- do.call(rbind, lista_anos)

# Adicionar nomes de coluna se o número de colunas for adequado
colnames(ignorado_anos_df_total) <- c("Ignorado", "Ignorado por Ano (%)", "Ano")

# Exibir a tabela total
print(ignorado_anos_df_total)

##################################### sexo ####################################
# Vetor de sexo
sexo <- unique(tab_fato_vigitel$sexo)

# Lista para armazenar os nomes das sexo
lista_sexo <- character(length(sexo))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_sexo <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada sexo
  resultados_sexo <- list()
  
  # Loop para cada sexo
  for (sexo_nome in sexo) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada sexo e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & sexo == sexo_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada sexo e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da sexo
    lista_sexo[sexo_nome] <- sexo_nome
    
    # Criar o nome da variável para a tabela ponderada por ignorado
    ignorado_name <- paste("ignorado.", tolower(sexo_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por ignorado para cada sexo e ano
    ignorado_sexo <- svytable(~ignorado, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da sexo ao dataframe
    ignorado_sexo$sexo <- sexo_nome
    
    # Salvar o resultado na lista
    resultados_sexo[[sexo_nome]] <- ignorado_sexo
  }
  
  # Combinar todos os resultados de cada sexo em um dataframe para o ano atual
  ignorado_sexo_df <- do.call(rbind, resultados_sexo)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(ignorado_sexo_df) <- c("Ignorado", "Ignorado por sexo (%)", "sexo")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_sexo[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- ignorado_sexo_df
}

# Unir todas as tabelas em uma única tabela
ignorado_sexo_df_total <- do.call(rbind, lista_anos_sexo)

#Acrescentando a coluna de Ano no df
ignorado_sexo_df_total$Ano <- rep(rep(anos, each = 4), length.out = nrow(ignorado_sexo_df_total))

# Exibir a tabela total
print(ignorado_sexo_df_total)

################################## faixa etária ###############################
# Vetor de faixas etárias
faixaeta <- unique(tab_fato_vigitel$faixaeta)

# Lista para armazenar os nomes das faixas etárias
lista_faixaeta <- character(length(faixaeta))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_faixaeta <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada faixa etária
  resultados_faixaeta <- list()
  
  # Loop para cada faixa etária
  for (faixaeta_nome in faixaeta) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada faixaeta e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & faixaeta == faixaeta_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada faixaeta e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da faixaeta
    lista_faixaeta[faixaeta_nome] <- faixaeta_nome
    
    # Criar o nome da variável para a tabela ponderada por ignorado
    ignorado_name <- paste("ignorado.", tolower(faixaeta_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por ignorado para cada faixaeta e ano
    ignorado_faixaeta <- svytable(~ignorado, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da faixaeta ao dataframe
    ignorado_faixaeta$faixaeta <- faixaeta_nome
    
    # Salvar o resultado na lista
    resultados_faixaeta[[faixaeta_nome]] <- ignorado_faixaeta
  }
  
  # Combinar todos os resultados de cada faixaeta em um dataframe para o ano atual
  ignorado_faixaeta_df <- do.call(rbind, resultados_faixaeta)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(ignorado_faixaeta_df) <- c("Ignorado", "Ignorado por Faixaeta (%)", "Faixaeta")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_faixaeta[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- ignorado_faixaeta_df
}

# Unir todas as tabelas em uma única tabela
ignorado_faixaeta_df_total <- do.call(rbind, lista_anos_faixaeta)

#Acrescentando a coluna de Ano no df
ignorado_faixaeta_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(ignorado_faixaeta_df_total))

# Exibir a tabela total
print(ignorado_faixaeta_df_total)

#################################### cidades ##################################
# Vetor de cidades
cidades <- unique(tab_fato_vigitel$cidade)

# Lista para armazenar os nomes das cidades
lista_cidades <- character(length(cidades))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cidades <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cidade
  resultados_cidades <- list()
  
  # Loop para cada cidade
  for (cidade_nome in cidades) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cidade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cidade == cidade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cidade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cidade
    lista_cidades[cidade_nome] <- cidade_nome
    
    # Criar o nome da variável para a tabela ponderada por ignorado
    ignorado_name <- paste("ignorado", tolower(cidade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por ignorado para cada cidade e ano
    ignorado_cidades <- svytable(~ignorado, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cidade ao dataframe
    ignorado_cidades$cidade <- cidade_nome
    
    # Salvar o resultado na lista
    resultados_cidades[[cidade_nome]] <- ignorado_cidades
  }
  
  # Combinar todos os resultados de cada cidade em um dataframe para o ano atual
  ignorado_cidades_df <- do.call(rbind, resultados_cidades)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(ignorado_cidades_df) <- c("Ignorado", "Ignorado por Cidade (%)", "Cidade")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cidades[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- ignorado_cidades_df
}

# Unir todas as tabelas em uma única tabela
ignorado_cidades_df_total <- do.call(rbind, lista_anos_cidades)

#Acrescentando a coluna de Ano no df
ignorado_cidades_df_total$Ano <- rep(rep(anos, each = 54), length.out = nrow(ignorado_cidades_df_total))

# Exibir a tabela total
print(ignorado_cidades_df_total)

#################################### cor/raça #################################
# Vetor de cor_raca
cor_raca <- unique(tab_fato_vigitel$cor_raca)

# Lista para armazenar os nomes das cor_raca
lista_cor_raca <- character(length(cor_raca))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_cor_raca <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada cor_raca
  resultados_cor_raca <- list()
  
  # Loop para cada cor_raca
  for (cor_raca_nome in cor_raca) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada cor_raca e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & cor_raca == cor_raca_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada cor_raca e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da cor_raca
    lista_cor_raca[cor_raca_nome] <- cor_raca_nome
    
    # Criar o nome da variável para a tabela ponderada por ignorado
    ignorado_name <- paste("ignorado.", tolower(cor_raca_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por ignorado para cada cor_raca e ano
    ignorado_cor_raca <- svytable(~ignorado, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da cor_raca ao dataframe
    ignorado_cor_raca$cor_raca <- cor_raca_nome
    
    # Salvar o resultado na lista
    resultados_cor_raca[[cor_raca_nome]] <- ignorado_cor_raca
  }
  
  # Combinar todos os resultados de cada cor_raca em um dataframe para o ano atual
  ignorado_cor_raca_df <- do.call(rbind, resultados_cor_raca)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(ignorado_cor_raca_df) <- c("Ignorado", "Ignorado_por_cor_raca", "cor_raca")
  
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_cor_raca[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- ignorado_cor_raca_df
}

# Unir todas as tabelas em uma única tabela
ignorado_cor_raca_df_total <- do.call(rbind, lista_anos_cor_raca)

#Acrescentando a coluna de Ano no df
ignorado_cor_raca_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(ignorado_cor_raca_df_total))

# Exibir a tabela total
print(ignorado_cor_raca_df_total)

################################## escolaridade ###############################
# Vetor de escolaridade
escolaridade <- unique(tab_fato_vigitel$escolar)

# Lista para armazenar os nomes das escolaridade
lista_escolaridade <- character(length(escolaridade))

# Vetor de anos
anos <- unique(tab_fato_vigitel$ano)

# Lista para armazenar os resultados de cada ano
lista_anos_escolaridade <- list()

# Loop para cada ano
for (ano_atual in anos) {
  # Lista para armazenar os resultados de cada escolaridade
  resultados_escolaridade <- list()
  
  # Loop para cada escolaridade
  for (escolaridade_nome in escolaridade) {
    # Criar o nome da variável para o subset
    subset_name <- paste("vig.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o subset para cada escolaridade e ano
    assign(subset_name, subset(tab_fato_vigitel, ano == ano_atual & escolar == escolaridade_nome))
    
    # Criar o nome da variável para o objeto de design de pesquisa
    survey_name <- paste("survey.", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Criar o objeto de design de pesquisa para cada escolaridade e ano
    assign(survey_name, svydesign(ids=~1, data=get(subset_name),
                                  weights=get(subset_name)$pesorake))
    
    # Armazenar o nome da escolaridade
    lista_escolaridade[escolaridade_nome] <- escolaridade_nome
    
    # Criar o nome da variável para a tabela ponderada por ignorado
    ignorado_name <- paste("ignorado", tolower(escolaridade_nome), ".", ano_atual, sep="")
    
    # Calcular a tabela ponderada por ignorado para cada escolaridade e ano
    ignorado_escolaridade <- svytable(~ignorado, design=get(survey_name)) %>%
      prop.table() %>%
      multiply_by(100) %>%
      round(digits=2) %>%
      as.data.frame()
    
    # Adicionar o nome da escolaridade ao dataframe
    ignorado_escolaridade$escolar <- escolaridade_nome
    
    # Salvar o resultado na lista
    resultados_escolaridade[[escolaridade_nome]] <- ignorado_escolaridade
  }
  
  # Combinar todos os resultados de cada escolaridade em um dataframe para o ano atual
  ignorado_escolaridade_df <- do.call(rbind, resultados_escolaridade)
  
  # Adicionar nomes de coluna se o número de colunas for adequado
  colnames(ignorado_escolaridade_df) <- c("Ignorado", "Ignorado por escolaridade (%)", "escolaridade")
 
  # Adicionar o dataframe do ano atual à lista de resultados
  lista_anos_escolaridade[[paste("resultados", ano_atual, "dataframe", sep=".")]] <- ignorado_escolaridade_df
}

# Unir todas as tabelas em uma única tabela
ignorado_escolaridade_df_total <- do.call(rbind, lista_anos_escolaridade)

#Acrescentando a coluna de Ano no df
ignorado_escolaridade_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(ignorado_escolaridade_df_total))

# Exibir a tabela total
print(ignorado_escolaridade_df_total)

########################### CRIANDO TABELAS FINAIS ############################
##################################### ano #####################################

pb_anos_filtrado <- filter(pesobaixo_anos_df_total, Peso_Baixo == 1)
ef_anos_filtrado <- filter(eutrof_anos_df_total, Eutrofia == 1)
sp_anos_filtrado <- filter(sobrepeso_anos_df_total, Sobrepeso == 1)
ob_anos_filtrado <- filter(obesid_anos_df_total, Obesidade == 1)
ob1_anos_filtrado <- filter(obesidgrau1_anos_df_total, Obesid_Grau_1 == 1)
ob2_anos_filtrado <- filter(obesidgrau2_anos_df_total, Obesid_Grau_2 == 1)
ob3_anos_filtrado <- filter(obesidgrau3_anos_df_total, Obesid_Grau_3 == 1)
ig_anos_filtrado <- filter(ignorado_anos_df_total, Ignorado == 1)

tbl_vig_ano <- merge(pb_anos_filtrado, ef_anos_filtrado, by ='Ano') %>%
  merge(sp_anos_filtrado, by = 'Ano') %>%
  merge(ob_anos_filtrado, by = 'Ano') %>%
  merge(ob1_anos_filtrado, by = 'Ano') %>%
  merge(ob2_anos_filtrado, by = 'Ano') %>%
  merge(ob3_anos_filtrado, by = 'Ano') %>%
  merge(ig_anos_filtrado, by = 'Ano')

view(tbl_vig_ano)
  
#################################### sexo #####################################
pb_sexo_filtrado <- filter(pesobaixo_sexo_df_total, Peso_Baixo == 1)
ef_sexo_filtrado <- filter(eutrofia_sexo_df_total, Eutrofia == 1)
sp_sexo_filtrado <- filter(sobrepeso_sexo_df_total, Sobrepeso == 1)
ob_sexo_filtrado <- filter(obesid_sexo_df_total, Obesidade == 1)
ob1_sexo_filtrado <- filter(obesidgrau1_sexo_df_total, Obesid_Grau_1 == 1)
ob2_sexo_filtrado <- filter(obesidgrau2_sexo_df_total, Obesid_Grau_2 == 1)
ob3_sexo_filtrado <- filter(obesidgrau3_sexo_df_total, Obesid_Grau_3 == 1)
ig_sexo_filtrado <- filter(ignorado_sexo_df_total, Ignorado == 1)

tbl_vig_sexo <- merge(pb_sexo_filtrado, ef_sexo_filtrado, by = c('Ano', 'sexo')) %>%
  merge(sp_sexo_filtrado, by = c('Ano', 'sexo')) %>%
  merge(ob_sexo_filtrado, by = c('Ano', 'sexo')) %>%
  merge(ob1_sexo_filtrado, by = c('Ano', 'sexo')) %>%
  merge(ob2_sexo_filtrado, by = c('Ano', 'sexo')) %>%
  merge(ob3_sexo_filtrado, by = c('Ano', 'sexo')) %>%
  merge(ig_sexo_filtrado, by = c('Ano', 'sexo'))

view(tbl_vig_sexo)

################################ faixa etária #################################
pb_faixaeta_filtrado <- filter(pesobaixo_faixaeta_df_total, Peso_Baixo == 1)
ef_faixaeta_filtrado <- filter(eutrofia_faixaeta_df_total, Eutrofia == 1)
sp_faixaeta_filtrado <- filter(sobrepeso_faixaeta_df_total, Sobrepeso == 1)
ob_faixaeta_filtrado <- filter(obesid_faixaeta_df_total, Obesidade == 1)
ob1_faixaeta_filtrado <- filter(obesidgrau1_faixaeta_df_total, Obesid_Grau_1 == 1)
ob2_faixaeta_filtrado <- filter(obesidgrau2_faixaeta_df_total, Obesid_Grau_2 == 1)
ob3_faixaeta_filtrado <- filter(obesidgrau3_faixaeta_df_total, Obesid_Grau_3 == 1)
ig_faixaeta_filtrado <- filter(ignorado_faixaeta_df_total, Ignorado == 1)

tbl_vig_faixaeta <- merge(pb_faixaeta_filtrado, ef_faixaeta_filtrado, by = c('Ano', 'Faixaeta')) %>%
  merge(sp_faixaeta_filtrado, by = c('Ano', 'Faixaeta')) %>%
  merge(ob_faixaeta_filtrado, by = c('Ano', 'Faixaeta')) %>%
  merge(ob1_faixaeta_filtrado, by = c('Ano', 'Faixaeta')) %>%
  merge(ob2_faixaeta_filtrado, by = c('Ano', 'Faixaeta')) %>%
  merge(ob3_faixaeta_filtrado, by = c('Ano', 'Faixaeta')) %>%
  merge(ig_faixaeta_filtrado, by = c('Ano', 'Faixaeta'))

view(tbl_vig_faixaeta)
################################### cidades ###################################
pb_cidades_filtrado <- filter(pesobaixo_cidades_df_total, Peso_Baixo == 1)
ef_cidades_filtrado <- filter(eutrofia_cidades_df_total, Eutrofia == 1)
sp_cidades_filtrado <- filter(sobrepeso_cidades_df_total, Sobrepeso == 1)
ob_cidades_filtrado <- filter(obesid_cidades_df_total, Obesidade == 1)
ob1_cidades_filtrado <- filter(obesidgrau1_cidades_df_total, Obesid_Grau_1 == 1)
ob2_cidades_filtrado <- filter(obesidgrau2_cidades_df_total, Obesid_Grau_2 == 1)
ob3_cidades_filtrado <- filter(obesidgrau3_cidades_df_total, Obesid_Grau_3 == 1)
ig_cidades_filtrado <- filter(ignorado_cidades_df_total, Ignorado == 1)

tbl_vig_cidades <- merge(pb_cidades_filtrado, ef_cidades_filtrado, by = c('Ano', 'Cidade')) %>%
  merge(sp_cidades_filtrado, by = c('Ano', 'Cidade')) %>%
  merge(ob_cidades_filtrado, by = c('Ano', 'Cidade')) %>%
  merge(ob1_cidades_filtrado, by = c('Ano', 'Cidade')) %>%
  merge(ob2_cidades_filtrado, by = c('Ano', 'Cidade')) %>%
  merge(ob3_cidades_filtrado, by = c('Ano', 'Cidade')) %>%
  merge(ig_cidades_filtrado, by = c('Ano', 'Cidade'))

view(tbl_vig_cidades)
################################## cor/raça ###################################
pb_cor_raca_filtrado <- filter(pesobaixo_cor_raca_df_total, Peso_Baixo == 1)
ef_cor_raca_filtrado <- filter(eutrofia_cor_raca_df_total, Eutrofia == 1)
sp_cor_raca_filtrado <- filter(sobrepeso_cor_raca_df_total, Sobrepeso == 1)
ob_cor_raca_filtrado <- filter(obesid_cor_raca_df_total, Obesidade == 1)
ob1_cor_raca_filtrado <- filter(obesidgrau1_cor_raca_df_total, Obesid_Grau_1 == 1)
ob2_cor_raca_filtrado <- filter(obesidgrau2_cor_raca_df_total, Obesid_Grau_2 == 1)
ob3_cor_raca_filtrado <- filter(obesidgrau3_cor_raca_df_total, Obesid_Grau_3 == 1)
ig_cor_raca_filtrado <- filter(ignorado_cor_raca_df_total, Ignorado == 1)

# Peso baixo, obesidade grau 3 e ignorado possuem valores de 100%, por isso são adicionadas as respectivas linhas de '0%' manualmente

#Acrescentando a coluna de Ano no df
ignorado_cor_raca_df_total$Ano <- rep(rep(anos, each = 12), length.out = nrow(ignorado_cor_raca_df_total))

# Criando novas linhas manualmente
nova_linha_pb_cr <- data.frame(Peso_Baixo = 1, Peso_Baixo_por_cor_raca= 0, cor_raca = "Indígena", Ano = 2010)
nova_linha_ob3_cr_1 <- data.frame(Obesid_Grau_3 = 1, Obesid_Grau_3_por_cor_raca = 0, cor_raca = "Indígena", Ano = 2008)
nova_linha_ob3_cr_2 <- data.frame(Obesid_Grau_3 = 1, Obesid_Grau_3_por_cor_raca = 0, cor_raca = "Indígena", Ano = 2006)
nova_linha_ob3_cr_3 <- data.frame(Obesid_Grau_3 = 1, Obesid_Grau_3_por_cor_raca = 0, cor_raca = "Parda", Ano = 2006)
nova_linha_ob3_cr_4 <- data.frame(Obesid_Grau_3 = 1, Obesid_Grau_3_por_cor_raca = 0, cor_raca = "Indígena", Ano = 2009)
nova_linha_ob3_cr_5 <- data.frame(Obesid_Grau_3 = 1, Obesid_Grau_3_por_cor_raca = 0, cor_raca = "Parda", Ano = 2009)
nova_linha_ig_cr_1 <- data.frame(Ignorado = 1, Ignorado_por_cor_raca = 0, cor_raca = "Indígena", Ano = 2008)
nova_linha_ig_cr_2 <- data.frame(Ignorado = 1, Ignorado_por_cor_raca = 0, cor_raca = "Indígena", Ano = 2007)
nova_linha_ig_cr_3 <- data.frame(Ignorado = 1, Ignorado_por_cor_raca = 0, cor_raca = "Indígena", Ano = 2010)
nova_linha_ig_cr_4 <- data.frame(Ignorado = 1, Ignorado_por_cor_raca = 0, cor_raca = "Ignorado", Ano = 2009)

# Concatenando as linhas manualmente aos respectivos df
pb_cor_raca_filtrado <- rbind(pb_cor_raca_filtrado, nova_linha_pb_cr)
ob3_cor_raca_filtrado <- rbind(ob3_cor_raca_filtrado, nova_linha_ob3_cr_1,nova_linha_ob3_cr_2,
                               nova_linha_ob3_cr_3, nova_linha_ob3_cr_4, nova_linha_ob3_cr_5)
ig_cor_raca_filtrado <- rbind(ig_cor_raca_filtrado, nova_linha_ig_cr_1,nova_linha_ig_cr_2,
                              nova_linha_ig_cr_3, nova_linha_ig_cr_4)

# Modificando manualmente alguns valores na coluna "Ano" de pb_cor_raca_filtrado
view(ig_cor_raca_filtrado)

ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.22 & ig_cor_raca_filtrado$Ano == 2007] <- 2011
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.95 & ig_cor_raca_filtrado$Ano == 2011] <- 2010
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.06 & ig_cor_raca_filtrado$Ano == 2010] <- 2008
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.27 & ig_cor_raca_filtrado$Ano == 2008] <- 2006
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.73 & ig_cor_raca_filtrado$Ano == 2008] <- 2006
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.59 & ig_cor_raca_filtrado$Ano == 2006] <- 2013
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.35 & ig_cor_raca_filtrado$Ano == 2006] <- 2013
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.62 & ig_cor_raca_filtrado$Ano == 2013] <- 2023
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 0.70 & ig_cor_raca_filtrado$Ano == 2013] <- 2023
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.10 & ig_cor_raca_filtrado$Ano == 2023] <- 2020
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 0.22 & ig_cor_raca_filtrado$Ano == 2023] <- 2020
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.38 & ig_cor_raca_filtrado$Ano == 2020] <- 2021
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 0.03 & ig_cor_raca_filtrado$Ano == 2020] <- 2021
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.94 & ig_cor_raca_filtrado$Ano == 2021] <- 2014
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.15 & ig_cor_raca_filtrado$Ano == 2021] <- 2014
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.06 & ig_cor_raca_filtrado$Ano == 2014] <- 2019
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.83 & ig_cor_raca_filtrado$Ano == 2014] <- 2019
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.76 & ig_cor_raca_filtrado$Ano == 2019] <- 2015
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.59 & ig_cor_raca_filtrado$Ano == 2019] <- 2015
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.93 & ig_cor_raca_filtrado$Ano == 2015] <- 2016
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.58 & ig_cor_raca_filtrado$Ano == 2015] <- 2016
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.00 & ig_cor_raca_filtrado$Ano == 2016] <- 2017
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.75 & ig_cor_raca_filtrado$Ano == 2016] <- 2017
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.99 & ig_cor_raca_filtrado$Ano == 2017] <- 2012
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.04 & ig_cor_raca_filtrado$Ano == 2017] <- 2012                           
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Branca" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 1.69 & ig_cor_raca_filtrado$Ano == 2012] <- 2018
ig_cor_raca_filtrado$Ano[ig_cor_raca_filtrado$cor_raca == "Amarela" & ig_cor_raca_filtrado$Ignorado_por_cor_raca == 2.36 & ig_cor_raca_filtrado$Ano == 2012] <- 2018

# Modificando manualmente alguns valores na coluna "Ano" de ob3_cor_raca_filtrado
view(ob3_cor_raca_filtrado)

ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 0.88 & ob3_cor_raca_filtrado$Ano == 2009] <- 2007
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.04 & ob3_cor_raca_filtrado$Ano == 2007] <- 2011
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 0.92 & ob3_cor_raca_filtrado$Ano == 2011] <- 2010
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 0.74 & ob3_cor_raca_filtrado$Ano == 2010] <- 2008
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.17 & ob3_cor_raca_filtrado$Ano == 2008] <- 2006
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.40 & ob3_cor_raca_filtrado$Ano == 2006] <- 2013
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.51 & ob3_cor_raca_filtrado$Ano == 2006] <- 2013
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 2.31 & ob3_cor_raca_filtrado$Ano == 2013] <- 2023
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 3.35 & ob3_cor_raca_filtrado$Ano == 2013] <- 2023
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.57 & ob3_cor_raca_filtrado$Ano == 2023] <- 2020
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 0.53 & ob3_cor_raca_filtrado$Ano == 2023] <- 2020
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.76 & ob3_cor_raca_filtrado$Ano == 2020] <- 2021
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 0.64 & ob3_cor_raca_filtrado$Ano == 2020] <- 2021
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.29 & ob3_cor_raca_filtrado$Ano == 2021] <- 2014
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.36 & ob3_cor_raca_filtrado$Ano == 2021] <- 2014
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.48 & ob3_cor_raca_filtrado$Ano == 2014] <- 2019
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 0.63 & ob3_cor_raca_filtrado$Ano == 2014] <- 2019
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.22 & ob3_cor_raca_filtrado$Ano == 2019] <- 2015
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.11 & ob3_cor_raca_filtrado$Ano == 2019] <- 2015
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.38 & ob3_cor_raca_filtrado$Ano == 2015] <- 2016
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca ==1.31 & ob3_cor_raca_filtrado$Ano == 2015] <- 2016
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.57 & ob3_cor_raca_filtrado$Ano == 2016] <- 2017
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca ==1.15 & ob3_cor_raca_filtrado$Ano == 2016] <- 2017
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.72 & ob3_cor_raca_filtrado$Ano == 2017] <- 2012
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca ==0.91 & ob3_cor_raca_filtrado$Ano == 2017] <- 2012
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Branca" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca == 1.59 & ob3_cor_raca_filtrado$Ano == 2012] <- 2018
ob3_cor_raca_filtrado$Ano[ob3_cor_raca_filtrado$cor_raca == "Amarela" & ob3_cor_raca_filtrado$Obesid_Grau_3_por_cor_raca ==1.84 & ob3_cor_raca_filtrado$Ano == 2012] <- 2018

tbl_vig_cor_raca <- merge(pb_cor_raca_filtrado, ef_cor_raca_filtrado, by = c('Ano', 'cor_raca')) %>%
  merge(sp_cor_raca_filtrado, by = c('Ano', 'cor_raca')) %>%
  merge(ob_cor_raca_filtrado, by = c('Ano', 'cor_raca')) %>%
  merge(ob1_cor_raca_filtrado, by = c('Ano', 'cor_raca')) %>%
  merge(ob2_cor_raca_filtrado, by = c('Ano', 'cor_raca')) %>%
  merge(ob3_cor_raca_filtrado, by = c('Ano', 'cor_raca')) %>%
  merge(ig_cor_raca_filtrado, by = c('Ano', 'cor_raca'))

view(tbl_vig_cor_raca)
################################# escolaridade ################################
pb_escolaridade_filtrado <- filter(pesobaixo_escolaridade_df_total, Peso_Baixo == 1)
ef_escolaridade_filtrado <- filter(eutrofia_escolaridade_df_total, Eutrofia == 1)
sp_escolaridade_filtrado <- filter(sobrepeso_escolaridade_df_total, Sobrepeso == 1)
ob_escolaridade_filtrado <- filter(obesid_escolaridade_df_total, Obesidade == 1)
ob1_escolaridade_filtrado <- filter(obesidgrau1_escolaridade_df_total, Obesid_Grau_1 == 1)
ob2_escolaridade_filtrado <- filter(obesidgrau2_escolaridade_df_total, Obesid_Grau_2 == 1)
ob3_escolaridade_filtrado <- filter(obesidgrau3_escolaridade_df_total, Obesid_Grau_3 == 1)
ig_escolaridade_filtrado <- filter(ignorado_escolaridade_df_total, Ignorado == 1)

# Peso baixo e obesidade grau 2 possuem valores de 100%, por isso são adicionadas as respectivas linhas de '0%' manualmente

# Criando novas linhas manualmente
nova_linha_pb_esc <- data.frame(Peso_Baixo = 1, Peso_Baixo_por_escolaridade = 0, escolaridade = "Ensino Médio", Ano = 2006)
nova_linha_ob2_esc <- data.frame(Obesid_Grau_2 = 1, Obesid_Grau_2_por_escolaridade = 0, escolaridade = "Ensino Médio", Ano = 2006)
# Concatenando as linhas manualmente aos respectivos df
pb_escolaridade_filtrado <- rbind(pb_escolaridade_filtrado, nova_linha_pb_esc)
ob2_escolaridade_filtrado <- rbind(ob2_escolaridade_filtrado, nova_linha_ob2_esc)

view(pb_escolaridade_filtrado)
view(ob2_escolaridade_filtrado)


tbl_vig_escolaridade <- merge(pb_escolaridade_filtrado, ef_escolaridade_filtrado, by = c('Ano', 'escolaridade')) %>%
  merge(sp_escolaridade_filtrado, by = c('Ano', 'escolaridade')) %>%
  merge(ob_escolaridade_filtrado, by = c('Ano', 'escolaridade')) %>%
  merge(ob1_escolaridade_filtrado, by = c('Ano', 'escolaridade')) %>%
  merge(ob2_escolaridade_filtrado, by = c('Ano', 'escolaridade')) %>%
  merge(ob3_escolaridade_filtrado, by = c('Ano', 'escolaridade')) %>%
  merge(ig_escolaridade_filtrado, by = c('Ano', 'escolaridade'))


############################### SALVANDO TABELAS ##############################
write.csv(tbl_vig_ano, "C:/Users/João/Desktop/Estágio Cordial/tabelas finais vigitel/tbl_vig_ano.csv", row.names = TRUE)
write.csv(tbl_vig_sexo, "C:/Users/João/Desktop/Estágio Cordial/tabelas finais vigitel/tbl_vig_sexo.csv", row.names = TRUE)
write.csv(tbl_vig_faixaeta, "C:/Users/João/Desktop/Estágio Cordial/tabelas finais vigitel/tbl_vig_faixaeta.csv", row.names = TRUE)
write.csv(tbl_vig_cidades, "C:/Users/João/Desktop/Estágio Cordial/tabelas finais vigitel/tbl_vig_cidades.csv", row.names = TRUE)
write.csv(tbl_vig_cor_raca, "C:/Users/João/Desktop/Estágio Cordial/tabelas finais vigitel/tbl_vig_cor_raca.csv", row.names = TRUE)
write.csv(tbl_vig_escolaridade, "C:/Users/João/Desktop/Estágio Cordial/tabelas finais vigitel/tbl_vig_escolaridade.csv", row.names = TRUE)



