## Arquivo com os Blocos ("Chunks") de Código para
## Aula 2
## 10 de fevereiro de 2017
## James Hunter

## Chunk 1 - Achar o Working Directory

getwd()

## Chunks 2, 3, 4 -- Carregar Pacote tidyverse e mostrar o que
## foi carregado com sessionInfo

sessionInfo()
library(tidyverse)
sessionInfo()

## Chunk 5 -- Carregar dados

dados <- read_csv(file = "artrite_data/inflammation-01.csv", 
                  col_names = FALSE)

## Chunk 6 -- Determinar a Estrutura de dados

str(dados[, 1:5])

## Chunk 7 -- Exemplos Simples de Assignment

## 1ª Versão
peso <- 55  ## Pessoa pesa 55 kg.

## 2ª Versão
peso_kg <- 55 ## Mais claro

## Pode Converter à Libra
(peso_lb <- peso_kg * 2.2)
peso_lb

## Chunk 8 -- Qual Tipo de Dados Temos?

class(dados)
dim(dados)

## Chunk 9 -- Subset por Índice

dados[1, 1]
dados[20, 20] # valor de linha 20, coluna 20
dados[1:3, 1:5] # valores dos primeiros 3 pacientes para primeiro 5 dias

## Chunk 10 -- Subsets dos Dados Não-Contiguos

dados[5:8, 6:9] # valores dos pacientes 5 - 8 para dias 6 até 9

## Chunk 11 -- Nomes nas Colunas

## vetor dos dias
dias <- c(paste0("dia", 1:ncol(dados)))

## vetor dos pacientes
pacientes <- c(paste0("pac", 1:nrow(dados)))

## Colocar os nomes nas variáveis
colnames(dados) <- dias

## Colocar os IDs dos pacientes na base
dados <- bind_cols(tibble(pacientes), dados)

## Chunk 12 -- Resultado

dados[1:6, 1:8]

## Chunk 13 -- as.numeric a uma coluna única

dados[, 'dia1'] <- as.numeric(unlist(dados[, 'dia1']))
str(dados[, 'dia1'])

## Chunk 14 -- Índice por Nome

str(dados$dia1)
## Retornar os IDs dos primeiros 10 pacientes
dados$pacientes[1:10] # Pode combinar os nomes das variáveis com
                      # índices númericos para as linhas/os casos 

## Chunk 15 -- Aplicar `as.numeric` às Variáveis de Dia

dados <- dados %>% mutate_at(vars(dia1:dia40), funs(as.numeric))
str(dados[, 2:6])

## Estatística Começa
## Chunk 16 -- Descrever os Dados 

dadosDia <- dados %>% select(-pacientes) 
## "-" em selecionar quer dizer omitir variável

## Create variable/vector for Paciente 1
pac1 <- dadosDia %>% slice(1) %>% unlist()
max(pac1)
mean(pac1)

## Chunk 17 -- Resumo das Médias das Variáveis Dia

meanDia <- dadosDia %>% summarize_all(mean)
unlist(meanDia)

## Chunk 18, 19 -- Gráfico BÁSICO das Médias por Dia

plot(unlist(meanDia), main = "Inflamação Média por Dia", 
     ylab = "Média de Inflamação", xlab = "Dia")

## Chunk 20 -- Gráfico de Paciente 1

plot(pac1, main = "Plot da Inflamação de Paciente 1", 
     ylab = "Grau de Inflamação", xlab = "Dia")

## Chunk 21, 22 -- Boxplot de Inflamação do Paciente 1

boxplot(pac1, main = "Boxplot da Inflamação de Paciente 1", 
        ylab = "Grau de Inflamação")

## Chunk 23 -- `summary` e `IQR` de Paciente 1

summary(pac1)
IQR(pac1)

