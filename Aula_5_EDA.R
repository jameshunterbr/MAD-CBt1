## Cálculos para Aula 5
## Estatística Exploratória
## 21/2/2017

## Carregar pacotes
  library(tidyverse)
  library(DescTools)
  library(knitr)
  library(forcats)
  options(scipen = 1000)

## Carregar Dados Exemplares -- Cegueira glaucomatosa

raca <- c("branco", "naobranco")
pop <- c(32930233, 3933333)
casos <- c(2832, 3227)
vars <- c("raca", "pop", "casos")
cegGlauc <- tibble(raca, pop, casos)
colnames(cegGlauc) <- vars
str(cegGlauc)
kable(cegGlauc)

## Calcular Prevalência por Grupo

cegGlauc <- cegGlauc %>% mutate(prop = casos/pop)
kable(cegGlauc)

## Calcular Prevalência por 100.000 

cegGlauc <- cegGlauc %>% mutate(prop100mil = prop*100000)
kable(cegGlauc)

## Carregar Câncer Cervical

Estado <- c("doente", "saudável")
Neg <- c(23362, 225)
Pos <- c(362, 154)
vars <- c("Estado", "Neg", "Pos")
cervCan <- tibble(Estado, Neg, Pos)
colnames(cervCan) <- vars

## Crescimento de Genotipagens

genotip <- read.csv("yr_genotipagem.csv") %>% 
  filter(yr >= 2010) %>%
  count(yr)
kable(genotip, captions = "Genotipagens por Ano")
taxacres <- 100 * (genotip$n[genotip$yr == 2015] - 
                   genotip$n[genotip$yr == 2010]) / 
                  genotip$n[genotip$yr == 2010] 

## Taxa Function - Funções ajudam bastante com tarefas repetitivas
### pct é TRUE ou FALSE se você quer ou não quer taxa em forma de pct; default = TRUE

taxa <- function(oldval, newval, pct = TRUE) {
  p <- ifelse(pct, 100, 1)
  t <- p * (newval - oldval) / oldval
  return(t)
}

taxa(genotip$n[genotip$yr == 2010], genotip$n[genotip$yr == 2015])

## CHD e Fumar

framCHD <- read_csv("framingham_ex1.csv") %>%
  mutate(fumante = factor(currentSmoker)) %>%
  mutate(CHD10Anos = factor(TenYearCHD)) %>%
  mutate(fumante = fct_recode(fumante, "Sim" = "1", "Nao" = "0")) %>%
  mutate(CHD10Anos = fct_recode(CHD10Anos, "Sim" = "1", "Nao" = "0")) %>%
  select(fumante, CHD10Anos)
(tabCHD <- table(framCHD))

# Calcular linha (fumante) e coluna (CHD) totais 
fumtots <-  rowSums(tabCHD)
CHDtots <- colSums(tabCHD)

suppressWarnings(tabCHD)
print(paste("Fumantes Total:", "Não = ", fumtots[1], "| Sim = ", fumtots[2]))
print(paste("CHD 10 Anos Total:", "Não = ", CHDtots[1], "| Sim = ", CHDtots[2]))

# Calcular Incidências e risco
fumincid <- tabCHD[2,2]/fumtots[2]
naofumincid <- tabCHD[1,2]/fumtots[1]
rr <- fumincid / naofumincid

## Distribuição de Frequências

peso_lb <- c(68, 63, 42, 27, 30, 36, 28, 22, 23, 24, 25, 44, 
          65, 43, 36, 42, 28, 31, 28, 25, 45, 12, 32, 49,
          38, 42, 27, 31, 16, 24, 69, 47, 23, 22, 43, 23,
          19, 46, 30, 43, 49, 12, 32, 79, 27, 25, 74, 51,
          12, 57, 51, 50, 38, 21, 27, 49, 28)

mean(peso_lb)
median(peso_lb)
range(peso_lb)
IQR(peso_lb)
sd(peso_lb)
Desc(peso_lb, plotit = FALSE)

## Mode Function -- R has another function mode that does something else useful

modex <- function(x) { 
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

modex(peso_lb)
sum(peso_lb == modex(peso_lb))

## Transformações

dados <- read_csv("pac_demo.csv")
testes <- dados %>% select(c(codepac, copias_cv:contagem_cd8))
Desc(testes$copias_cv, plotit = TRUE) # original
Desc(sqrt(testes$copias_cv), main = "Raiz Quadrado de Cópias CV", plotit = TRUE)
Desc(log10(testes$copias_cv), main = "Log Base 10 de Cópias CV", plotit = TRUE)