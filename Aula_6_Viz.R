## Aula 6 - Vizualização dos Dados
## James hunter
## 24/2/17

## Carregar modulos

suppressMessages(library(tidyverse))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(nycflights13)) #pacote de dados
suppressPackageStartupMessages(library(forcats))
options(scipen = 1000)


## Carregar dados

testes <- read_csv("pac_demo.csv") %>%
          mutate(logcv = log10(copias_cv)) %>%  
          select(c(codepac, logcv, contagem_cd4, contagem_cd8))

## Gráfico 1 - cd4 x logcv

cvcd4 <- ggplot(data = testes, aes(x = contagem_cd4, y = logcv )) + geom_point()
cvcd4  # Só precisar chamar o gráfico para mostrar ele


## Gráfico 2 - cd4 x cd8

cd4cd8 <- ggplot(data = testes, aes(x = contagem_cd4, y = contagem_cd8)) + geom_point()
cd4cd8 <- cd4cd8 + labs(x = "Contagem de Celulas CD4+/ml", 
                        y = "Contagem de Celulas CD8+/ml", 
                        title = "Contagem de Celulas T CD4+ e CD8+ na Amostra")
cd4cd8

## Coloque gênero de volta para ver diferença entre gêneros

testessexo <- read_csv("pac_demo.csv") %>%
  mutate(logcv = log10(copias_cv)) %>%
  select(c(codepac, sexo, logcv, contagem_cd4, contagem_cd8))

## Gráfico de contagens com gênero

grcdsex <- ggplot(data = testessexo, aes(x = contagem_cd4, 
                                         y = contagem_cd8,
                                         colour = sexo)) + geom_point()
grcdsex <- grcdsex + labs(x = "Contagem de Celulas CD4+/ml", 
                          y = "Contagem de Celulas CD8+/ml", 
                          title = "Contagem de Celulas T CD4+ e CD8+ na Amostra",
                          colour = "Gênero")
grcdsex

## Overplotting com Flights

# Versão Raw

data(flights)
AKvoos <- flights %>% filter(carrier == "AS") 
grAK <- ggplot(data = AKvoos, aes(x = dep_delay, y = arr_delay)) + geom_point()
grAK

# Versão alpha
grAK2 <- ggplot(data = AKvoos, aes(x = dep_delay, y = arr_delay)) + 
  geom_point(alpha = 0.2)
grAK2

# Versão Jitter
grAK3 <- ggplot(data = AKvoos, aes(x = dep_delay, y = arr_delay)) +
  geom_jitter(width = 30, height = 30)
grAK3

## Dados de Tempo

data(weather)
jantemp <- weather %>% filter(origin == "EWR" & month == 1)
janjul <- weather %>% 
  filter(month %in% c(1, 7)) %>%
  mutate(mes = ifelse(month == 1, "jan", "jul")) %>%
  mutate(mes = factor(mes)) %>%
  select(origin, mes, temp, dewp)

## Jan Temperatura em EWR

ggplot(data = jantemp, aes(x = time_hour, y = temp)) + geom_line()

## Mostra temperaturas com linha de congelamento

ggplot(data = jantemp, aes(x = time_hour, y = temp)) + 
  geom_line() + 
  geom_hline(aes(yintercept = 32), linetype = 2, color = "blue")

## Histograma de `logcv`

ggplot(data = testes, mapping = aes(x = logcv)) + geom_histogram()

## Reduzir o Número de Bins para 10

ggplot(data = testes, mapping = aes(x = logcv)) + 
  geom_histogram(bins = 10, color = "white")

## Ajustar o Intervalo de `binwidth` para 1

ggplot(data = testes, mapping = aes(x = logcv)) + 
  geom_histogram(binwidth = 1, color = "white")

## Curva de Densidade Sozinho

ggplot(data = testes, mapping = aes(x = logcv)) + geom_density()

## Para Tirar a Linha ao Fundo, Use `geom_linha` 

ggplot(data = testes, mapping = aes(x = logcv)) + geom_line(stat = "density")


## Para Combinar com Histograma, Use Versão `geom_line(stat = "density")`

ggplot(data = testes, mapping = aes(x = logcv, y = ..density..)) + geom_histogram(color = "white") + geom_line(stat = "density", color = "red")


## Kernel Bandwidth

ggplot(data = testes, mapping = aes(x = logcv)) + 
  geom_line(stat = "density", adjust = 0.5, color = "red") +
  geom_line(stat = "density", adjust = 1.0) +
  geom_line(stat = "density", adjust = 2.0, color = "blue")

## Facets

ggplot(data = testessexo, mapping = aes(x = logcv)) + 
  geom_histogram(color = "white") + 
  facet_wrap(~ sexo)

## Boxplot de CD4

ggplot(data = testes, mapping = aes(x = 1, y = contagem_cd4)) + geom_boxplot()

## Boxplot de 2 Níveis de Gênero sobre CD4

ggplot(data = testessexo, mapping = aes(x = sexo, y = contagem_cd4)) + 
  geom_boxplot()

## Média no Boxplot

ggplot(data = testessexo, mapping = aes(x = sexo, y = contagem_cd4)) +
  geom_boxplot() + stat_summary(fun.y = "mean", 
                                geom = "point", shape = 23, size = 3, fill = "white")

## Dados no Boxplot

ggplot(data = testessexo, mapping = aes(x = sexo, y = contagem_cd4)) +
  geom_boxplot() + stat_summary(fun.y = "mean", 
                                geom = "point", shape = 23, size = 3, fill = "white") + 
  geom_point(color = "darkblue")

## Gráfico de Barras

ggplot(data = testessexo, mapping = aes(x = sexo)) + geom_bar()

## Barras Grupadas

ggplot(data = janjul, mapping = aes(x = origin, y = temp, fill = mes)) +
  geom_bar(position = "dodge", stat = "identity")

## Stacked Bar

ggplot(data = janjul, mapping = aes(x = origin, y = temp, fill = mes)) +
  geom_bar(stat = "identity")

