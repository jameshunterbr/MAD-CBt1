## Programa para a Aula de Regressão Linear
## Author: James Hunter
## Date: 18/3/17
## Version: 1.0

## Carregar Pacotes

suppressMessages(library(tidyverse))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(nycflights13)) #pacote de dados
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(broom))
suppressMessages(library(mosaic))
options(scipen = 10)

## Carregar Dados

### Se mosaic ainda não for instalado
### "uncomment" a linha seguinte para instalá-lo
### install.packages("mosaic")
suppressMessages(library(mosaic))
data(Galton)
str(Galton)

## Criar base de dados só de pais e filhos

boys <- Galton %>% filter(sex == "M") %>% select(-family, -mother, -sex, -nkids)
glimpse(boys)

## Gráfico de Dispersão de boys

grpf <- ggplot(data = boys, aes(x = father, y = height)) + geom_point(shape = 20) + geom_rug()
grpf <- grpf + labs(x = "Altura do Pai", y = "Altura do Filho", title = "Alturas em Polegadas")
grpf

## Mostra Linha de Tendência

grpf2 <- grpf + geom_smooth(method = "lm", se = FALSE, color = "red")
grpf2

## Estatística Descritiva de Boys

describe(boys)
paste("Coeficiente de Correlação:", with(boys, round(cor(father, height), 3)))

## Soma de Quadrados

set.seed(1946)
boyscoef <-  coef(lm(height~father, data  = boys))
int <- boyscoef[1]; inc <- boyscoef[2] 
x <- boys %>% 
  filter((height >= 66 & height < 71) & (father >= 67 & father < 71)) %>% 
  sample_n(5) %>% 
  mutate(ypred = int + inc * father)
xplot <- ggplot(data = x, aes(x = father, y = height)) 
xplot <- xplot + geom_point(shape = 19, colour = "darkgreen")
xplot <- xplot + geom_abline(aes(intercept = int, slope = inc), colour = "darkred")
xplot <- xplot + geom_segment(aes(x = father[3], y = height[3], xend = father[3], 
                                  yend = ypred[3]), colour = "blue", 
                              arrow = arrow(length = unit(0.3,"cm")))
xplot <- xplot + geom_point(aes(x = father[3], y = ypred[3]), 
                            shape = 19, color = "blue" )
xplot <- xplot + annotate(geom = "text", x = 68.2, y = 68.7, 
                          label = "y-hat")
xplot <- xplot + annotate(geom = "text", x = 68.2, y = 66.8, label = "y(i)")
xplot

## Soma de Quadrados 2

xplot2 <- xplot + geom_hline(yintercept = mean(boys$height), color = "orange")
xplot2 <- xplot2 + geom_segment(aes(x = father[3], y = ypred[3], 
                                    xend = father[3], 
                                    yend = mean(boys$height)), 
                                colour = "green", 
                                arrow = arrow(length = unit(0.3,"cm")))
xplot2 <- xplot2 + annotate(geom = "text", x = 67.7, y = 68.9, 
                            label = "SSR", color = "darkgreen")
xplot2 <- xplot2 + annotate(geom = "text", x = 67.5, y = 69.3, label = "y-bar")
xplot2 <- xplot2 + annotate(geom = "text", x = 67.7, y = 68, 
                            label = "SSE", color = "darkblue")
xplot2 <- xplot2 + geom_segment(aes(x = father[3] + 0.05, y = height[3], 
                                    xend = father[3] + .05, 
                                    yend = mean(boys$height)), 
                                colour = "red", 
                                arrow = arrow(length = unit(0.3,"cm")))
xplot2 <- xplot2 + annotate(geom = "text", x = 68.3, y = 68, 
                            label = "SST", color = "darkred")
xplot2

## Executar a Regressão

fit1 <- lm(height ~ father, data = boys)
summary(fit1)

## Extrair coeficientes do modelo

### com broom::tidy

broom::tidy(fit1) %>% kable()

### com coef

coef(fit1) 

## Previsões de Novos Valores

fit1 %>% broom::augment(newdata = data_frame(father = 72))

## Histograma dos Modelos

set.seed(1946)
homodelos <- replicate(5000, (lm(shuffle(height) ~ father, data = boys) %>% coef()))
homodelos <- tibble(homodelos[2,])
colnames(homodelos) <- "father"
modgr1 <- ggplot(homodelos, aes(x = father)) 
modgr1 <- modgr1 + geom_histogram(color = "white", bins = 20)
modgr1 <- modgr1 + labs(x = "Pais", y = "Número")
modgr1

paste("Número de simulações com beta1 >= obs:", 
      sum(homodelos$father >= inc))
modgr2 <- ggplot(homodelos, aes(x = father, fill = (father >= inc))) 
modgr2 <- modgr2 + geom_histogram(color = "white", bins = 20)
modgr2 <- modgr2 + labs(x = "Pais", y = "Número", fill = "pai >= obs")
modgr2

## Gráfico de Resíduos

mods <- augment(fit1) 
residgr <- ggplot(data = mods, mapping = aes(x = .fitted, y = .resid))
residgr <- residgr + geom_point()
residgr <- residgr + geom_hline(yintercept = 0, color = "blue")
residgr <- residgr + labs(x = "Valores Previstos pelo Modelo", 
                          y = "Resíduos")
residgr

## Gráfico Q-Q

grqq <- ggplot(data = mods, aes(sample = .resid))
grqq <- grqq + stat_qq()
grqq <- grqq + labs(x = "Quantiles Teóricos", 
                    y = "Quantiles da Amostra")
grqq

## Gráficos Q-Q Também Disponível Diretamente em Base R

qqnorm(boys$height)
qqline(boys$height, col = 2, lwd = 2)
grid()

## Função `plot` para Objetos `lm`

par(mfrow=c(2,2))
plot(fit1)
mfrow=c(1,1)

## Função `qqPlot()` do Pacote `car`

car::qqPlot(fit1)