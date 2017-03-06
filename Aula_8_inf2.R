## Aula 8 -- Inferência 2
## MAD-CB
## James Hunter
## 7/3/2017

suppressMessages(library(tidyverse))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(nycflights13)) #pacote de dados
suppressPackageStartupMessages(library(forcats))
options(scipen = 1000)

## Eleitores
n <- 10^6 ##número de eleitores
set.seed(1)
p <- .496 ## proporção dos Democrats
cidade <- rep( c("D","R"), n*c( p, 1 - p))
cidade <- sample(cidade) # não necessáario, mas mistura os eleitores
prop.table(table(cidade))
npoll <- 1000 ## tamanho de amostra de sondagem
poll <- sample(cidade, npoll, replace = TRUE)
table(poll)

## Variáveis Aleatórias
numpolls = 5
pollDem <- numeric(numpolls)
## o primeiro loop do curso
for (i in 1:numpolls) {
  samp <- sample(cidade, 1000, replace = TRUE)
  pollDem[i] <- sum(samp == "D")
}
pollDem

## Erro Padrão do Valor
seDem <- sqrt(npoll * p * (1 - p))

## Erro Padrão da Proporção
sePad <- sqrt(p * (1 - p)/sqrt(npoll))

## p-hat
p_hat <- mean(poll == "D")
se <- sqrt(p_hat * (1 - p_hat)/1000)
cat("Nossa estimativa da proporção dos Democrats\né",p_hat,
    "mais ou menos", round(se, 5))

## Distribuição de Probabilidade
trials = 10^4
erro <- replicate(trials, {
  X <- sample(cidade, npoll, replace = TRUE)
  mean(X == "D") - p
})

mean(abs(erro) > 0.01581) ## erros maiores que o SE
hist(erro)
abline(v = 0.0, col = "red", lwd = 2)

## qqplot
qqnorm(erro)
qqline(erro, col = "red", lwd = 2)

#Comparação
cat("Proporção verdadeira: ", mean(abs(erro) > 0.01581))
cat("Proporção teorica: ", pnorm(-1) + (1 - pnorm(1)))

## CARA/COROA
set.seed(1); n <- 1000; k <-  1; prob <- 0.5
tiras <- rbinom(n, k, prob)
(caras <- sum(tiras)) ## número de CARAS


## Curva Normal Padronizada
library(ggplot2)
dnorm.lim <- function(x) {
  y <- dnorm(x)
  y[x < -1.96 | x > 1.96] <- NA
  return(y)
}
norm.plot <- ggplot(data.frame(x = c(-3, 3)), aes(x = x))
norm.plot <- norm.plot + stat_function(fun = dnorm.lim, geom = "area", 
                                       fill = "blue", alpha = 0.2)
norm.plot <- norm.plot + stat_function(fun = dnorm)
norm.plot <- norm.plot + labs(y = "Densidade", x = "Z", title = "95% Área")
norm.plot <- norm.plot + annotate("text", x = -2, y = 0.07, label = "z = -1.96")
norm.plot <- norm.plot + annotate("text", x = 2, y = 0.07, label = "z = 1.96")
norm.plot <- norm.plot + annotate("text", x = -2.3, y = 0.02, label = "alfa/2 = 0.025")
norm.plot <- norm.plot + annotate("text", x = 2.3, y = 0.02, label = "alfa/2 = 0.975")
norm.plot

## Calcular IC 
phat <- sum(tiras)/1000
nivel <- 0.05
z <-  qnorm(nivel/2, mean = 0, sd = 1, lower.tail = FALSE)
marg.erro  <-  z * sqrt(phat*(1 - phat)/1000)
(ci <- phat + c(-marg.erro, +marg.erro))

## Calcular IC com Pacote binom
## Se não tiver carregado o pacote binom, precisa instalar. 
## Tira a marca de comentário na próxima linha para fazer
# install.packages("binom")
## Se já tem, pode ir diretamente ao próximo comando
library(binom)
binom.confint(sum(tiras), n, conf.level = 0.95, methods = "asymptotic")

## Testes de Hipoteses

## Carregar Dados e Análise Exploratória
temps <- read_table("TempData.txt", col_names = FALSE) 
colnames(temps) <- "tempC"
library(psych)
psych::describe(temps)
(xbar <- mean(temps$tempC))
(dp <- sd(temps$tempC))
(n <- length(temps$tempC))
boxplot(temps$tempC, ylab = "Temperatura C")
abline(h = 37, col = "red")


## t-Distribuição Família
x <- seq(-3, 3, 0.01)
t1 <- dt(x, 1)
t2 <- dt(x, 2)
t5 <- dt(x, 5)
t30 <- dt(x, 30)
plot(x, t30, type = "l", col = "red", ylim = c(0, 0.4), ylab = "Densidade",
     main = "Família das Distribuições Student's t")
lines(x, t1, col = "blue")
lines(x, t2, col = "green")
lines(x, t5, col = "black")
abline(v = 0)
leg.txt <- c("df = 30", "df = 5", "df = 2", "df = 1")
legend("topright", legend = leg.txt, 
       col = c("red", "black", "green", "blue"), lty = 1)

## Tipos de Calculos das Distribuições
dnorm(1.96)
pnorm(1.96)
qnorm(0.975)
runif(3, 0, 1) ## 3 números aleatórios entre 0 e 1 da dist. Uniforme

## teste t para temperatura
describe(temps$tempC)
## Estatística de teste
mu <- 37; df <- n - 1
(tstat <- (xbar - mu) / sqrt(dp^2 / n))
2 * pt(tstat, df) # para teste de 2 lados

## Teste t com 2 Amostras
load("Crimes.PMSP")
describe(Crimes.PMSP$TotHD2011)
describe(Crimes.PMSP$TotHD2012)
apply(Crimes.PMSP[,8:9], 2, mean)
apply(Crimes.PMSP[,8:9], 2, sd)
boxplot(Crimes.PMSP[,8:9], horizontal = FALSE, xlab = "Ano", 
        ylab = "Número Mensal", main = "Homicídios Dolosos & Tentativas",
        names = c("2011", "2012"))
with(Crimes.PMSP, t.test(TotHD2012, TotHD2011, mu = 0, alternative = "greater"))

## Expectativa da Vida
load("vidadados.RData")
amerSSA <- vidadados %>% 
           filter(Regiao %in% c("Amer", "SSA"))
Desc(amerSSA$ExpVida[amerSSA$Regiao == "Amer"])
Desc(amerSSA$ExpVida[amerSSA$Regiao == "SSA"])
amerSSA %>% group_by(Regiao) %>%
            summarise_at(vars(ExpVida), funs(mean, sd))
vidagr <- vidagr <- ggplot(amerSSA,aes(x = Regiao, y = ExpVida)) + geom_boxplot()
vidagr <- vidagr + labs(x = "Região", y = "Expectativa de Vida em Anos",
                        title = "Expectativa de Vida -- Americas x África África Subsaariana")

vidagr
options(scipen = 4)
t.test(amerSSA$ExpVida[amerSSA$Regiao == "Amer"], 
       amerSSA$ExpVida[amerSSA$Regiao == "SSA"], 
       mu = 0, alternative = "two.sided")