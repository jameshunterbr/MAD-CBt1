## Arquivo com os Blocos ("Chunks") de Código para
## Aula 3
## 14 de fevereiro de 2017
## James Hunter

## Criar Conjunto de Testes CD4+

ano <- (2001:2009) 
n <- c(2196, 21217, 17017, 20366, 24753, 23480, 25354, 25347, 26507)
cd4n <- tibble::as_tibble(cbind(ano, n))

cd4n

## Resumo de 2 Variáveis

summary(cd4n)

## Resumo Usando DescTools

library(DescTools)
options(scipen = 1000)
Desc(cd4n$n, plotit = FALSE)

## Medindo Dispersão com Desvio da Média

suppressMessages(library(tidyverse))
dados1 <- tibble(x = c(1, 2, 3, 4, 5))
dados1
mean(dados1$x)

## Distância de Cada Ponto da Média

dev = dados1$x - mean(dados1$x)
(dados1 <- bind_cols(dados1, tibble(dev)))
sum(dados1$dev)

## Distância de Cada Ponto da Média ao Quadrado

(dados1 <- bind_cols(dados1, tibble(devsq = dados1$dev^2)))
sum(dados1$devsq)

## Gráfico do desvio e desvio^2
dados2 <- tibble(dev = 1:10, devsq = dev^2)
plot(devsq ~ dev, data = dados2, xlab = "Desvio", ylab = "Desvio ao Quadrado",
     pch = 20, col = "darkgreen", type = "l", lwd = 2,
     main = "Desvio ao Quadrado Dá Mais Peso\naos Valores Grandes")
grid(col = "black", lty = "dotted")

## Dados com Outlier

(dados3 <- tibble(x = c(1, 2, 3, 4, 50)))
Desc(dados3$x, plotit = TRUE)

## O Quarteto de Anscombe

anscombe
ansres <- anscombe %>% summarize_all(funs(mean, sd))
glimpse(round(ansres, 3))

## Gráfico  (Don't try this at home, Yet!)

ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  ## or   ff[[2]] <- as.name(paste0("y", i))
  ##      ff[[3]] <- as.name(paste0("x", i))
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)

