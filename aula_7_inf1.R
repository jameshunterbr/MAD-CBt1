## Aula 7 -- Inferência e Distribuições
## MAD-CB
## James Hunter
## 3/3/2017

## Cálculos Quincunx 
n <- 10; k <- 4; p <- 0.5
choose(n, k) # função para combinações

## Binomial aplicada a nossa exemplo

choose(n, k)
dbinom(k, n, p)
(quincunx <- round(dbinom(0:10, n, p), 3))
qc <- tibble(x = 0:10, quincunx)
qcgr <- ggplot(data = qc, mapping = aes(x = factor(x), y = quincunx))
qcgr <- qcgr + geom_bar(fill = "darkblue", width = 0.2, stat = "identity")
qcgr <- qcgr + labs(x = "Bin", y = "Probabilidade", 
                    title = "Probabilidade dos Bins de um Quinqunce", 
                    subtitle = "p = 0.5" )
qcgr

## Comparação de Teoria com o Último Estado de Jogo (slide Quincunx - 3)
## Dados do quincunx
data.qq <- c(1, 8, 44, 101, 208, 233, 230, 177, 47, 9, 2) 
prob.qq <- round(data.qq / sum(data.qq), 3)

## Gráfico
qc2 <- tibble(bin = 0:10, quincunx, dados = prob.qq)
qc2 <- qc2 %>% gather(type, prob, -bin)
qcgr2 <- ggplot(data = qc2, mapping = aes(x = factor(bin), 
                                          y = prob, fill = type))
qcgr2 <- qcgr2 + geom_bar(width = 0.6, stat = "identity", 
                          position = "dodge")
qcgr2 <- qcgr2 + labs(x = "Bin", y = "Probabilidade", 
                    title = "Probabilidade dos Bins Observada", 
                    subtitle = "p = 0.5" )
qcgr2

## Contas Azuis e Vermelhos

### criar contas
conta <- rep(c("vermelho", "azul"), times = c(526, 474))

### Selecionar 1 conta da garrafa: pode fazer multiplas vezes
sample(conta, 1)

### Replicate
trials <- 10000
set.seed(1)
eventos <- replicate(trials, sample(conta, 1))
head(eventos)

### Resultados da simulação
(tab <- table(eventos))
prop.table(tab)


