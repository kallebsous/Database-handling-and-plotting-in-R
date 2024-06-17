#Necessário instalar os seguintes pacotes:
#MASS, ggplot2, Modeest

edit(Skye)
#Os dados são da Variação química dos principais elementos nas lavas do Oceano da Ilha de Skye

#Variável A = Porcentagem de óxidos de sódio e potássio.
#Variável F = Porcentagem de óxido de ferro.
#Variável M = Porcentagem de óxido de magnésio


mean(Skye$A) #media da Variável A
mean(Skye$F) #media da Variável F
mean(Skye$M) #media da Variável M

median(Skye$A) #mediana da Variável A
median(Skye$F) #mediana da Variável F
median(Skye$M) #mediana da Variável M

#Package Modeest
mfv(Skye$A) #moda da Variável A
mfv(Skye$F) #moda da Variável F
mfv(Skye$M) #moda da Variável M

var(Skye$A) #variancia da Variável A
var(Skye$F) #variancia da Variável F
var(Skye$M) #variancia da Variável M

sd(Skye$A) #desvio padrão da Variável A
sd(Skye$F) #desvio padrão da Variável F
sd(Skye$M) #desvio padrão da Variável M


#Tabela de Frequência da Variação química da Porcentagem de óxidos de sódio e potássio na Ilha de Skye

Skye$A #dados

sort(Skye$A)
#valores da variação de porcentagem de Sódio e Potássio
SodPota <- hist(Skye$A, plot=F, breaks = 5) #vetor com os dados 

v = range(Skye$A) #extraindo o menor e maior valor dos dados

classe <- length(SodPota$counts) #quantidade de linhas = 5L


amplitude = (v[2]-v[1])/classe #Amplitude das classes = 7.8

amplitude = ceiling(amplitude) #arredonda para o maior valor = 8

intervalo = seq(v[1],amplitude*classe+v[1], by = amplitude)

SodPota.cut = cut(Skye$A, intervalo, right = FALSE) #corta os dados de acordo com a amplitude
SodPota.cut

SodPota.freq = table(SodPota.cut) #criado a tabela de frequencia
SodPota.freq 

cbind(SodPota.freq) #organiza ela, trasnformando linhas em colunas

tabela.final <- data.frame(SodPota.freq, SodPota$mids) #finaliza a tabela criando um objeto com os dados e os pontos medios

edit(tabela.final) #imprime a tabela


#histograma da Variável A

histograma <- ggplot(data.frame(A = Skye$A), aes(x = A)) +
  geom_histogram(fill = "darkred", color = "black", binwidth = 2) +
  theme_minimal() +
  labs(title = "Variação química",
       x = "Porcentagem de óxidos de sódio e potássio",
       y = "Frequência")
print(histograma)

#grafico de pizza da Variável A
#no ggplot2 só temos o gráfico de coordenadas polares que se assemelha a um de pizza
graficopizza <- ggplot(Skye, aes(x = "", y = A, fill = factor(A))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(title = "Variação química",
       fill = "Porcentagem de óxidos de sódio e potássio")
print(graficopizza)

#outro modo de representar o grafico de pizza é usando a 
#função propria do R
pie(table(Skye$A), col = rainbow(length(Skye$A)))

#grafico de dispersão da variável "M" em relação a "A",  enquanto "A" aumenta, "M" diminui
graficodispersao <- ggplot() +
  geom_point(aes(x = Skye$A, y = Skye$M), color = "darkred") +
  labs(title = "Gráfico de Dispersão",
       x = "Porcentagem de óxidos de sódio e potássio ",
       y = "Porcentagem de óxido de magnésio")
print(graficodispersao)
