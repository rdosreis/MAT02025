set.seed(16052023)

# Suponha que temos o registro das alturas de uma população

N <- 10000000
altura_homens <- rnorm(n = N/2, mean = 1.778, sd = 0.0762)
altura_mulheres <- rnorm(n = N/2, mean = 1.6383, sd = 0.0635)
altura_pop <- c(altura_homens, altura_mulheres)

# Desta forma, conhecemos a VERDADEIRA ALTURA MÉDIA da população

(A_VERDADE <- mean(altura_pop))

# Suponha que selecionamos uma amostra das alturas da população

n <- 100
altura_amostra <- sample(x = altura_pop, size = n, replace = FALSE)

# Vamos estimar a altura média utilizando três diferentes estimadores

## A média aritmética (x_barra)

mean(altura_amostra)

## A primeira observação da amostra (x_1)

altura_amostra[1]

## A média aritmética entre os valores mínimo e máximo (media_min_max)

sum(c(min(altura_amostra), max(altura_amostra))) / 2

#### --------------------------------------------------
# Vamos repetir o procedimento de ESTIMAR A ALTURA MÉDIA
#### --------------------------------------------------

altura_amostra <- sample(x = altura_pop, size = n, replace = FALSE)

# Vamos estimar a altura média utilizando três diferentes estimadores

## A média aritmética (x_barra)

mean(altura_amostra)

## A primeira observação da amostra (x_1)

altura_amostra[1]

## A média aritmética entre os valores mínimo e máximo (media_min_max)

sum(c(min(altura_amostra), max(altura_amostra))) / 2

#### --------------------------------------------------
# Agora vamos repetir o procedimento de ESTIMAR A ALTURA MÉDIA
# VÁRIAS VEZES. Digamos, M = 1000. E vamos guardar todas as
# M = 1000 estimativas da altura média obtidas pelos
# três diferentes estimadores
#### --------------------------------------------------

M <- 1000
x_barra <- numeric(length = M)
x_1 <- numeric(length = M)
media_min_max <- numeric(length = M)

for (i in 1:M){
  
  altura_amostra <- sample(x = altura_pop, size = n, replace = FALSE)
  
  # Vamos estimar a altura média utilizando três diferentes estimadores
  
  ## A média aritmética (x_barra)
  
  x_barra[i] <- mean(altura_amostra)
  
  ## A primeira observação da amostra (x_1)
  
  x_1[i] <- altura_amostra[1]
  
  ## A média aritmética entre os valores mínimo e máximo (media_min_max)
  
  media_min_max[i] <- sum(c(min(altura_amostra), max(altura_amostra))) / 2
  
}

# Vamos ver como se comportam cada um dos estimadores
# Histograma, boxplot, média, desvio padrão, etc.

mean(x_barra); sd(x_barra)

mean(x_1); sd(x_1)

mean(media_min_max); sd(media_min_max)

# , xlim=c(-35, 100)

hist(x_1,
     col = '#1B9E77',
     border = "white",
     xlim=c(1.35, 2.15),
     ylim = c(0, 40),
     probability = TRUE,
     xlab = "estimativa",
     ylab = "Densidade",
     main = "")
hist(media_min_max,
     col = '#D95F02',
     border = "white",
     add = TRUE,
     probability = TRUE)
hist(x_barra,
     col = '#7570B3',
     border = "white",
     add = TRUE,
     probability = TRUE)
abline(v = A_VERDADE, col = "red", lwd = 2)
legend(x = 1.75,
       y = 40,
       legend = c("x_1", "media_min_max", "x_barra"),
       fill = c("#1B9E77", "#D95F02", "#7570B3"),
       border = "white",
       bty = "n")

estimativas <- c(x_1, media_min_max, x_barra)
estimadores <- c(rep("x_1", M), rep("media_min_max", M), rep("x_barra", M))
estimadores <- factor(estimadores,
                      levels = c("x_1", "media_min_max", "x_barra"))

boxplot(estimativas ~ estimadores, col = c("#1B9E77", "#D95F02", "#7570B3"))
abline(h = A_VERDADE, col = "red", lwd = 2)

# Veja o que acontece com o comportamento dos três estimadores
# se obtemos mais (menos) informação.
# Ou seja, aumente (diminua) o tamanho da amostra (n).

# Além disso, implemente o mesmo esquema de simulação, porém
# agora para avaliar os dois estimadores para a variância populacional
# apresentados em aula