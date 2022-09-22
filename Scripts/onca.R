# Simulações onça-pintada
library(ggplot2)
library(tidyr)
library(dplyr)

# Código simulação
source("Scripts/crescimento_discreto.R")

# Locais
locais <- c("Parque das emas",
            "Parque indígena do Xingu",
            "Parque Nacional do Iguaçu",
            "Pantanal brasileiro",
            "Chapada Diamantina")

# Simulações
t <- 1:12
Ttotal <- length(t)
lambda <- rbind(rnorm(Ttotal, 1, sd = 0.03),
                rnorm(Ttotal, 1.05, sd = 0.02),
                rnorm(Ttotal, 0.9, sd = 0.1),
                rnorm(Ttotal, 0.95, sd = 0.05),
                rnorm(Ttotal, 0.85, sd = 0.04))

lambda[4, 10] <- 0.7 # Incendios no pantanal

# Populações iniciais
N0 <- c(412, 523, 114, 1057, 138)

# Simulações
n_locais <- length(locais)
crescimento_sp <- matrix(nrow = n_locais, ncol = Ttotal + 1)
for (i in 1:n_locais) {
  crescimento_sp[i, ] <- crescimento(N0[i], lambda[i, ], t)  
}
colnames(crescimento_sp) <- c(2010, 2010 + t)
populacao <- data.frame(locais, crescimento_sp, check.names = FALSE)

# Checar resultado
populacao %>% 
  pivot_longer(-locais, values_to = "População", names_to = "Ano") %>% 
  ggplot(aes(x = Ano, y = População, group = locais, color = locais)) +
  geom_line()

# Salvar os resultados
write.csv(populacao, 
          file = "Resultados/população_onça.csv", 
          row.names = FALSE)
