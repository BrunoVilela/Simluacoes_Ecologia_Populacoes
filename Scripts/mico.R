# Simulações Mico-leão-dourado
library(ggplot2)
library(tidyr)
library(dplyr)

# Código simulação
source("Scripts/crescimento_discreto.R")

# Locais
locais <- c("Reserva Biológica Poço das Antas",
            "Reserva Biológica União",
            "Fragmentos ao redor da bacia do Rio São João")

# Simulações
t <- 1:12
Ttotal <- length(t)
lambda <- rbind(rnorm(Ttotal, 1.10, sd = 0.03),
                rnorm(Ttotal, 1.09, sd = 0.02),
                rnorm(Ttotal, 0.98, sd = 0.1))

lambda[1, 8] <- 1.3 # Incendios no pantanal

# Populações iniciais
N0 <- c(213, 184, 167)

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
          file = "Resultados/população_mico.csv", 
          row.names = FALSE)
