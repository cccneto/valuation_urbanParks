library(tidyverse)
library(readxl)
library(Ecdat)
library(lmtest)
library(VIM)
library(GJRM)

# importando dados do computador
baseunificada <- read_excel("C:/Users/User/Desktop/dados_cadu_t4/dados_santana_limpo.xlsx")


# mudando nome da variavel
baseunificada <- baseunificada %>% 
  mutate( resp1 = dap1, resp2 = dap2)

# Ajustando codigo de variaveis

base <- baseunificada %>%
  mutate(
    infraestrutura = case_when(infraest > 3 ~ "1", TRUE ~ "0"),
    temperatura = case_when(temperatura > 3 ~ "1", TRUE ~ "0"),
    sombra = case_when(sombra > 3 ~ "1", TRUE ~ "0"),
    escolar = case_when(escolar > 5 ~ "1", TRUE ~ "0"), # 1=superior completo, 0= otherwise
    sexo = case_when(sexo == 1 ~ "1", TRUE ~ "0"),
    saudefis = case_when(saudefis > 3 ~ "1", TRUE ~ "0"),
    saudemen = case_when(saudemen > 3 ~ "1", TRUE ~ "0"),
    freqvis = case_when(freqvis > 3 ~ "1", TRUE ~ "0")
  )

# renomeando
dados_santana <- base %>% 
  select(resp1, resp2, lance1, lance2, idade, sexo, 
         escolar, freqvis, tempoestad, saudefis, saudemen, 
         sombra, temperatura)

## Configurando listas de variaveis - **modelo sem renda**

treat.eq <- resp1 ~ lance1 + idade + sexo + escolar + freqvis + tempoestad + saudefis + saudemen + sombra + temperatura
out.eq <- resp2 ~ lance2 + idade + sexo + escolar + freqvis + tempoestad + saudefis + saudemen + sombra + temperatura


f.list <- list(treat.eq, out.eq)
mr <- c("probit", "probit")

mod1 <- glm(treat.eq, family = "binomial", data = dados_santana)
summary(mod1)

# regressoes dos parques
bvp_santana <- gjrm(f.list, data=dados_santana, Model="B", margins= mr)
summary(bvp_santana)

# MACAXEIRA

# atribuindo valores médios
lance1_med <- mean(dados_santana$lance1)
# renda_med <- mean(dados_santana$renda)
idade_med <- mean(dados_santana$idade)
sexo_med <- mean(as.numeric(dados_santana$sexo))
escolar_med <- mean(as.numeric(dados_santana$escolar))
temperatura_med <- mean(as.numeric(dados_santana$temperatura))
infra_med <- mean(as.numeric(dados_santana$infraestrutura))
lance2_med <- mean(dados_santana$lance2)
freqvis_med <- mean(as.numeric(dados_santana$freqvis))
tempoestad_med  <- mean(dados_santana$tempoestad)
saudefis_med  <- mean(as.numeric(dados_santana$saudefis))
saudemen_med  <- mean(as.numeric(dados_santana$saudemen))
sombra_med <- mean(as.numeric(dados_santana$sombra))


## coeficientes eq 1
cf_intercept <- bvp_santana$coefficients[1]
cf_lance1 <- bvp_santana$coefficients[2]
cf_idade <- bvp_santana$coefficients[3]
cf_sexo <- bvp_santana$coefficients[4]
cf_escolar <- bvp_santana$coefficients[5]
cf_freqvis <- bvp_santana$coefficients[6]
cf_tempoestad <- bvp_santana$coefficients[7]
cf_saudefis <- bvp_santana$coefficients[8]
cf_saudemen <- bvp_santana$coefficients[9]
cf_sombra <- bvp_santana$coefficients[10]
cf_temperatura <- bvp_santana$coefficients[11]


# coeficientes eq 2
cf_intercept2 <- bvp_santana$coefficients[12]
cf_lance2 <- bvp_santana$coefficients[13]
cf_idade2 <- bvp_santana$coefficients[14]
cf_sexo2 <- bvp_santana$coefficients[15]
cf_escolar2 <- bvp_santana$coefficients[16]
cf_freqvis2 <- bvp_santana$coefficients[17]
cf_tempoestad2 <- bvp_santana$coefficients[18]
cf_saudefis2 <- bvp_santana$coefficients[19]
cf_saudemen2 <- bvp_santana$coefficients[20]
cf_sombra2 <- bvp_santana$coefficients[21]
cf_temperatura2 <- bvp_santana$coefficients[22]

## CALCULANDO A DAP - macaxeira

### Dap equação 1
dap_eq1 <- -(cf_intercept + cf_idade*idade_med + cf_sexo*sexo_med + cf_escolar*escolar_med + cf_freqvis*freqvis_med + cf_tempoestad*tempoestad_med + cf_saudefis*saudefis_med + cf_saudemen*saudemen_med + cf_sombra*sombra_med + cf_temperatura*temperatura_med)/cf_lance1 

### Dap equação 2
dap_eq2 <- -(cf_intercept2 + cf_idade2*idade_med + cf_sexo2*sexo_med + cf_escolar2*escolar_med + cf_freqvis2*freqvis_med + cf_tempoestad2*tempoestad_med +
               cf_saudefis2*saudefis_med + cf_saudemen2*saudemen_med + cf_sombra2*sombra_med + cf_temperatura2*temperatura_med)/cf_lance2

# daps
dap_eq1

dap_eq2

