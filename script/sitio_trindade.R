library(tidyverse)
library(readxl)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(VIM)
library(GJRM)


# Carregando base de dados
url <- "https://github.com/cccneto/valuation_urbanParks/blob/master/dados/base_trindade.xlsx?raw=true"
destfile <- tempfile()
curl::curl_download(url, destfile)
baseunificada <- read_excel(destfile)


baseunificada <- baseunificada %>% 
    mutate(
    infraestrutura = as.double(infraest),
    sombra = as.double(sombra),
    temperatura = as.double(temperatura),
    resp1 = dap1,
    resp2 = dap2)

# Ajustando codigo de variaveis

base <- baseunificada %>%
  mutate(
    infraestrutura = case_when(infraestrutura >= 4 ~ "1", TRUE ~ "0"), 
    temperatura = case_when(temperatura >= 4 ~ "1", TRUE ~ "0"),
    sombra = case_when(sombra >= 4 ~ "1", TRUE ~ "0"),
    escolar = case_when(escolar > 19 ~ "1", TRUE ~ "0"), # 1=superior completo, 0= otherwise
    sexo = case_when(sexo == 1 ~ "1", TRUE ~ "0")
  )

base <- base %>%
  mutate(
     idade = as.integer(idade),
    sexo = as.integer(sexo),
    escolar = as.integer(escolar),
    renda = as.integer(renda),
    infraestrutura = as.integer(infraestrutura),
    sombra = as.integer(sombra),
    temperatura = as.integer(temperatura),
    lance1 = as.integer(lance1),
    lance2 = as.integer(lance2),
    resp1 = as.factor(resp1),
    resp2 = as.factor(resp2)
  )

# renomeando
dados_trindade <- base 

## Configurando listas de variaveis - **modelo sem renda**
treat.eq <- resp1 ~ lance1 + idade + sexo + escolar + temperatura + infraestrutura
out.eq <- resp2 ~ lance2 + idade + sexo + escolar + temperatura + infraestrutura
f.list <- list(treat.eq, out.eq)
mr <- c("probit", "probit")

# regressoes dos parques
bvp_trindade <- gjrm(f.list, data=dados_trindade, Model="B", margins= mr)
summary(bvp_trindade)

# MACAXEIRA

# atribuindo valores médios
lance1_med <- mean(dados_trindade$lance1)
renda_med <- mean(dados_trindade$renda)
idade_med <- mean(dados_trindade$idade)
sexo_med <- mean(dados_trindade$sexo)
escolar_med <- mean(dados_trindade$escolar)
temp_med <- mean(dados_trindade$temperatura)
infra_med <- mean(dados_trindade$infraestrutura)
lance2_med <- mean(dados_trindade$lance2)

## coeficientes eq 1
cf_intercept <- bvp_trindade$coefficients[1]
cf_lance1 <- bvp_trindade$coefficients[2]
cf_idade <- bvp_trindade$coefficients[3]
cf_sexo <- bvp_trindade$coefficients[4]
cf_escolar <- bvp_trindade$coefficients[5]
cf_temp <- bvp_trindade$coefficients[6]
cf_infra <- bvp_trindade$coefficients[7]

# coeficientes eq 2
cf_intercept2 <- bvp_trindade$coefficients[8]
cf_lance2 <- bvp_trindade$coefficients[9]
cf_idade2 <- bvp_trindade$coefficients[10]
cf_sexo2 <- bvp_trindade$coefficients[11]
cf_escolar2 <- bvp_trindade$coefficients[12]
cf_temp2 <- bvp_trindade$coefficients[13]
cf_infra2 <- bvp_trindade$coefficients[14]

## CALCULANDO A DAP - macaxeira

### Dap equação 1
dap_eq1 <- -(cf_intercept + cf_idade*idade_med + cf_sexo*sexo_med + cf_escolar*escolar_med + cf_temp*temp_med + cf_infra*infra_med)/cf_lance1 # R$ 40,47
# R$ 36,90
# A dap da equação 1 foi de R$ 
round(dap_eq1, digits = 2)

### Dap equação 2
dap_eq2 <- -(cf_intercept + cf_idade2*idade_med + cf_sexo2*sexo_med + cf_escolar2*escolar_med + cf_temp2*temp_med + cf_infra2*infra_med)/cf_lance2 # R$ 39,50 
#R$ 14,57

