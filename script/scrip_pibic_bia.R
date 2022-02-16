library(tidyverse)
library(readxl)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(VIM)
library(GJRM)


# Carregando base de dados
url <- "https://github.com/cccneto/valuation_urbanParks/blob/master/dados/baseunificada.xlsx?raw=true"
destfile <- tempfile()
curl::curl_download(url, destfile)
baseunificada <- read_excel(destfile)


baseunificada <- baseunificada %>% 
  select(parque, idade, sexo, cidade, escolar, renda, infraestrutura, sombra, temperatura, 
         lance1,lance2, resp1, resp2, bidl, bidh, lnRendat1, lnRendat2, lnRenda) %>% 
  mutate(
    infraestrutura = as.double(infraestrutura),
    sombra = as.double(sombra),
    temperatura = as.double(temperatura))

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
    parque = as.factor(parque),
    idade = as.integer(idade),
    sexo = as.integer(sexo),
    cidade = as.factor(cidade),
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



dados_macaxeira <- base %>% filter(parque == "macaxeira")
dados_jaqueira <- base %>% filter(parque == "jaqueira")
dados_santana <- base %>% filter(parque == "santana")

## Configurando listas de variaveis - **modelo sem renda**
treat.eq <- resp1 ~ lance1 + idade + sexo + escolar + temperatura + infraestrutura
out.eq <- resp2 ~ lance2 + idade + sexo + escolar + temperatura + infraestrutura
f.list <- list(treat.eq, out.eq)
mr <- c("probit", "probit")

# regressoes dos parques
bvp_macaxeira <- gjrm(f.list, data=dados_macaxeira, Model="B", margins= mr)
summary(bvp_macaxeira)

bvp_jaqueira <- gjrm(f.list, data=dados_macaxeira, Model="B", margins= mr)
summary(bvp_jaqueira)

bvp_santana <- gjrm(f.list, data=dados_macaxeira, Model="B", margins= mr)
summary(bvp_santana)

# MACAXEIRA

# atribuindo valores médios
lance1_med_mcx <- mean(dados_macaxeira$lance1)
renda_med_mcx <- mean(dados_macaxeira$renda)
idade_med_mcx <- mean(dados_macaxeira$idade)
sexo_med_mcx <- mean(dados_macaxeira$sexo)
escolar_med_mcx <- mean(dados_macaxeira$escolar)
temp_med_mcx <- mean(dados_macaxeira$temperatura)
infra_med_mcx <- mean(dados_macaxeira$infraestrutura)
lance2_med_mcx <- mean(dados_macaxeira$lance2)

## coeficientes eq 1
cf_intercept_mcx <- bvp_macaxeira$coefficients[1]
cf_lance1_mcx <- bvp_macaxeira$coefficients[2]
cf_idade_mcx <- bvp_macaxeira$coefficients[3]
cf_sexo_mcx <- bvp_macaxeira$coefficients[4]
cf_escolar_mcx <- bvp_macaxeira$coefficients[5]
cf_temp_mcx <- bvp_macaxeira$coefficients[6]
cf_infra_mcx <- bvp_macaxeira$coefficients[7]

# coeficientes eq 2
cf_intercept2_mcx <- bvp_macaxeira$coefficients[8]
cf_lance2_mcx <- bvp_macaxeira$coefficients[9]
cf_idade2_mcx <- bvp_macaxeira$coefficients[10]
cf_sexo2_mcx <- bvp_macaxeira$coefficients[11]
cf_escolar2_mcx <- bvp_macaxeira$coefficients[12]
cf_temp2_mcx <- bvp_macaxeira$coefficients[13]
cf_infra2_mcx <- bvp_macaxeira$coefficients[14]

## CALCULANDO A DAP - macaxeira

### Dap equação 1
dap_eq1_mcx <- -(cf_intercept_mcx + cf_idade_mcx*idade_med_mcx + cf_sexo_mcx*sexo_med_mcx + cf_escolar_mcx*escolar_med_mcx + cf_temp_mcx*temp_med_mcx + cf_infra_mcx*infra_med_mcx)/cf_lance1_mcx # R$ 40,47
# R$ 36,90
# A dap da equação 1 foi de R$ 
round(dap_eq1, digits = 2)

### Dap equação 2
dap_eq2_mcx <- -(cf_intercept_mcx + cf_idade2_mcx*idade_med_mcx + cf_sexo2_mcx*sexo_med_mcx + cf_escolar2_mcx*escolar_med_mcx + cf_temp2_mcx*temp_med_mcx + cf_infra2_mcx*infra_med_mcx)/cf_lance2_mcx # R$ 39,50 
#R$ 14,57

#############################

# JAQUEIRA

# atribuindo valores médios
lance1_med_jaq <- mean(dados_jaqueira$lance1)
renda_med_jaq <- mean(dados_jaqueira$renda)
idade_med_jaq <- mean(dados_jaqueira$idade)
sexo_med_jaq <- mean(dados_jaqueira$sexo)
escolar_med_jaq <- mean(dados_jaqueira$escolar)
temp_med_jaq <- mean(dados_jaqueira$temperatura)
infra_med_jaq <- mean(dados_jaqueira$infraestrutura)
lance2_med_jaq <- mean(dados_jaqueira$lance2)

## coeficientes eq 1
cf_intercept_jaq <- bvp_jaqueira$coefficients[1]
cf_lance1_jaq <- bvp_jaqueira$coefficients[2]
cf_idade_jaq <- bvp_jaqueira$coefficients[3]
cf_sexo_jaq <- bvp_jaqueira$coefficients[4]
cf_escolar_jaq <- bvp_jaqueira$coefficients[5]
cf_temp_jaq <- bvp_jaqueira$coefficients[6]
cf_infra_jaq <- bvp_jaqueira$coefficients[7]

# coeficientes eq 2
cf_intercept2_jaq <- bvp_jaqueira$coefficients[8]
cf_lance2_jaq <- bvp_jaqueira$coefficients[9]
cf_idade2_jaq <- bvp_jaqueira$coefficients[10]
cf_sexo2_jaq <- bvp_jaqueira$coefficients[11]
cf_escolar2_jaq <- bvp_jaqueira$coefficients[12]
cf_temp2_jaq <- bvp_jaqueira$coefficients[13]
cf_infra2_jaq <- bvp_jaqueira$coefficients[14]

## CALCULANDO A DAP - jaqueira

### Dap equação 1
dap_eq1_jaq <- -(cf_intercept_jaq + cf_idade_jaq*idade_med_jaq + cf_sexo_jaq*sexo_med_jaq + cf_escolar_jaq*escolar_med_jaq + cf_temp_jaq*temp_med_jaq + cf_infra_jaq*infra_med_jaq)/cf_lance1_jaq # R$ 40,47
# R$ 43,52

### Dap equação 2
dap_eq2_jaq <- -(cf_intercept_jaq + cf_idade2_jaq*idade_med_jaq + cf_sexo2_jaq*sexo_med_jaq + cf_escolar2_jaq*escolar_med_jaq + cf_temp2_jaq*temp_med_jaq + cf_infra2_jaq*infra_med_jaq)/cf_lance2_jaq # R$ 39,50 
# R$ 19,36


#############################

# SANTANA

# atribuindo valores médios
lance1_med_sant <- mean(dados_santana$lance1)
renda_med_sant <- mean(dados_santana$renda)
idade_med_sant <- mean(dados_santana$idade)
sexo_med_sant <- mean(dados_santana$sexo)
escolar_med_sant <- mean(dados_santana$escolar)
temp_med_sant <- mean(dados_santana$temperatura)
infra_med_sant <- mean(dados_santana$infraestrutura)
lance2_med_sant <- mean(dados_santana$lance2)

## coeficientes eq 1
cf_intercept_sant <- bvp_santana$coefficients[1]
cf_lance1_sant <- bvp_santana$coefficients[2]
cf_idade_sant <- bvp_santana$coefficients[3]
cf_sexo_sant <- bvp_santana$coefficients[4]
cf_escolar_sant <- bvp_santana$coefficients[5]
cf_temp_sant <- bvp_santana$coefficients[6]
cf_infra_sant <- bvp_santana$coefficients[7]

# coeficientes eq 2
cf_intercept2_sant <- bvp_santana$coefficients[8]
cf_lance2_sant <- bvp_santana$coefficients[9]
cf_idade2_sant <- bvp_santana$coefficients[10]
cf_sexo2_sant <- bvp_santana$coefficients[11]
cf_escolar2_sant <- bvp_santana$coefficients[12]
cf_temp2_sant <- bvp_santana$coefficients[13]
cf_infra2_sant <- bvp_santana$coefficients[14]

## CALCULANDO A DAP - SANTANA

### Dap equação 1
dap_eq1_sant <- -(cf_intercept_sant + cf_idade_sant*idade_med_sant + cf_sexo_sant*sexo_med_sant + cf_escolar_sant*escolar_med_sant + cf_temp_sant*temp_med_sant + cf_infra_sant*infra_med_sant)/cf_lance1_sant # R$ 40,47
# R$ 68,81

### Dap equação 2
dap_eq2_sant <- -(cf_intercept_sant + cf_idade2_sant*idade_med_sant + cf_sexo2_sant*sexo_med_sant + cf_escolar2_sant*escolar_med_sant + cf_temp2_sant*temp_med_sant + cf_infra2_sant*infra_med_sant)/cf_lance2_sant # R$ 39,50 
# R$ 33.65