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

# calculando para MACAXEIRA

# atribuindo valores médios
lance1_med <- mean(dados_macaxeira$lance1)
renda_med <- mean(dados_macaxeira$renda)
idade_med <- mean(dados_macaxeira$idade)
sexo_med <- mean(dados_macaxeira$sexo)
escolar_med <- mean(dados_macaxeira$escolar)
temp_med <- mean(dados_macaxeira$temperatura)
infra_med <- mean(dados_macaxeira$infraestrutura)
lance2_med <- mean(dados_macaxeira$lance2)

## coeficientes eq 1
cf_intercept <- bvp_macaxeira$coefficients[1]
cf_lance1 <- bvp_macaxeira$coefficients[2]
cf_idade <- bvp_macaxeira$coefficients[3]
cf_sexo <- bvp_macaxeira$coefficients[4]
cf_escolar <- bvp_macaxeira$coefficients[5]
cf_temp <- bvp_macaxeira$coefficients[6]
cf_infra <- bvp_macaxeira$coefficients[7]

# coeficientes eq 2
cf_intercept2 <- bvp_macaxeira$coefficients[8]
cf_lance2 <- bvp_macaxeira$coefficients[9]
cf_idade2 <- bvp_macaxeira$coefficients[10]
cf_sexo2 <- bvp_macaxeira$coefficients[11]
cf_escolar2 <- bvp_macaxeira$coefficients[12]
cf_temp2 <- bvp_macaxeira$coefficients[13]
cf_infra2 <- bvp_macaxeira$coefficients[14]

## CALCULANDO A DAP - macaxeira

### Dap equação 1
dap_eq1 <- -(cf_intercept + cf_idade*idade_med + cf_sexo*sexo_med + cf_escolar*escolar_med + cf_temp*temp_med + cf_infra*infra_med)/cf_lance1 # R$ 40,47
# R$ 36,90
# A dap da equação 1 foi de R$ 
round(dap_eq1, digits = 2)

### Dap equação 2
dap_eq2 <- -(cf_intercept + cf_idade2*idade_med + cf_sexo2*sexo_med + cf_escolar2*escolar_med + cf_temp2*temp_med + cf_infra2*infra_med)/cf_lance2 # R$ 39,50 
#R$ 14,57

#############################

# calculando para JAQUEIRA

# atribuindo valores médios
lance1_med <- mean(dados_jaqueira$lance1)
renda_med <- mean(dados_jaqueira$renda)
idade_med <- mean(dados_jaqueira$idade)
sexo_med <- mean(dados_jaqueira$sexo)
escolar_med <- mean(dados_jaqueira$escolar)
temp_med <- mean(dados_jaqueira$temperatura)
infra_med <- mean(dados_jaqueira$infraestrutura)
lance2_med <- mean(dados_jaqueira$lance2)

## coeficientes eq 1
cf_intercept <- bvp_jaqueira$coefficients[1]
cf_lance1 <- bvp_jaqueira$coefficients[2]
cf_idade <- bvp_jaqueira$coefficients[3]
cf_sexo <- bvp_jaqueira$coefficients[4]
cf_escolar <- bvp_jaqueira$coefficients[5]
cf_temp <- bvp_jaqueira$coefficients[6]
cf_infra <- bvp_jaqueira$coefficients[7]

# coeficientes eq 2
cf_intercept2 <- bvp_jaqueira$coefficients[8]
cf_lance2 <- bvp_jaqueira$coefficients[9]
cf_idade2 <- bvp_jaqueira$coefficients[10]
cf_sexo2 <- bvp_jaqueira$coefficients[11]
cf_escolar2 <- bvp_jaqueira$coefficients[12]
cf_temp2 <- bvp_jaqueira$coefficients[13]
cf_infra2 <- bvp_jaqueira$coefficients[14]

## CALCULANDO A DAP - macaxeira

### Dap equação 1
dap_eq1 <- -(cf_intercept + cf_idade*idade_med + cf_sexo*sexo_med + cf_escolar*escolar_med + cf_temp*temp_med + cf_infra*infra_med)/cf_lance1 # R$ 40,47
# R$ 43,52
# A dap da equação 1 foi de R$ 
round(dap_eq1, digits = 2)

### Dap equação 2
dap_eq2 <- -(cf_intercept + cf_idade2*idade_med + cf_sexo2*sexo_med + cf_escolar2*escolar_med + cf_temp2*temp_med + cf_infra2*infra_med)/cf_lance2 # R$ 39,50 
# R$ 19,36