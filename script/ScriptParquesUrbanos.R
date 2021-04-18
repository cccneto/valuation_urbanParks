
library(readxl)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(dplyr)
library(VIM)
library(GJRM)
library(tidyr)

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


# Conferindo missing values restantes
base %>% summarise_all(~ sum(is.na(.)))

# Testando correlação entre variáveis 

base %>% 
  select(resp1, resp2, lance1, lance2) %>% 
  mutate(resp1 = as.numeric(resp1), resp2 = as.numeric(resp2)) %>% 
  cor()  
# conclusao: dado que $\rho$ != 0, vamos utilizar pelo modelo bivariado

# Salvando dados de cada parque.

dados_sitio_da_trindade <- base %>% filter(parque == "sitio da trindade")
dados_13demaio <- base %>% filter(parque == "13demaio")
dados_santosdumont <- base %>% filter(parque == "santosdumont")
dados_lindu <- base %>% filter(parque == "lindu")
dados_caiara <- base %>% filter(parque == "caiara")
dados_macaxeira <- base %>% filter(parque == "macaxeira")
dados_jaqueira <- base %>% filter(parque == "jaqueira")
dados_santana <- base %>% filter(parque == "santana")

# ANALISE CONJUNTA DE TODOS OS PARQUES

  ## Configurando listas de variaveis - **modelo sem renda**
  treat.eq <- resp1 ~ lance1 + idade + sexo + escolar + temperatura + infraestrutura
  out.eq <- resp2 ~ lance2 + idade + sexo + escolar + temperatura + infraestrutura
  f.list <- list(treat.eq, out.eq)
  mr <- c("probit", "probit")
  
  ## Modelo Linear
  bvp <- gjrm(f.list, data=base, Model="B", margins= mr)
  summary(bvp)

coefs <- summary(bvp)
coefs$tableP1
coefs <- summary(bvp$coefficients)

# montando tabela de coeficientes
full_coefs <- 
    cbind(
      coefs$tableP1[ ,c("Estimate", "Std. Error", "Pr(>|z|)")],
      coefs$tableP2[ ,c("Estimate", "Std. Error", "Pr(>|z|)")])
                    
 
full_coefs  
 
stargazer::stargazer(full_coefs, type = "text")



# atribuindo valores médios
lance1_med <- mean(base$lance1)
renda_med <- mean(base$renda)
idade_med <- mean(base$idade)
sexo_med <- mean(base$sexo)
escolar_med <- mean(base$escolar)
temp_med <- mean(base$temperatura)
infra_med <- mean(base$infraestrutura)
lance2_med <- mean(base$lance2)

## coeficientes eq 1
cf_intercept <- bvp$coefficients[1]
cf_lance1 <- bvp$coefficients[2]
cf_idade <- bvp$coefficients[3]
cf_sexo <- bvp$coefficients[4]
cf_escolar <- bvp$coefficients[5]
cf_temp <- bvp$coefficients[6]
cf_infra <- bvp$coefficients[7]

# coeficientes eq 2
cf_intercept2 <- bvp$coefficients[8]
cf_lance2 <- bvp$coefficients[9]
cf_idade2 <- bvp$coefficients[10]
cf_sexo2 <- bvp$coefficients[11]
cf_escolar2 <- bvp$coefficients[12]
cf_temp2 <- bvp$coefficients[13]
cf_infra2 <- bvp$coefficients[14]

## CALCULANDO A DAP 

### Dap equação 1
dap_eq1 <- -(cf_intercept + cf_idade*idade_med + cf_sexo*sexo_med + cf_escolar*escolar_med + cf_temp*temp_med + cf_infra*infra_med)/cf_lance1 # R$ 40,47

# A dap da equação 1 foi de R$ 
round(dap_eq1, digits = 2)

### Dap equação 2
dap_eq2 <- -(cf_intercept + cf_idade2*idade_med + cf_sexo2*sexo_med + cf_escolar2*escolar_med + cf_temp2*temp_med + cf_infra2*infra_med)/cf_lance2 # R$ 39,50 

# A dap da equação 2 foi de R$ 
round(dap_eq2, digits = 2)

# Análise individual dos parques 

## Regressão Parques - individualmente - linear na renda 

bvp.sitiotrindade <- gjrm(f.list, data=dados_sitio_da_trindade, Model="B", margins= mr)
bvp.trezedemaio <- gjrm(f.list, data=dados_13demaio, Model="B", margins= mr)
bvp.santosdumont <- gjrm(f.list, data=dados_santosdumont, Model="B", margins= mr)
bvp.lindu <- gjrm(f.list, data=dados_lindu, Model="B", margins= mr)
bvp.caiara <- gjrm(f.list, data=dados_caiara, Model="B", margins= mr)
bvp.macaxeira <- gjrm(f.list, data=dados_macaxeira, Model="B", margins= mr)
bvp.jaqueira <- gjrm(f.list, data=dados_jaqueira, Model="B", margins= mr)
bvp.santana <- gjrm(f.list, data=dados_santana, Model="B", margins= mr)

# Como eu posso extrair os valores de ('Estimate', 'std. Error', 'Pr(>|z|)') dos resultados do output e coloca-los em um formato de tabela? 
# 'Estimate', 'std. Error', 'Pr(>|z|)'

l.bvp.sitiotrindade <- summary(bvp.sitiotrindade)
l.bvp.trezedemaio <- summary(bvp.trezedemaio)
l.bvp.santosdumont <- summary(bvp.santosdumont)
l.bvp.lindu <- summary(bvp.lindu)
l.bvp.caiara <- summary(bvp.caiara)
l.bvp.macaxeira <- summary(bvp.macaxeira)
l.bvp.jaqueira <- summary(bvp.jaqueira)
l.bvp.santana <- summary(bvp.santana)

library(dplyr)

# extraindo os resultados dos modelos individuais
resultado_sitiotrindade <-
  rbind(l.bvp.sitiotrindade$tableP1, l.bvp.sitiotrindade$tableP2) %>%
  as.data.frame() %>% tibble::rownames_to_column("Variavel") %>% select(!"z value") %>% 
  mutate(Parque = "Sitio Trindade")

resultado_trezedemaio <-
  rbind(l.bvp.trezedemaio$tableP1, l.bvp.trezedemaio$tableP2) %>%
  as.data.frame()  %>% tibble::rownames_to_column("Variavel") %>% select(!"z value") %>% 
  mutate(Parque = "Treze de Maio")

resultado_santosdumont <-
  rbind(l.bvp.santosdumont$tableP1, l.bvp.santosdumont$tableP2) %>%
  as.data.frame() %>% tibble::rownames_to_column("Variavel") %>% select(!"z value")  %>% 
  mutate(Parque = "Santos Dumont")


resultado_lindu <-
  rbind(l.bvp.lindu$tableP1, l.bvp.lindu$tableP2) %>%
  as.data.frame() %>% tibble::rownames_to_column("Variavel") %>% select(!"z value")  %>% 
  mutate(Parque = "Lindu")

resultado_caiara <- 
  rbind(l.bvp.caiara$tableP1, l.bvp.caiara$tableP2) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Variavel") %>% 
  select(!"z value")  %>% 
  mutate(Parque = "Caiara")
 
resultado_macaxeira <- 
  rbind(l.bvp.macaxeira$tableP1, l.bvp.macaxeira$tableP2) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Variavel") %>% 
  select(!"z value") %>% 
  mutate(Parque = "Macaxeira")

resultado_jaqueira <-
  rbind(l.bvp.jaqueira$tableP1, l.bvp.jaqueira$tableP2) %>%
  as.data.frame() %>% tibble::rownames_to_column("Variavel") %>% select(!"z value") %>% 
  mutate(Parque = "Jaqueira")

resultado_santana <-
  rbind(l.bvp.santana$tableP1, l.bvp.santana$tableP2) %>%
  as.data.frame() %>% tibble::rownames_to_column("Variavel") %>% select(!"z value") %>% 
  mutate(Parque = "Santana")

# extraindo valores regressao todos parques juntos

l.bvp.allparques <- summary(bvp)
rbind(l.bvp.allparques$tableP1, l.bvp.allparques$tableP2) %>%
  as.data.frame() %>% tibble::rownames_to_column("Variavel") %>% select(!"z value")

df_parques <- rbind(resultado_santana, resultado_caiara, resultado_macaxeira, 
      resultado_lindu, resultado_santosdumont, resultado_trezedemaio, resultado_sitiotrindade)

df_parques %>% 
  group_by(Equação = df_parques$Parque, `Estimate` = Estimate, `Std. Erro` = `Std. Error`, `p-valor` = `Pr(>|z|)`)


lmtest::waldtest.default(bvp,bvp.caiara, test = c("Chisq", "F"))
lmtest::waldtest(bvp)
mdscore::wald.test(bvp)

library(EdSurvey)
waldTest(model = bvp, coefficients = 2:5)
