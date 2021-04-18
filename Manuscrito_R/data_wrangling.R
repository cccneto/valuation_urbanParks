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

write.csv2(base, file = "base_cleaned.csv")

# Testando correlação entre variáveis 

base %>% 
  select(resp1, resp2, lance1, lance2) %>% 
  mutate(resp1 = as.numeric(resp1), resp2 = as.numeric(resp2)) %>% 
  cor()  
# conclusao: dado que $\rho$ != 0, vamos utilizar pelo modelo bivariado