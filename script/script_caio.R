library(readxl)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(dplyr)
library(VIM)

library(GJRM)

# carregando base
library(readxl)
url <- "https://github.com/cccneto/valuation_urbanParks/blob/master/dados/base_caio.xlsx?raw=true"
destfile <- "base_caio.xlsx"
curl::curl_download(url, destfile)
base_caio <- read_excel(destfile)

base_caio <- base_caio %>% 
  mutate(renda_med = mean(renda),
         idade_med = mean(idade),
         sexo_med = mean(sexo),
         educa_med = mean(educa),
         lance1 = lance1*-1,
         lance2 = lance2*-1)

treat.eq <- resp1 ~ idade + sexo + educa + lance1
out.eq <- resp2 ~ idade + sexo + educa + lance2
f.list <- list(treat.eq, out.eq)
mr <- c("probit", "probit")

# modelo
bvp2 <- gjrm(f.list, data=base_caio, Model="B", margins= mr)
summary(bvp2)

# criando variaveis para calculo dap
# equation 1
coef_renda <- bvp2$coefficients["renda"]
coef_idade <- bvp2$coefficients["idade"]
coef_sexo <- bvp2$coefficients["sexo"]
coef_educa <- bvp2$coefficients["educa"]
coef_lance1 <- bvp2$coefficients["lance1"]
coef_intercept <- bvp2$coefficients["(Intercept)"]

# equation 2
# coef_renda2 <- bvp2$coefficients[]
coef_idade2 <- bvp2$coefficients[7]
coef_sexo2 <- bvp2$coefficients[8]
coef_educa2 <- bvp2$coefficients[9]
coef_lance22 <- bvp2$coefficients[10]
coef_intercept2 <- bvp2$coefficients[6]

# calculo da dap

#equação 1
dap_eq1 <- ((coef_intercept) + (coef_idade*base_caio$idade_med) + 
              (coef_sexo*base_caio$sexo_med) + 
              (coef_educa*base_caio$educa_med)/coef_lance1) # R$ 12,35

# equação 2
dap_eq2 <- ((coef_intercept2) + (coef_idade2*base_caio$idade_med) + 
              (coef_sexo2*base_caio$sexo_med) + 
              (coef_educa2*base_caio$educa_med)/coef_lance22) # R$ 15,08


