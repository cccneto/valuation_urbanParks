library(readxl)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(dplyr)
library(VIM)
library(GJRM)

# carregando base
url <-
  "https://github.com/cccneto/valuation_urbanParks/blob/master/dados/baseunificada.xlsx?raw=true"
destfile <- "base_unificada.xlsx"
curl::curl_download(url, destfile)
baseunificada <- read_excel(destfile)



# ajustando codigo de variaveis

baseunificada <- baseunificada %>%
  mutate(
    infraestrutura = case_when( # dummie para bom/otimo = 1
      infraestrutura == '1' ~ "0",
      infraestrutura == '2' ~ "0",
      infraestrutura == '3' ~ "0",
      TRUE ~ "1"), 
    temperatura = case_when(  # dummie para bom/otimo = 1
       temperatura > "3" ~ "1", TRUE ~ "0"),
    sombra = case_when( # dummie para bom/otimo = 1
      sombra == '1' ~ "0",
      sombra == '2' ~ "0",
      sombra == '3' ~ "0", TRUE ~ "1"), 
    escolar = case_when(escolar > '15' ~ "1", TRUE ~ "0"), # 1=superior completo, 0= otherwise
    sexo = case_when(sexo == '2' ~ "0",
                     sexo == '3' ~ "0",
                     TRUE ~ "1")) 

baseunificada %>% select(temperatura) %>% unique()
# selecionando variáveis de interesse
dados <- baseunificada %>%
  filter(!is.na(baseunificada), !is.na(renda)) %>% # filtrando missing values
  filter(!renda < 1) %>% 
  filter(!lance2 < 1) %>% # retirar os zeros do lance2
  select( parque, idade, sexo, cidade, escolar,renda, qdepend, 
          objetivo, infraestrutura, sombra, temperatura, tamanho,
          lance1,lance2, depend, resp1, resp2, bidl, bidh)

# ajustando classes das variaveis
dados <- dados %>%
  mutate(
    parque = as.factor(parque),
    idade = as.integer(idade),
    sexo = as.factor(sexo),
    cidade = as.factor(cidade),
    escolar = as.factor(escolar),
    renda = as.integer(renda),
    qdepend = as.integer(qdepend),
    depend = as.factor(depend),
    objetivo = as.factor(objetivo),
    infraestrutura = as.factor(infraestrutura),
    sombra = as.factor(sombra),
    temperatura = as.factor(temperatura),
    lance1 = as.integer(lance1),
    lance2 = as.integer(lance2),
    resp1 = as.double(resp1),
    resp2 = as.double(resp2),
    t1 = lance1*-1,
    t2 = lance2*-1,
    lnRendat1 = log((renda - t1)/renda),
    lnRendat2 = log((renda - t2)/renda)
  )



# testing correlation between variables
dados %>% select(resp1, resp2, lance1, lance2) %>% cor()  
# conclusao: dado que $\rho$ != 0, vamos utilizar pelo modelo bivariado


# saving data from each Park - the full file that contains all data is 'dados'.
dados_sitio_da_trindade <- dados %>% filter(parque == "sitio da trindade")
dados_13demaio <- dados %>% filter(parque == "13demaio")
dados_santosdumont <- dados %>% filter(parque == "santosdumont")
dados_lindu <- dados %>% filter(parque == "lindu")
dados_caiara <- dados %>% filter(parque == "caiara")
dados_macaxeira <- dados %>% filter(parque == "macaxeira")
dados_jaqueira <- dados %>% filter(parque == "jaqueira")
dados_santana <- dados %>% filter(parque == "santana")

dados %>% filter(parque == "macaxeira") %>% relocate(temperatura) %>% select(temperatura) %>% count(temperatura)


# configurando listas de variaveis
treat.eq <- resp1 ~ lance1 + idade + sexo + escolar + temperatura + infraestrutura
out.eq <- resp2 ~ lance2 + idade + sexo + escolar + temperatura + infraestrutura
f.list <- list(treat.eq, out.eq)
mr <- c("probit", "probit")


treat.eq1 <- resp1 ~ lance1 + idade + sexo + escolar + lnRendat1 + temperatura + infraestrutura
out.eq1 <- resp2 ~ lance2 + idade + sexo + escolar + lnRendat2 + temperatura + infraestrutura
f.list1 <- list(treat.eq1, out.eq1)
mr <- c("probit", "probit")



# regression from all parks 
bvp <- gjrm(f.list, data=dados, Model="B", margins= mr)
summary(bvp)

bvp_ll <- gjrm(f.list1, data=dados, Model="B", margins= mr)
summary(bvp_ll)


# each park - linear na renda 

db.sitiotrindade <-gjrm(f.list, data=dados_sitio_da_trindade, Model="B", margins= mr)
db.trezedemaio <- gjrm(f.list, data=dados_13demaio, Model="B", margins= mr)
db.santosdumont <- gjrm(f.list, data=dados_santosdumont, Model="B", margins= mr)
db.lindu <- gjrm(f.list, data=dados_lindu, Model="B", margins= mr)
db.caiara <- gjrm(f.list, data=dados_caiara, Model="B", margins= mr)
db.macaxeira <- gjrm(f.list, data=dados_macaxeira, Model="B", margins= mr)
db.jaqueira <- gjrm(f.list, data=dados_jaqueira, Model="B", margins= mr)
db.santana <- gjrm(f.list, data=dados_santana, Model="B", margins= mr)


# each park - log linear

db.sitiotrindade <-gjrm(f.list1, data=dados_sitio_da_trindade, Model="B", margins= mr)
db.trezedemaio <- gjrm(f.list1, data=dados_13demaio, Model="B", margins= mr)
db.santosdumont <- gjrm(f.list1, data=dados_santosdumont, Model="B", margins= mr)
db.lindu <- gjrm(f.list1, data=dados_lindu, Model="B", margins= mr)
db.caiara <- gjrm(f.list1, data=dados_caiara, Model="B", margins= mr)
db.macaxeira <- gjrm(f.list1, data=dados_macaxeira, Model="B", margins= mr)
db.jaqueira <- gjrm(f.list1, data=dados_jaqueira, Model="B", margins= mr)
db.santana <- gjrm(f.list1, data=dados_santana, Model="B", margins= mr)

dados_macaxeira %>% 


lance1 + idade + sexo + escolar + temperatura + infraestrutura


dados %>% 
   summarise(across(.cols = c(renda, idade),
           .fns = mean(),
           .names = "med_{col}"))


# modelo
bvp2 <- gjrm(f.list, data=base_caio, Model="B", margins= mr)
summary(bvp2)
AIC(bvp2)
conv.check(bvp2)
BIC(bvp2)
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
