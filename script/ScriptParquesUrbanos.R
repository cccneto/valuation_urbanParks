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
View(baseunificada)


# ajustando codigo de variaveis

baseunificada <- baseunificada %>%
  mutate(
    infraestrutura = case_when( # dummie para bom/otimo = 1
      infraestrutura == '1' ~ "0",
      infraestrutura == '2' ~ "0",
      infraestrutura == '3' ~ "0",
      TRUE ~ "1"), 
    temperatura = case_when(  # dummie para bom/otimo = 1
      temperatura == '1' ~ "0",
      temperatura == '2' ~ "0",
      temperatura == '3' ~ "0",
      TRUE ~ "1"),
    sombra = case_when( # dummie para bom/otimo = 1
      sombra == '1' ~ "0",
      sombra == '2' ~ "0",
      sombra == '3' ~ "0",
      TRUE ~ "1"), 
    escolar = case_when(escolar > '15' ~ "1",
                        TRUE ~ "0"), # 1=superior completo, 0= otherwise
    sexo = case_when(sexo == '2' ~ "0",
                     sexo == '3' ~ "0",
                     TRUE ~ "1")) 

# selecionando variáveis de interesse
dados <- baseunificada %>%
  filter(!is.na(baseunificada), !is.na(renda)) %>% # filtrando missing values
  filter(!renda < 1) %>% 
  filter(!lance2 < 1) %>% # retirar os zeros do lance2
  select( parque, idade, sexo, cidade, escolar,renda, qdepend, 
          objetivo, infraestrutura, sombra, temperatura, tamanho,
          lance1,lance2, depend, resp1, resp2, bidl, bidh)


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

# dados de temperatura estão faltantes para "Macaxeira"
dados %>% filter(parque == "macaxeira") %>% relocate(temperatura) %>% select(temperatura) %>% count(temperatura)



# conferindo missing values

aggr_plot <- VIM::aggr( dados, col = c('navyblue', 'red'), numbers = TRUE,
  sortVars = TRUE, labels = names(dados), cex.axis = .8, gap = 3, ylab = c("Histogram of missing data", "Pattern")
)

# DATA VISUALIZATION

# More advanced barplot suitable for a research report
with( dados, 
      barplot( round(tapply(resp1, lance1, mean), 2), las = 1,
    xlab = "Bid value categories in reais", ylab = "Proportion of selecting yes",
    yaxs = 'i', xaxs = 'i', main = "Implied demand 'curve'", border = NA, font.main = 4)
)

# testing correlation between variables
dados %>% select(resp1, resp2, lance1, lance2) %>% cor()  
# conclusao: dado que $\rho$ != 0, vamos utilizar pelo modelo bivariado


# testing variables to all parks
sb1 <-  sbchoice(resp1 ~ idade | lance1, dist = "logistic", data = dados) # NSS
sb2 <-  sbchoice(resp1 ~ sexo | lance1, dist = "logistic", data = dados) # NSS
sb3 <-  sbchoice(resp1 ~ renda | lance1, dist = "logistic", data = dados)
sb4 <-  sbchoice(resp1 ~ escolar | lance1, dist = "logistic", data = dados) # NSS
sb5 <-  sbchoice(resp1 ~ infraestrutura | lance1, dist = "logistic", data = dados)
sb6 <-  sbchoice(resp1 ~ qdepend | lance1, dist = "logistic", data = dados)
sb7 <-  sbchoice(resp1 ~ depend | lance1, dist = "logistic", data = dados)  # NSS

# saving data from each Park - the full file that contains all data is 'dados'.
dados_sitio_da_trindade <- dados %>% filter(parque == "sitio da trindade")
dados_13demaio <- dados %>% filter(parque == "13demaio")
dados_santosdumont <- dados %>% filter(parque == "santosdumont")
dados_lindu <- dados %>% filter(parque == "lindu")
dados_caiara <- dados %>% filter(parque == "caiara")
dados_macaxeira <- dados %>% filter(parque == "macaxeira")
dados_jaqueira <- dados %>% filter(parque == "jaqueira")
dados_santana <- dados %>% filter(parque == "santana")


# # plots the predicted support at each bid value
# plot(sb1, las = 1)    #  las control axis orientation
# abline(h = 0.5, lty = 2, col = "red") # adds a horizontal line to the plot
# 

DCchoice::sbchoice()

# # A relatively complete model
sb1 <- sbchoice(resp1 ~ 1 + idade + sexo + escolar + temperatura + infraestrutura | lance1, dist = "logistic", data = dados)
summary(sb1)

set.seed(123) # As it is a new simulation we again set a start value
bootCI(sb1)   # The command to get the relevant 95% CI by bootstrap method

sb2 <- sbchoice(resp2 ~ 1 + idade + sexo + escolar + temperatura + infraestrutura | lance2, dist = "logistic", data = dados)
summary(sb2)
set.seed(123) # As it is a new simulation we again set a start value
bootCI(sb2)


## CLASSIC BIVARIATE PROBIT

# pela AIC curve 'out1' tem melhor ajuste

treat.eq <- resp1 ~ lance1 + idade + sexo + escolar + temperatura + infraestrutura
out.eq <- resp2 ~ lance2 + idade + sexo + escolar + temperatura + infraestrutura
f.list <- list(treat.eq, out.eq)
mr <- c("probit", "probit")


treat.eq1 <- resp1 ~ lance1 + idade + sexo + escolar + lnRendat1 + temperatura + infraestrutura
out.eq1 <- resp2 ~ lance2 + idade + sexo + escolar + lnRendat2 + temperatura + infraestrutura
f.list1 <- list(treat.eq1, out.eq1)
mr <- c("probit", "probit")



# regression log linear income from all parks 
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


