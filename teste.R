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
          temperatura == '1' ~ "0",
          temperatura == '2' ~ "0",
          temperatura == '3' ~ "0",
          TRUE ~ "1"),
     sombra = case_when( # dummie para bom/otimo = 1
              sombra == '1' ~ "0",
              sombra == '2' ~ "0",
              sombra == '3' ~ "0",
              TRUE ~ "1"), 
     escolar = case_when(escolar == '16' ~ "1",
                             TRUE ~ "0"), # 1=superior completo, 0= otherwise
     sexo = case_when(sexo == '2' ~ "0",
                          sexo == '3' ~ "0",
                          TRUE ~ "1")) %>%
  glimpse()

# selecionando variáveis de interesse
dados <- baseunificada %>%
  filter(!is.na(baseunificada)) %>% # filtrando missing values
  filter(!lance2 < 1) %>% # retirar os zeros do lance2
  select( parque, idade, sexo, cidade, escolar,renda, qdepend, 
          objetivo, infraestrutura, sombra, temperatura, tamanho,
           lance1,lance2, depend, resp1, resp2, bidl, bidh) %>%
  glimpse()

dados <- dados %>%
  mutate(
    parque = as.factor(parque),
    idade = as.integer(idade),
    sexo = as.factor(sexo),
    cidade = as.factor(cidade),
    escolar = as.factor(escolar),
    renda = as.integer(renda),
    qdepend = as.integer(qdepend),
    depend = as.double(depend),
    objetivo = as.factor(objetivo),
    infraestrutura = as.numeric(infraestrutura),
    lance1 = as.integer(lance1),
    lance2 = as.integer(lance2),
    resp1 = as.double(resp1),
    resp2 = as.double(resp2)
  )

# conferindo missing values

aggr_plot <- VIM::aggr(
  dados, col = c('navyblue', 'red'), numbers = TRUE,
  sortVars = TRUE, labels = names(dados),
  cex.axis = .8, gap = 3, ylab = c("Histogram of missing data", "Pattern")
)

# DATA VISUALIZATION

# More advanced barplot suitable for a research report
with(
  dados,
  barplot(
    round(tapply(resp1, lance1, mean), 2),
    las = 1,
    xlab = "Bid value categories in reais",
    ylab = "Proportion of selecting yes",
    yaxs = 'i',
    xaxs = 'i',
    main = "Implied demand 'curve'",
    border = NA,
    font.main = 4
  )
)

# testing correlation between variables
dados %>%
  select(resp1, resp2, lance1, lance2) %>%
  cor()  # conclusao: dado que $\rho$ != 0, vamos utilizar pelo modelo bivariado

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
# 
# # A relatively complete model
# sb1 <- sbchoice(resp1 ~ 1 | lance1, dist = "logistic", data = dados)
# summary(sb1)

# set.seed(123) # As it is a new simulation we again set a start value
# bootCI(sb1)   # The command to get the relevant 95% CI by bootstrap method

# # melhor ajuste para sbdc
sb.full <- sbchoice(resp1 ~ 1 + idade | lance1, dist = "logistic",
                    data = dados)

# # get confidence intervals for the model sb.full
# set.seed(123) # We still need a start value for the simulation
# bootCI(sb.full)
# bootCI(sb1)

## CLASSIC BIVARIATE PROBIT

# pela AIC curve 'out1' tem melhor ajuste
out1 <- gjrm(list(resp1 ~ lance1 + idade + sexo + renda + escolar + infraestrutura + qdepend + depend,
                  resp2 ~ lance2 + idade + sexo + renda + escolar + infraestrutura + qdepend + depend), 
             data = dados, margins = c("probit", "probit"), Model = "B")

out2 <- gjrm(list(resp1 ~ lance1 + idade + sexo + renda + escolar + infraestrutura + qdepend,
                  resp2 ~ lance2 + idade + sexo + renda + escolar + infraestrutura + qdepend), 
             data = dados, margins = c("probit", "probit"), Model = "B")

out3 <- gjrm(list(resp1 ~ lance1 + idade + sexo + renda + escolar + infraestrutura,
                  resp2 ~ lance2 + idade + sexo + renda + escolar + infraestrutura), 
             data = dados, margins = c("probit", "probit"), Model = "B")


out4 <- gjrm(list(resp1 ~ lance1 + idade + sexo + escolar + infraestrutura,
                  resp2 ~ lance2 + idade + sexo + escolar + infraestrutura), 
             data = dados, margins = c("probit", "probit"), Model = "B")


