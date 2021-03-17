library(readxl)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(dplyr)
library(VIM)

# carregando base
url <-
  "https://github.com/cccneto/valuation_urbanParks/blob/master/dados/baseunificada.xlsx?raw=true"
destfile <- "base_unificada.xlsx"
curl::curl_download(url, destfile)
baseunificada <- read_excel(destfile)


# ajustando codigo de variaveis

baseunificada <- baseunificada %>%
  mutate(
    infraestrutura = case_when(
      infraestrutura == '1' ~ "0",
      infraestrutura == '2' ~ "0",
      infraestrutura == '3' ~ "0",
      TRUE ~ "1"
    )
  ) %>%
  mutate(escolar = case_when(escolar == '16' ~ "1",
                             TRUE ~ "0")) %>% # 1=superior completo, 0= otherwise
  mutate(sexo = case_when(sexo == '2' ~ "0",
                          sexo == '3' ~ "0",
                          TRUE ~ "1")) %>%
  glimpse()

# selecionando variáveis de interesse
dados <- baseunificada %>%
  filter(!is.na(baseunificada)) %>% # filtrando missing values
  filter(!lance2 < 1) %>% # retirar os zeros do lance2
  select(
    parque,
    idade,
    sexo,
    cidade,
    escolar,
    renda,
    qdepend,
    objetivo,
    infraestrutura,
    sombra,
    temperatura,
    tamanho,
    lance1,
    lance2,
    depend,
    resp1,
    resp2,
    bidl,
    bidh
  ) %>%
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
    depend = as.factor(depend),
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

View(dados)
# testing correlation between variables
dados %>%
  select(resp1, resp2, lance1, lance2) %>%
  cor()  # conclusao: dado que $\rho$ != 0, vamos utilizar pelo modelo bivariado

# testing variables
sb1 <-  sbchoice(resp1 ~ idade | lance1, dist = "logistic", data = dados)
sb2 <-  sbchoice(resp1 ~ sexo | lance1, dist = "logistic", data = dados) # NSS
sb3 <-  sbchoice(resp1 ~ renda | lance1, dist = "logistic", data = dados)
sb4 <-  sbchoice(resp1 ~ escolar | lance1, dist = "logistic", data = dados) # NSS
sb5 <-  sbchoice(resp1 ~ infraestrutura | lance1, dist = "logistic", data = dados)
sb6 <-  sbchoice(resp1 ~ qdepend | lance1, dist = "logistic", data = dados)
sb7 <-  sbchoice(resp1 ~ depend | lance1, dist = "logistic", data = dados)

summary(sb1)
summary(sb2)
summary(sb3)
summary(sb4)
summary(sb5)
summary(sb6)



# plots the predicted support at each bid value
plot(sb1, las = 1)    #  las control axis orientation
abline(h = 0.5, lty = 2, col = "red") # adds a horizontal line to the plot


# A relatively complete model
sb1 <- sbchoice(resp1 ~ 1 | lance1, dist = "logistic", data = dados)
summary(sb1)

# get confidence intervals for the model sb1
set.seed(123) # The start value can be anything
krCI(sb1)     # The command to get the relevant 95% CI by Krinsky Robb method

set.seed(123) # As it is a new simulation we again set a start value
bootCI(sb1)   # The command to get the relevant 95% CI by bootstrap method

# melhor ajuste para sbdc
sb.full <- sbchoice(resp1 ~ 1 + idade | lance1, dist = "logistic",
                    data = dados)


# get confidence intervals for the model sb.full
set.seed(123) # We still need a start value for the simulation
bootCI(sb.full)
bootCI(sb1)

## AN INITIAL GENERAL DOUBLE BOUNDED MODEL

db.full <-
  dbchoice(
    resp1 + resp2 ~ 1 |
      lance1 + lance2,
    dist = "logistic",
    log = TRUE,
    data = dados
  )

#criando variavel para join "NP" e "dados"
NP$ID <- seq(1:length(NP$age))

dados$ID <- seq(1:length(dados$parque))
basecompleta <- left_join(NP, dados, by = "ID")




# testando nova estrutura de dados

base_df.full <-
  dados %>% select(
    resp1,
    resp2,
    idade,
    sexo,
    estadocivil,
    qdepend,
    renda,
    infraestrutura,
    lance1,
    lance2,
    bidl,
    bidh
  )

teste_df.full <- teste_df.full %>%
  mutate(nn = case_when(resp1 == '0' &
                          resp2 == '0'  ~ 1, TRUE ~ 0)) %>%
  mutate(ny = case_when(resp1 == '0' &
                          resp2 == '1'  ~ 1, TRUE ~ 0)) %>%
  mutate(yy = case_when(resp1 == '1' &
                          resp2 == '1'  ~ 1, TRUE ~ 0)) %>%
  mutate(yn = case_when(resp1 == '1' & resp2 == '0'  ~ 1, TRUE ~ 0))

teste_df.full <- dados %>% select(resp1, resp2, lance1, bidl, bidh)

teste <-
  ct2df(
    teste_df.full,
    bid1 = "lance1",
    bid2h = "bidh",
    bid2l = "bidl",
    yy = "yy",
    yn = "yn",
    ny = "ny",
    nn = "nn",
    y = "y",
    n = "n",
    type = "double"
  )

base_df.full %>% glimpse()

dados %>% summary()

dados %>% select(lance2) %>% filter(lance2 == 0)
