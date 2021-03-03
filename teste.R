library(readxl)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(dplyr)
library(VIM)

# carregando base
baseunificada <- read.csv("https://raw.githubusercontent.com/cccneto/valuation_urbanParks/master/dados.csv", sep = ";")



# ajustando codigo de variaveis 

baseunificada <- baseunificada %>%
  mutate(infraestrutura = 
           case_when(infraestrutura == '1' ~ "0",
                     infraestrutura == '2' ~ "0",
                     infraestrutura == '3' ~ "0",
                     TRUE ~ "1")) %>%
  glimpse()  # otimo/bom = 1

# selecionando variáveis de interesse
dados <- baseunificada %>%  
  filter(!is.na(baseunificada)) %>% # filtrando missing values
  select(parque, idade, sexo, cidade,  
         escolar, renda, qdepend, objetivo, 
         infraestrutura, sombra, temperatura, tamanho,
         lance1, lance2, resp1, resp2, bidl, bidh) %>%
  glimpse()

dados <- dados %>% mutate(parque = as.factor(parque),
                          idade = as.integer(idade),
                          sexo = as.factor(sexo),
                          cidade = as.factor(cidade),
                          escolar = as.factor(escolar),
                          renda = as.integer(renda),
                          qdepend = as.integer(qdepend),
                          objetivo = as.factor(objetivo),
                          infraestrutura = as.numeric(infraestrutura),
                          lance1 = as.integer(lance1),
                          lance2 = as.integer(lance2),
                          resp1 = as.double(resp1),
                          resp2 = as.double(resp2)
)
# retirar os zeros do lance2
dados <- dados %>% filter(!lance2 < 1)


# conferindo missing values

aggr_plot <- VIM::aggr(dados, col=c('navyblue','red'), 
                       numbers=TRUE, sortVars=TRUE, labels=names(dados), cex.axis=.7, gap=5, ylab=c("Histogram of missing data","Pattern"))



# DATA VISUALIZATION 
round(tapply(dados$resp2, dados$lance2, mean), 2)

barplot(tapply(dados$resp2, dados$lance2, mean))

# More advanced barplot suitable for a research report
with(dados, barplot(round(tapply(resp1, lance1, mean), 2), las = 1,
                    xlab = "Bid value categories in reais", 
                    ylab = "Proportion of selecting yes", 
                    yaxs = 'i', xaxs = 'i', 
                    main = "Implied demand 'curve'",
                    border = NA, font.main = 4))

# A relatively complete model
sb1 <- sbchoice(resp1 ~ 1 | lance1, dist = "logistic", data = dados)

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

db.full <- dbchoice(resp1 + resp2 ~ 1 | lance1 + lance2, dist = "logistic", log= TRUE, data = dados)

#criando variavel para join "NP" e "dados"
NP$ID <- seq(1:length(NP$age))

dados$ID <- seq(1:length(dados$parque))
basecompleta <- left_join(NP, dados, by = "ID")




# testando nova estrutura de dados

base_df.full <- dados %>% select(resp1, resp2, idade, sexo, estadocivil, 
                                 qdepend, renda, infraestrutura, lance1, lance2, bidl, bidh)

teste_df.full <- teste_df.full %>% 
  mutate(nn = case_when(resp1 == '0' & resp2 == '0'  ~ 1, TRUE ~ 0)) %>% 
  mutate(ny = case_when(resp1 == '0' & resp2 == '1'  ~ 1, TRUE ~ 0)) %>% 
  mutate(yy = case_when(resp1 == '1' & resp2 == '1'  ~ 1, TRUE ~ 0)) %>% 
  mutate(yn = case_when(resp1 == '1' & resp2 == '0'  ~ 1, TRUE ~ 0))

teste_df.full <- dados %>% select(resp1, resp2, lance1, bidl, bidh)

teste <- ct2df(teste_df.full, bid1 = "lance1", bid2h = "bidh", bid2l = "bidl",
               yy = "yy", yn = "yn", ny = "ny", nn = "nn", y = "y", n = "n", type = "double")

base_df.full %>% glimpse()

dados %>% summary()

dados %>% select(lance2) %>% filter(lance2 == 0)
