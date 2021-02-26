library(readxl)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(dplyr)
library(VIM)

# carregando base
baseunificada <- read_excel("C:/Users/User/Dropbox/PROJETO PARQUES URBANOS/BANCO DE DADOS/baseunificada.xlsx")

# ajustando codigo de variaveis 

# (H = 1, M = 0)
baseunificada <- baseunificada %>% 
   mutate(sexo = case_when(sexo == '2' ~ 0, 
                           sexo == '3' ~ 0,
                           TRUE ~ sexo)) %>% 
   glimpse()

baseunificada <- baseunificada %>%
  mutate(racacor = as.character(racacor)) %>% 
  mutate(racacor = case_when(racacor == '1' ~ "branco",
                          racacor == '2' ~ "preto", 
                          racacor == '3' ~ "amarelo",
                          racacor == '4' ~ "pardo",
                          racacor == '5' ~ "indigena", TRUE ~ racacor
                          )) %>%  glimpse()

baseunificada <- baseunificada %>%
              mutate(estadocivil = 
              case_when(estadocivil == '2' ~ "1", TRUE ~ "0")) %>%
              glimpse()  # casado = 2 --> 1

baseunificada <- baseunificada %>%
  mutate(infraestrutura = 
           case_when(infraestrutura == '1' ~ "0",
                     infraestrutura == '2' ~ "0",
                     infraestrutura == '3' ~ "0",
                     TRUE ~ "1")) %>%
  glimpse()  # otimo/bom = 1

baseunificada <- baseunificada %>%
  mutate(qualiar = 
           case_when(qualiar == '1' ~ "0",
                     qualiar == '2' ~ "0",
                     qualiar == '3' ~ "0",
                     TRUE ~ "1")) %>%
  glimpse()  # otimo/bom = 1

# corrigindo nome de variavel
baseunificada <- baseunificada %>% 
              rename(cod_cidade = `cod cidade`,
                     cod_parque = `cod parque`,
                     cod_bairro = `cod bairro`)

# selecionando variáveis de interesse
dados <- baseunificada %>%  
          filter(!is.na(baseunificada)) %>% # filtrando missing values
          select(parque, idade, sexo, cidade, bairro, estadocivil, 
                 escolar, renda, qdepend, objetivo, freqvis,
                 tempoestad, tempoatfis, qualiar, infraestrutura, 
                 lance1, lance2, resp1, resp2) %>%
                glimpse()

dados <- dados %>% mutate(parque = as.factor(parque),
                 idade = as.integer(idade),
                 sexo = as.factor(sexo),
                 cidade = as.factor(cidade),
                 bairro = as.factor(bairro),
                 estadocivil = as.numeric(estadocivil),
                 escolar = as.factor(escolar),
                 renda = as.integer(renda),
                 qdepend = as.integer(qdepend),
                 objetivo = as.factor(objetivo),
                 freqvis = as.integer(freqvis),
                 qualiar = as.numeric(qualiar),
                 infraestrutura = as.numeric(infraestrutura),
                 lance1 = as.integer(lance1),
                 lance2 = as.integer(lance2),
                 resp1 = as.numeric(resp1),
                 resp2 = as.numeric(resp2)
                 )

# conferindo missing values

aggr_plot <- VIM::aggr(dados, col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(dados), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))



# DATA VISUALIZATION 
round(tapply(dados$resp1, dados$lance1, mean), 2)

barplot(tapply(dados$resp1, dados$lance1, mean))

# More advanced barplot suitable for a research report
with(dados, barplot(round(tapply(resp1, lance1, mean), 2), las = 1,
                 xlab = "Bid value categories in reais", 
                 ylab = "Proportion of selecting yes", 
                 yaxs = 'i', xaxs = 'i', 
                 main = "Implied demand 'curve'",
                 border = NA, font.main = 4))


# plots the predicted support at each bid value
plot(sb1, las = 1)    #  las control axis orientation
abline(h = 0.5, lty = 2, col = "red") # adds a horizontal line to the plot 


# A relatively complete model
sb1 <- sbchoice(resp1 ~ 1 | lance1, dist = "logistic", data = dados)

# get confidence intervals for the model sb1
set.seed(123) # The start value can be anything
krCI(sb1)     # The command to get the relevant 95% CI by Krinsky Robb method

set.seed(123) # As it is a new simulation we again set a start value 
bootCI(sb1)   # The command to get the relevant 95% CI by bootstrap method

# The confidence intervals around the median WTP estimate for the two methods are as follows:
  
# For the Krinsky Robb method the 95% CI is $39.90 to $52.61.
# For the Bootstrap method the 95% CI is $ 39.84 to $51.75.

# Portanto, embora tenhamos uma estimativa pontual de $ 45.01 como a DDP mediana, 
# a partir da informação do IC de 95%, pode-se ver que existe uma incerteza 
# considerável em torno dessa estimativa.

# O uso de uma pergunta de acompanhamento nos permite reduzir substancialmente 
# a incerteza da estimativa da DDP; mas antes de estimarmos o modelo DBDC, 
# vamos primeiro incorporar as informações adicionais que temos sobre idade, 
# sexo e renda. Se acharmos que pode haver uma relação entre essas variáveis 
# e a DDP de alguém, devemos incluir essas variáveis no modelo.

sb.full_1 <- sbchoice(resp1 ~ 1 + idade + sexo + estadocivil + qdepend + renda + infraestrutura + qualiar + tempoestad | lance1, dist = "logistic", 
                    data = dados)

# melhor ajuste para sbdc
sb.full <- sbchoice(resp1 ~ 1 + idade + sexo + estadocivil + qdepend + renda + infraestrutura | lance1, dist = "logistic", 
                      data = dados)

# variaveis estatisticamente significativas: idade + sexo + estadocivil + qdepend + renda + infraestrutura
# variaveis nao ES: qualiar + tempoestadia
summary(sb1)
summary(sb.full)
coeftest(sb1)
coeftest(sb.full)

# Comparison of simple and full model WTP estimates
## A detailed plot for the simple model
plot(sb1, las = 1, xlab = "Valor do Lance em Reais", main = "Simple model", 
     cex.main = 0.8, font.main = 4)  # plots  predicted support at each price 
abline(h = 0.5, lty = 2, col = "red")  # adds a horizontal line to the plot 
segments(x0 = 45.01, y0 = -1, x1 = 45.01, y1 = 0.5, col = "red",lty = 2)
points(45.01, 0.5, col = "red", pch = 16)
text(5, 1, "Median WTP estimate = 45.01 reais", pos = 4, cex = 0.7)

## A detailed plot for the full model
plot(sb.full, las = 1, xlab = "Valor do Lance em Reais", main = "Full model", 
     cex.main = 0.8, font.main = 4)  # plots  predicted support at each price 
abline(h = 0.5, lty = 2, col = "green")  # adds a horizontal line  
segments(x0 = 45.50, y0 = -1, x1 = 45.50, y1 = 0.5, col = "green", 
         lty = 2)
points(45.50, 0.5, col = "green", pch = 16)
text(5, 1, "Median WTP estimate = 45.50 euro", pos = 4, cex = 0.7)


# get confidence intervals for the model sb.full
set.seed(123) # We still need a start value for the simulation  
bootCI(sb.full)
bootCI(sb1)

# Como temos um modelo mais completo, há menos incerteza sobre a estimativa 
# da DDP. Portanto, embora as estimativas da DDP mediana sejam semelhantes 
# para o modelo simples e o modelo completo, há um valor prático real em 
# ter uma especificação do modelo completo: intervalos de confiança 
# menores de 95% em torno da estimativa da DDP mediana. Note, entretanto, 
# que o IC 95% ainda cobre uma faixa considerável: R$ 39.99 a R $ 51.94.
# 
# Uma das maneiras mais eficazes de reduzir o nível de incerteza sobre 
# quanto as pessoas estão dispostas a pagar é fazer uma pergunta 
# complementar (**follow-up question**). Agora consideramos este cenário.

## AN INITIAL GENERAL DOUBLE BOUNDED MODEL

db.full <- dbchoice(resp1 + resp2 ~ 1 + idade + sexo + estadocivil + qdepend + renda + infraestrutura | lance1 + lance2, dist = "logistic", 
                    data = dados)
