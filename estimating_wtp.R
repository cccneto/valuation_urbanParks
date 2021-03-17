library(readxl)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(dplyr)
library(VIM)

# carregando base
url <- "https://github.com/cccneto/valuation_urbanParks/blob/master/dados/base_caio.xlsx?raw=true"
destfile <- "base_caio.xlsx"
curl::curl_download(url, destfile)
base_caio <- read_excel(destfile)

base_caio <- base_caio %>% 
            select(quant_visit, resp1, resp2, lance1, lance2, idade, sexo, educa, renda, temp_visit) %>% 
            filter(!is.na(resp1)) %>% 
            filter(!is.na(resp1)) %>% 
            filter(!is.na(lance1)) %>% 
            filter(!is.na(lance2)) %>% 
            mutate(temp_visit = as.factor(temp_visit),
                   quant_visit = as.factor(quant_visit),
                   educa = as.factor(educa))

# conferindo missing values
  
aggr_plot <- VIM::aggr(base_caio, col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(base_caio), cex.axis=.7, gap=5, ylab=c("Histogram of missing data","Pattern"))



# DATA VISUALIZATION 
round(tapply(base_caio$resp1, base_caio$lance1, mean), 2)

barplot(tapply(base_caio$resp1, base_caio$lance1, mean))

# More advanced barplot suitable for a research report
with(base_caio, barplot(round(tapply(resp1, lance1, mean), 2), las = 1,
                 xlab = "Bid value categories in reais", 
                 ylab = "Proportion of selecting yes", 
                 yaxs = 'i', xaxs = 'i', 
                 main = "Implied demand 'curve'",
                 border = NA, font.main = 4))

# A relatively complete model
sb1 <- sbchoice(resp1 ~ idade | lance1, dist = "logistic", data = base_caio)
sb2 <- sbchoice(resp1 ~ sexo | lance1, dist = "logistic", data = base_caio)
sb3 <- sbchoice(resp1 ~ renda | lance1, dist = "logistic", data = base_caio)
sb4 <- sbchoice(resp1 ~ educa | lance1, dist = "logistic", data = base_caio)
sb5 <- sbchoice(resp1 ~ temp_visit | lance1, dist = "logistic", data = base_caio)
sb6 <- sbchoice(resp1 ~ quant_visit | lance1, dist = "logistic", data = base_caio)

summary(sb1)
summary(sb2)
summary(sb3)
summary(sb4)
summary(sb5)
summary(sb6)

# plots the predicted support at each bid value
plot(sb1, las = 1)    #  las control axis orientation
abline(h = 0.5, lty = 2, col = "red") # adds a horizontal line to the plot 


set.seed(123) # As it is a new simulation we again set a start value 
bootCI(sb1)   # The command to get the relevant 95% CI by bootstrap method

# melhor ajuste para sbdc
sb.eq1 <- sbchoice(resp1 ~ 1 + idade + sexo + renda + educa + temp_visit| lance1, dist = "logistic", 
                      data = base_caio)

sb.eq2 <- sbchoice(resp2 ~ 1 + idade + sexo + renda + educa + temp_visit| lance2, dist = "logistic", 
                    data = base_caio)

# variaveis estatisticamente significativas: idade + sexo + estadocivil + qdepend + renda + infraestrutura
# variaveis nao ES: qualiar + tempoestadia
summary(sb.eq1)
summary(sb.eq2)

# AN INITIAL GENERAL DOUBLE BOUNDED MODEL

db.full <- dbchoice(resp1 + resp2 ~ idade | lance1 + lance2, 
                   data = base_caio)


