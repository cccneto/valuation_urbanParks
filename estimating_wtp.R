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
sb1 <- sbchoice(resp1 ~ 1 | lance1, dist = "logistic", data = base_caio)

# plots the predicted support at each bid value
plot(sb1, las = 1)    #  las control axis orientation
abline(h = 0.5, lty = 2, col = "red") # adds a horizontal line to the plot 


# get confidence intervals for the model sb1
set.seed(123) # The start value can be anything
krCI(sb1)     # The command to get the relevant 95% CI by Krinsky Robb method

set.seed(123) # As it is a new simulation we again set a start value 
bootCI(sb1)   # The command to get the relevant 95% CI by bootstrap method

# melhor ajuste para sbdc
sb.full <- sbchoice(resp1 ~ 1 + idade + sexo + renda | lance1, dist = "logistic", 
                      data = base_caio)

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
segments(x0 = 22.69, y0 = -1, x1 = 22.69, y1 = 0.5, col = "red",lty = 2)
points(22.69, 0.5, col = "red", pch = 16)
text(5, 1, "Median WTP estimate = 45.01 reais", pos = 4, cex = 0.7)

## A detailed plot for the full model
plot(sb.full, las = 1, xlab = "Valor do Lance em Reais", main = "Full model", 
     cex.main = 0.8, font.main = 4)  # plots  predicted support at each price 
abline(h = 0.5, lty = 2, col = "green")  # adds a horizontal line  
segments(x0 = 22.79, y0 = -1, x1 = 22.79, y1 = 0.5, col = "green", 
         lty = 2)
points(22.79, 0.5, col = "green", pch = 16)
text(5, 1, "Median WTP estimate = 22.79 euro", pos = 4, cex = 0.7)


# get confidence intervals for the model sb.full
set.seed(123) # We still need a start value for the simulation  
bootCI(sb.full)
bootCI(sb1)

# AN INITIAL GENERAL DOUBLE BOUNDED MODEL

db.full <- dbchoice(resp1 + resp2 ~ idade | lance1 + lance2, 
                   data = base_caio)

