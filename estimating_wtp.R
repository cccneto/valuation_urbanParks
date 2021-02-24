library(readxl)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(dplyr)

# carregando base
basesaude2 <- read_excel("C:/Users/User/Dropbox/PROJETO PARQUES URBANOS/BANCO DE DADOS/basesaude2.xlsx")

# ajustando codigo de variaveis 

# (H = 1, M = 0)
basesaude2 <- basesaude2 %>% 
   mutate(sexo = case_when(sexo == '2' ~ 0, TRUE ~ sexo)) %>% 
   glimpse()

basesaude2 <- basesaude2 %>%
  mutate(racacor = as.character(racacor)) %>% 
  mutate(racacor = case_when(racacor == '1' ~ "branco",
                          racacor == '2' ~ "preto", 
                          racacor == '3' ~ "amarelo",
                          racacor == '4' ~ "pardo",
                          racacor == '5' ~ "indigena", TRUE ~ racacor
                          )) %>%  glimpse()

basesaude2 <- basesaude2 %>%
              mutate(estadocivil = 
              case_when(estadocivil == '2' ~ "1", TRUE ~ "0")) %>%
              glimpse()  # casado = 2 --> 1

basesaude2 <- basesaude2 %>%
  mutate(infraestrutura = 
           case_when(infraestrutura == '1' ~ "0",
                     infraestrutura == '2' ~ "0",
                     infraestrutura == '3' ~ "0",
                     TRUE ~ "1")) %>%
  glimpse()  # otimo/bom = 1

basesaude2 <- basesaude2 %>%
  mutate(qualiar = 
           case_when(qualiar == '1' ~ "0",
                     qualiar == '2' ~ "0",
                     qualiar == '3' ~ "0",
                     TRUE ~ "1")) %>%
  glimpse()  # otimo/bom = 1

# corrigindo nome de variavel
basesaude2 <- basesaude2 %>% 
              rename(cod_cidade = `cod cidade`,
                     cod_parque = `cod parque`,
                     cod_bairro = `cod bairro`)

# selecionando variáveis de interesse
dados <- basesaude2 %>%  
          select(parque, idade, sexo, cidade, bairro, estadocivil, 
                 escolar, renda, depend, qdepend, objetivo, freqvis,
                 tempoestad, tempoatfis, qualiar, infraestrutura, 
                 lance1, lance2, resp1, resp2) %>% glimpse()

dados <- dados %>% 
                 mutate(parque = as.factor(parque),
                 idade = as.integer(idade),
                 sexo = as.factor(sexo),
                 cidade = as.factor(cidade),
                 bairro = as.factor(bairro),
                 estadocivil = as.numeric(estadocivil),
                 escolar = as.factor(escolar),
                 renda = as.integer(renda),
                 depend = as.numeric(depend),
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
