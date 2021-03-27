require(GJRM)
set.seed(123)
x1 <- sample(1:100, size = 20)
bid1 <- sample(c(5, 10, 20, 30), size = 20, replace = T)
bid2 <- sample(c(5, 10, 20, 30), size = 20, replace = T)
ans1 <- sample(c(1,0), size = 20, replace = T)
ans2 <- sample(c(1,0), size = 20, replace = T)
df <- cbind(x1, bid1, bid2, ans1, ans2)
df <- as.data.frame(df)

treat.eq <- ans1 ~ bid1 + x1
out.eq <- ans2 ~ bid2 + x1
f.list <- list(treat.eq, out.eq)
mr <- c("probit", "probit")

## Model
bvp <- gjrm(f.list, data=df, Model="B", margins= mr)
sbvp <- summary(bvp)

# Como eu posso extrair os valores de ('Estimate', 'std. Error', 'Pr(>|z|)') dos resultados do output e coloca-los em um formato de tabela? 

