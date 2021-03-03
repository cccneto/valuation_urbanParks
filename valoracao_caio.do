import excel "C:\Users\User\Downloads\base_caio.xlsx", sheet("dados") firstrow

            **********************************************************************
			*             GERANDO VARIAVEIS PARA ANALISE DA DAP 		 		 *
			**********************************************************************
generate lnlance1= ln(lance1)
generate lnlance2=ln(lance2)

generate lndrenda= ln(renda)
generate lndrenda1= ln(renda)

generate t1=lance1*(-1)
generate t2=lance2*(-1)
gen lnrendat1 = ln(( renda - t1)/ renda)
gen lnrendat2 = ln(( renda - t2)/ renda)

			
label variable lance1 "Valor 1"
label variable resp1 "Aceita 1?"
label variable lance2 "Valor 2"
label variable resp2 "Aceita 2?"

*********************************************************************************

* Descriptive stastistics of variables used in the analysis
sum resp1 resp2 lance1 lance2 lnlance2 lnlance1 idade sexo renda lnrendat1 lnrendat2 idade escolar distantp t1 t2

* Analisando correlações, principalmente para os lances (1 e 2).
cor lance1 lance2
cor resp1 resp2
cor resp1 resp2 lance1 lance2 lnlance2 lnlance1 idade renda lnrendat1 lnrendat2 idade escolar distantp t1 t2
		
*********************************************************************************************************
*Criando variáveis com os valores médios
		
egen resp1_med = mean(resp1)
egen resp2_med = mean(resp2)		
egen idade_med = mean(idade)
egen escolar_med = mean(educa)
egen renda_med = mean(renda)
egen t1_med= mean(t1)
egen sexo_med= mean(sexo)


