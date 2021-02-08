**INTUITO DO DO-FILE É FAZER UM GRANDE LOOP PARA PEGAR UMA ÚNICA INFORMAÇÃO 
*DE DIFERENTES BASES ESTADUAIS DISPONIBILIZADAS NO REPOSITÓRIO DE DADOS DO TSE
*A INFORMAÇÃO NESSE CASO É A PORCENTAGEM DE APROVAÇÃO DE PREFEITOS E VEREADORES DE ESQUERDA NOS MUNICÍPIOS BRASILEIROS

*A PARTIR DE 2012, AS BASES DO TSE VÊM EM CSVs SEPARADAS POR ESTADO, E DAÍ VEM A NECESSIDADE DE MAIS FACILMENTE ITERAR PELAS DIFERENTES BASES 

** primeira parte : o loop principal e o que ele faz
*AQUI O PROCESSO É FEITO PARA 4 ANOS NO LOOP DE FORA
*E A ITERAÇÃO DENTRO PARA CADA UM DOS ESTADOS
*NO FUNDO, O CÓDIGO ACESSA PARA CADA ANO, OS 27 DIFERENTES ESTADOS 

*DENTRO DE CADA ESTADO, O CÓDIGO CALCULA A PORCENTAGEM DE CANDIDATOS ELEITOS DE PARTIDOS DE ESQUERDA SELECIONADOS EM RELAÇÃO A 
*TODOS OS CANDIDATOS QUE CONCORRIAM. A IDEIA É AVALIAR PARA CADA MUNICÍPIO DE CADA ESTADO, COMO FOI A PARTICIPAÇÃO DE PARTIDOS CONHECIDOS DE ESQUERDA

foreach i in 2012 2008 2004 2000{

foreach l in AC RR RO AM AP PE RN SE MA PA TO GO MT MS AL BA ES MG PB PI PR RJ RN SC SP RS{

clear all
import delimited "C:\Users\Matheus\Desktop\tcc\excel\politica\candidatostudo`i'\consulta_cand_`i'_`l'.txt", delimiter(";") 

gen eleito = 1 if (v43 == "ELEITO" | v43 == "ELEITO POR QP" | v43 == "ELEITO POR MÉDIA")
gen esquerda = 1 if (v19 == "PT" | v19 == "PSOL" | v19 == "PCB" | v19 == "PC do B" | v19 == "PSTU")
gen esqel = 1 if eleito==1 & esquerda == 1

replace eleito = 0 if eleito==.
replace esquerda = 0 if esquerda==.
replace esqel = 0 if esqel==.

collapse (sum) eleito esqel, by(v8)

gen propesqel = esqel/eleito
gen uf = "`l'"
drop if v8 ==""
save "C:\Users\Matheus\Desktop\baseporestado\a`i'\base`l'.dta", replace
}
}

**parte dois: AQUI FAZEMOS UM APPEND DAS DIFERENTES BASES CRIADAS PARA TER TODOS OS MUNICIPIOS BRASILEIROS NAQUELE ANO
* para 2012

u "C:\Users\Matheus\Desktop\baseporestado\a2012\baseAC.dta", clear

foreach l in RR RO AM AP PE RN SE MA PA TO GO MT MS AL BA ES MG PB PI PR RJ RN SC SP RS{

append using "C:\Users\Matheus\Desktop\baseporestado\a2012\base`l'.dta"

save "C:\Users\Matheus\Desktop\baseporestado\a2012\base3`l'.dta", replace
u "C:\Users\Matheus\Desktop\baseporestado\a2012\base3`l'.dta", clear

}


u "C:\Users\Matheus\Desktop\baseporestado\a2012\base3RS.dta", clear


*para 2008


u "C:\Users\Matheus\Desktop\baseporestado\a2008\baseAC.dta", clear

foreach l in RR RO AM AP PE RN SE MA PA TO GO MT MS AL BA ES MG PB PI PR RJ RN SC SP RS{

append using "C:\Users\Matheus\Desktop\baseporestado\a2008\base`l'.dta"

save "C:\Users\Matheus\Desktop\baseporestado\a2008\base3`l'.dta", replace
u "C:\Users\Matheus\Desktop\baseporestado\a2008\base3`l'.dta", clear

}


u "C:\Users\Matheus\Desktop\baseporestado\a2008\base3RS.dta", clear


*para 2004



u "C:\Users\Matheus\Desktop\baseporestado\a2004\baseAC.dta", clear

foreach l in RR RO AM AP PE RN SE MA PA TO GO MT MS AL BA ES MG PB PI PR RJ RN SC SP RS{

append using "C:\Users\Matheus\Desktop\baseporestado\a2008\base`l'.dta"

save "C:\Users\Matheus\Desktop\baseporestado\a2004\base3`l'.dta", replace
u "C:\Users\Matheus\Desktop\baseporestado\a2004\base3`l'.dta", clear

}


u "C:\Users\Matheus\Desktop\baseporestado\a2004\base3RS.dta", clear



*para 2000



u "C:\Users\Matheus\Desktop\baseporestado\a2000\baseAC.dta", clear

foreach l in RR RO AM AP PE RN SE MA PA TO GO MT MS AL BA ES MG PB PI PR RJ RN SC SP RS{

append using "C:\Users\Matheus\Desktop\baseporestado\a2000\base`l'.dta"

save "C:\Users\Matheus\Desktop\baseporestado\a2000\base3`l'.dta", replace
u "C:\Users\Matheus\Desktop\baseporestado\a2000\base3`l'.dta", clear

}


u "C:\Users\Matheus\Desktop\baseporestado\a2000\base3RS.dta", clear



*POR FIM JUNTAMOS LATERALMENTE OS MUNICIPIOS PARA QUE TENHAMOS NAS LINHAS OS 5560 MUNICÍPIOS BRASILEIROS E EM CADA COLUNA OS DADOS
*DA "ACEITAÇÃO DA ESQUERDA" PARA CADA UM DOS QUATRO DIFERENTES ANOS: 2000, 2004, 2008, 2012

foreach i in 00 04 08 12{

use "C:\Users\Matheus\Desktop\stata\atigaspoli`i'.dta", clear

drop if ibge ==. 

duplicates drop ibge, force

save "C:\Users\Matheus\Desktop\stata\atigaspoli2`i'.dta", replace

}


u "C:\Users\Matheus\Desktop\stata\atigaspoli212.dta", clear

merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\atigaspoli208.dta"

merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\atigaspoli204.dta", gen(_merge2)

merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\atigaspoli200.dta", gen(_merge3)

save "C:\Users\Matheus\Desktop\stata\baseantigapolicompleta.dta", replace


gen media_aprovacao_esq = (propesqel00 + propesqel04 + propesqel08 + propesqel12)/4 if propesqel00 !=. &  propesqel04!=. & /// 
propesqel08!=. & propesqel12!=.

replace media_aprovacao_esq = ( propesqel04 + propesqel08 + propesqel12)/3 if propesqel00 ==. &  propesqel04!=. & /// 
propesqel08!=. & propesqel12!=.

replace media_aprovacao_esq = ( propesqel08 + propesqel12)/2 if propesqel00 ==. &  propesqel04==. & /// 
propesqel08!=. & propesqel12!=.

replace  media_aprovacao_esq = propesqel12 if propesqel00 ==. &  propesqel04==. & /// 
propesqel08==. & propesqel12!=.

tostring ibge, replace

save "C:\Users\Matheus\Desktop\stata\baseantigapolicompleta2.dta", replace















