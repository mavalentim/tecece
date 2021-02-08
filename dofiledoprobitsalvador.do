* FAZENDO O PROBIT AQUI TAMBEM PARA TER CERTEZA


*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


*SAUDE - PRECISOU DE LEVES ALTERAÇOES - VEM DO SCRIPT PROPENSITY SCORE R
*BASE POR MUNICIPIO COM MORTES DE ANOS RECENTES
use "C:\Users\Matheus\Desktop\stata\prop\obito.dta", clear

tostring ano, replace

*tirando todos os missings
replace mort_totais = 0 if mort_totais ==.
replace suicidio = 0 if suicidio ==.
replace agressao = 0 if agressao ==.
replace resp = 0 if resp ==.
replace saneamento = 0 if saneamento ==.
replace prev = 0 if prev ==.
replace prevum = 0 if prevum ==.
replace prevdois = 0 if prevdois ==.
replace prevtres = 0 if prevtres ==.
replace infantil = 0 if infantil ==.

*anos anteriores a eleicao e mantendo mort por qualquer aspecto
keep if ano =="2016" | ano =="2015"| ano =="2014"
keep ibge ano mort_totais
destring ano, replace

reshape wide mort_totais, i(ibge) j(ano)

foreach i in 2014 2015 2016{
replace mort_totais`i' = 0 if mort_totais`i' ==.
}

save "C:\Users\Matheus\Desktop\stata\logit\baseMAElogit1.dta", replace

*MERGINDO COM BASE DOS DSEIS PARA TERMOS CARACTERISTICAS DOS DISTRITOS E POPULACAO 

*identificando qual municipio fica em qual DSEI
merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\prop\basemunicipiopordsei.dta",generate(_mergedsei)

keep if _merge==3

save "C:\Users\Matheus\Desktop\stata\logit\baseMAElogit2.dta", replace


*indo em direcao a mergir a populacao residente naquele dsei para taxa de mortalidade do dsei e part nas morts

*populacao de cada dsei largo
import excel "C:\Users\Matheus\Desktop\pedidosai\respostapedidopopulacao\Recurso Demografico 2010-2019.xlsx", sheet("DEMOGRAFICO") firstrow clear

*pegando 2016 e 2019
keep H G F DSEI
rename H popdsei2016
rename G popdsei2015
rename F popdsei2014
drop if DSEI =="Total Geral"

tostring DSEI , replace
save "C:\Users\Matheus\Desktop\stata\logit\popdseilargo.dta", replace

*mortes totais por dsei largo


u"C:\Users\Matheus\Desktop\stata\logit\baseMAElogit2.dta", clear

collapse (sum) mort_totais2014 mort_totais2015 mort_totais2016, by (DSEI)

foreach i in 2014 2015 2016{
rename mort_totais`i' mort_totaisDSEI`i'
}
merge 1:m DSEI using "C:\Users\Matheus\Desktop\stata\logit\baseMAElogit2.dta", generate(_merge4)


foreach i in 2014 2015 2016{
gen partmort`i' = mort_totais`i'/mort_totaisDSEI`i'
}

merge m:1 DSEI using "C:\Users\Matheus\Desktop\stata\logit\popdseilargo.dta", generate(_merge5)


foreach i in 2014 2015 2016{
gen taxamortdsei`i' = mort_totaisDSEI`i'/popdsei`i'
}
drop if _merge5 ==2
drop  _merge4 _merge5 mort_totais2014 mort_totais2015  mort_totais2016
save "C:\Users\Matheus\Desktop\stata\logit\baseMAElogit3.dta", replace








*BASE DA POLITICA - PEGANDO JA COLAPSADO POR MUNICIPIO

u "C:\Users\Matheus\Desktop\stata\prop\politicacompleta.dta", clear


*colocando agora as caracteristicas que consideramos relevantes para explicar eleicao de indigenas

*PIB
merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\prop\basepibunica.dta", gen(_mergepib)
keep if _mergepib ==3 
drop _mergepib


*SANEAMENTO BASICO

merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\esgoto\esgoto.dta", gen(mergeesgoto)
keep if mergeesgoto==3
drop mergeesgoto


*NIVEL DE DESMATAMENTO

merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\desmata\desmata.dta", gen(mergefloresta)
keep if mergefloresta==3
drop mergefloresta


*SAUDE 

merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\logit\baseMAElogit3.dta", gen(_mergesaude)
















foreach i in 2014 2015 2016{

replace partmort`i' = 0 if partmort`i'==.
replace taxamortdsei`i'=0 if taxamortdsei`i'==.
replace popdsei`i' = 0 if popdsei`i' ==.
}


gen regiao = substr(ibge,1,1)
gen norte =1 if regiao =="1"
replace norte =0 if norte==.
gen nordeste =1 if regiao == "2"
replace nordeste =0 if nordeste==.
gen sudeste = 1 if regiao == "3"
replace sudeste =0 if sudeste ==.
gen co = 1 if regiao =="5"
replace co =0 if co==.
gen sul = 1 if regiao =="4"
replace sul =0 if sul ==.

gen taxamortdseirecente = (taxamortdsei2016 + taxamortdsei2015 + taxamortdsei2014)/3
gen partnasmortrecente = (partmort2016 + partmort2015 + partmort2014)/3
gen ano = "2016"
save "C:\Users\Matheus\Desktop\stata\logit\logitfinal.dta", replace



u"C:\Users\Matheus\Desktop\stata\logit\logitfinal.dta", clear


*usando taxas recentes

*usando taxa distrital
probit muntevecand taxamortdseirecente popdsei2016 anosagro pibpcap media_aprovacao_esq desmata semesgoto norte nordeste co sudeste
estimates table, star(.05 .01 .001)

probit munelegeucand taxamortdseirecente popdsei2016 anosagro pibpcap media_aprovacao_esq desmata semesgoto norte nordeste co sudeste
estimates table, star(.05 .01 .001)

*usando taxa municipal
probit muntevecand partnasmortrecente popdsei2016 anosagro pibpcap media_aprovacao_esq desmata semesgoto norte nordeste co sudeste
estimates table, star(.05 .01 .001)

probit munelegeucand partnasmortrecente popdsei2016 anosagro pibpcap media_aprovacao_esq desmata semesgoto norte nordeste co sudeste
estimates table, star(.05 .01 .001)






u "C:\Users\Matheus\Desktop\stata\logit\logitfinal.dta", clear
*somente municipios de dseis
keep if _mergesaude ==3

gen desmatanorte = desmata*norte

probit muntevecand taxamortdseirecente popdsei2016 anosagro pibpcap media_aprovacao_esq desmata semesgoto norte nordeste co sudeste
estimates table, star(.05 .01 .001)

probit munelegeucand taxamortdseirecente popdsei2016 anosagro pibpcap media_aprovacao_esq desmata semesgoto norte nordeste co sudeste
estimates table, star(.05 .01 .001)



probit muntevecand partnasmortrecente popdsei2016  anosagro pibpcap media_aprovacao_esq desmata semesgoto norte nordeste co sudeste
estimates table, star(.05 .01 .001)

probit munelegeucand partnasmortrecente popdsei2016 anosagro pibpcap media_aprovacao_esq desmata semesgoto norte nordeste co sudeste
estimates table, star(.05 .01 .001)


******** outras estimativas
logit muntevecand partmort2016 popdsei2016 anosagro pibpcap media_aprovacao_esq 
estimates table, star(.05 .01 .001)

logit munelegeucand partnasmortrecente popdsei2016 anosagro pibpcap media_aprovacao_esq norte nordeste co sudeste
estimates table, star(.05 .01 .001)








