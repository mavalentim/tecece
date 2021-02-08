* FAZENDO UM PROPENSITY SCORE WEIGHTING
 
**OBJETIVO DO DO-FILE 
*o do-file a seguir calcula pesos por propensity score weighting
*e depois roda uma regressão utilizando os pesos calculadas
*em suma, o intuito desse método é criar ponderações para observações que receberam e que
*que não receberam tratamento para auxiliar no viés de seleção

**IDENTIFICANDO AS VARIÁVEIS
*VARIÁVEIS DE IDENTIFICAÇÃO: ibge DSEI 
*SÃO COVARIADAS: ano pibpcap anosagro media_aprovacao_esq desmata semesgoto
*SÃO TRATAMENTOS: munelegeucand prefeito_ind_el controlecamera
*A VARIÁVEL DE INTERESSE: partinfantil 

**PARTES
*primeira parte ajusta a base deixando ela em formato wide
*segunda parte cria a variável de interesse - a variação da mortalidade
*terceira parte analisa a relação entre covariadas e tratamento escolhido antes do balanceamento
*quarta parte roda os modelos de probabilidade e cria as ponderações
*quinta parte analisa a relação entre covariadas e tratamento escolhido depois do balanceamento
*sexta parte roda a regressão com os pesos

**POSSIBILIDADE DE REUTILIZAÇÃO
*o processo é facilmente replicado, basta substituir as bases,
*substituir as variáveis de interesse, as variáveis de tratamento 
*e as covariadas nos locais indicados pela definição das partes
 

u"C:\Users\Matheus\Desktop\stata\prop\basefinal.dta", clear


keep ibge DSEI ano part* taxadsei* popdsei munelegeucand prefeito_ind_el  pibpcap anosagro media_aprovacao_esq controlecamera desmata semesgoto
rename munelegeucand t
gen dummy_2019 = 1 if ano ==2019
replace dummy_2019 = 0 if dummy_2019 == .

tostring ibge, replace
gen regiao = substr(ibge,1,1)

gen norte = 1 if regiao == "1" 
gen co = 1 if regiao == "5"
gen sul = 1 if regiao == "4"
gen sud = 1 if regiao == "3"
gen nordeste = 1 if regiao == "2"

global regiao norte co sud sul nordeste

foreach k of global regiao{
replace `k' = 0 if `k' ==.
}
destring ibge, replace

 global variaveis pibpcap popdsei media_aprovacao_esq desmata semesgoto anosagro norte sul sud nordeste co

 global variaveistodas partmort_totais partsuicidio partagressao partresp partsaneamento partprev partprevum partprevdois partprevtres partinfantil ///
 partparto partmenosde5 partacomat taxadseimort taxadseiinfantil taxadseiprev taxadseiagressao taxadseiprev3 taxadseiprev2 pibpcap popdsei ///
 media_aprovacao_esq desmata semesgoto anosagro norte sul sud nordeste co partnaoespec taxadseinaoespec

* PROPENSITY SCORE E PREDICT PARA GERAR AS SCORES

**3 PARTE
*analisando a relação entre covariadas e tratamento antes do balanceamento 
foreach v of global variaveis{
reg `v' t
}

**4 PARTE 
*fazendo as previsoes usando função logística - USA AS VARIAVEIS ORIGINAIS
 logit t pibpcap popdsei media_aprovacao_esq desmata semesgoto anosagro norte sul sud nordeste co 
 
 predict ps, pr
 
*fazendo os pesos a partir das previsões
 
 
 
 * GERANDO ATE - AVERAGE TREATMENT 
 * CRIANDO NOVAS VARIAVEIS COM O PST - TRANSFORMAR TODAS AS VARIAVEIS INDEPENDENTES
foreach v of global variaveistodas{ 
gen `v'ps=`v'/ps if t==1
replace `v'ps=`v'/(1-ps) if t==0
}

* GERANDO ATT - AVERAGE TREATMENT TREATED
* CRIANDO NOVAS VARIAVEIS COM O PST - TRANSFORMAR TODAS AS VARIAVEIS INDEPENDENTES


egen p=sum(ps) if t==1
replace p =57.529152  if p ==.
foreach v of global variaveistodas{

gen `v'pstt=(1-`v')*(1-p)*ps/p*(1-ps)*(1-p) if t==0
replace `v'pstt=`v'/(1-ps) if t==1


}

*SÓ CRIANDO AS VARIÁVEIS PARA DEPOIS TESTAR NOVAMENTE






*FAZENDO SUPORTE COMUM
*hist ps, by(t)
*hist ps , by(regiao)
sum ps if t==1, detail
return list
scalar min=r(min)
scalar p1=r(p1)
scalar p99t=r(p99)
scalar p99=r(p99)
scalar p5=r(p5)
scalar p95=r(p95)


sum ps if t==0, detail
scalar max=r(max)
scalar p99=r(p99)

*SUPORTE COMUM EM SI 
gen cs1=0
replace cs1=1 if ps>p1 & ps<p99

gen tentativa = 0
replace tentativa = 1 if ps>p5 & ps<p95

*hist ps if tentativa==1, by(t)

*MODIFICANDO O BANCO DE DADOS TIRANDO OS QUE NAO SAO ==1
*drop if cs1 != 1
*cai só 5 observações

*como as OBSERVAÇÕES FICARIA

tab t cs1
tab t tentativa
* SÓ PARA MOSTRAR QUE A DISTRIBUICAO E MAIS PROXIMA
*by t para mostrar que as distribuicoes sao mais iguais
*hist ps if cs1==1, by(t)


 
 
 *5 PARTE 
 *analisando a mesma relação entre covariadas e tratamento depois do balanceamento
 *agora nenhuma é significante
 
 *sem o suporte comum
 foreach v of global variaveis{
 reg `v' t ps 
 }
 destring ano ibge, replace
 
 
 
 *com o suporte comum
foreach v of global variaveis{
 reg `v' t ps if tentativa ==1
 }
 destring ano ibge, replace
 
*6 PARTE 
*regressao usando as ponderações 
* TEM QUE TIRAR O DE FORA DO SUPORTE COMUM
gen multiplicacao = t*dummy_2019

drop if tentativa ==0

xtset ibge ano 
* ta desbalanceado

*filtrando a base
merge m:1 ibge using "C:\Users\Matheus\Desktop\stata\filtro.dta"
drop if _merge != 3
xtset ibge ano 



xtreg partnaoespecps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps copstt
estimates table, star(.05 .01 .001)
*ATT
xtreg partnaoespecpstt t dummy_2019 multiplicacao pibpcappstt popdseipstt media_aprovacao_esqpstt desmatapstt ///
semesgotopstt anosagropstt nortepstt sulpstt nordestepstt copstt
estimates table, star(.05 .01 .001)










xtreg partprevpstt t dummy_2019 multiplicacao pibpcappstt popdseipstt media_aprovacao_esqpstt desmatapstt ///
semesgotopstt anosagropstt nortepstt sulpstt nordestepstt copstt
estimates table, star(.05 .01 .001)

xtreg partinfantilpstt t dummy_2019 multiplicacao pibpcappstt popdseipstt media_aprovacao_esqpstt desmatapstt ///
semesgotopstt anosagropstt nortepstt sulpstt nordestepstt copstt
estimates table, star(.05 .01 .001)

xtreg taxadseinaoespecpstt t dummy_2019 multiplicacao pibpcappstt popdseipstt media_aprovacao_esqpstt desmatapstt ///
semesgotopstt anosagropstt nortepstt sulpstt nordestepstt copstt
estimates table, star(.05 .01 .001)

xtreg partagressaopstt t dummy_2019 multiplicacao pibpcappstt popdseipstt media_aprovacao_esqpstt desmatapstt ///
semesgotopstt anosagropstt nortepstt sulpstt nordestepstt copstt
estimates table, star(.05 .01 .001)
* ATE

xtreg partprevps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)



xtreg partagressaops t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)

xtreg taxadseinaoespecps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)


* EFEITO EM TODOS - MEDE O QUANTO PODE TER EM QUALQUER LOCAL QUE RECEBA
*estao aumentando a morte de criancas significativamente
xtreg partinfantilps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)

xtreg partmenosde5ps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)

xtreg partsuicidiops t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)

xtreg partsaneamentops t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)

*significativo negativo
xtreg taxadseimortps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps sudps nordesteps cops
estimates table, star(.05 .01 .001)

*significativo neg
xtreg taxadseiinfantilps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)


xtreg taxadseiprevps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)

xtreg taxadseiagressaops t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)

xtreg taxadseiprev2ps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps sudps nordesteps cops
estimates table, star(.05 .01 .001)

*efeito MEDIO NOS TRATADOS

xtreg taxadseiinfantilps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)



************

xtreg taxadseinaoespecpstt t dummy_2019 multiplicacao pibpcappstt popdseipstt media_aprovacao_esqpstt desmatapstt ///
semesgotopstt anosagropstt nortepstt sulpstt nordestepstt copstt
estimates table, star(.05 .01 .001)

xtreg taxadseinaoespecps t dummy_2019 multiplicacao pibpcapps popdseips media_aprovacao_esqps desmataps ///
semesgotops anosagrops norteps sulps nordesteps cops
estimates table, star(.05 .01 .001)




 
