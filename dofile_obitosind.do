**************************** FAZENDO UM DO-FILE PARA LIDAR COM AS ADVERSIDADES ****************************

*BASE MAE

*importando a base de óbitos editada no R
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
replace menosde5=0 if menosde5==.
replace parto =0 if parto==.
replace acomat =0 if acomat==.
replace naoespec = 0 if naoespec ==.
*antes e depois do tratamento
keep if ano =="2016" | ano =="2019"
*dropando quem nao tiver antes e depois? pode dropar missing
*ou transformar em wide e dropar que nao tiver os dois anos

save "C:\Users\Matheus\Desktop\stata\prop\baseMAE.dta", replace


*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
*BASE DE MORTES TOTAIS POR DISTRITO SANITARIO

*importando base onde demonstra-se onde fica cada municipio
import excel "C:\Users\Matheus\Desktop\tcc\excel\Recurso Demografico 2010-2019.xlsx", sheet("MUNICIPIO") firstrow allstring clear

gen teste=1
collapse (sum) teste, by(CO_MUNICIPIO_IBGE)
drop if teste >1
drop teste
save "C:\Users\Matheus\Desktop\stata\prop\basemunicipiounicodsei.dta", replace

*filtrar essa abaixo com a de cima para obter municipios que estao em um so DSEI
import excel "C:\Users\Matheus\Desktop\tcc\excel\Recurso Demografico 2010-2019.xlsx", sheet("MUNICIPIO") firstrow allstring clear
merge m:1 CO_MUNICIPIO_IBGE using "C:\Users\Matheus\Desktop\stata\prop\basemunicipiounicodsei.dta", generate(_mergeentrpol)
drop if _mergeentrpol!=3

*renomeando
rename DSEI_GESTAO DSEI
rename CO_MUNICIPIO_IBGE ibge
drop NO_MUNICIPIO _mergeentrpol
save "C:\Users\Matheus\Desktop\stata\prop\basemunicipiopordsei.dta", replace

merge 1:m ibge using "C:\Users\Matheus\Desktop\stata\prop\baseMAE.dta", generate(_mergeentrpol)

drop if _mergeentrpol != 3

save "C:\Users\Matheus\Desktop\stata\prop\baseMAE2.dta", replace

*criando mortes totais por dsei e mergindo na baseMAE2

collapse (sum) mort_totais naoespec suicidio agressao resp saneamento prev infantil prevum prevdois prevtres parto menosde5 acomat, by(DSEI ano)
rename * *dsei
rename DSEIdsei DSEI
rename anodsei ano
merge 1:m DSEI ano using "C:\Users\Matheus\Desktop\stata\prop\baseMAE2.dta", generate(_mergemortdsei)

drop  _mergemortdsei _mergeentrpol

foreach i in mort_totais naoespec suicidio agressao resp saneamento prev prevum prevdois prevtres infantil parto menosde5 acomat{
gen part`i' = (`i'/`i'dsei)*100

}
save "C:\Users\Matheus\Desktop\stata\prop\baseMAE3.dta", replace

*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*EQUILIBRANDO O XTSET

**aproveitando para balancear o painel
gen tem16 = 1 if ano == "2016"
gen tem19 = 1 if ano == "2019"
replace tem16=0 if tem16==.
replace tem19=0 if tem19 ==.

collapse (sum) tem16 (sum)tem19, by(ibge)
keep if tem16 ==1 & tem19 ==1

merge 1:m ibge using "C:\Users\Matheus\Desktop\stata\prop\baseMAE3.dta", generate(_mergextset)
drop if _mergextset != 3
drop tem16
drop tem19
drop _mergextset

*salvando a base painel ja equilibrada
save "C:\Users\Matheus\Desktop\stata\prop\baseMAE4.dta", replace




*MERGES

*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
**BASE POPULACAO DO DSEI
import excel "C:\Users\Matheus\Desktop\tcc\excel\Recurso Demografico 2010-2019.xlsx", sheet("DEMOGRAFICO") firstrow clear

*pegando 2016 e 2019
keep H K DSEI
rename H pop2016
rename K pop2019
drop if DSEI =="Total Geral"

reshape long pop, i(DSEI) j(ano)
rename pop popdsei
tostring DSEI ano, replace
save "C:\Users\Matheus\Desktop\stata\prop\popdsei.dta", replace

*merge com a base mae

*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
*BASE POLO BASE POR DSEI
import excel "C:\Users\Matheus\Desktop\tcc\excel\saudeinf\saudeinfra.xlsx", sheet("polos base") firstrow clear

*pegando 2016 e 2019
keep H K DSEI
rename H polobase2016
rename K polobase2019
drop if DSEI =="Total Geral"

reshape long polobase, i(DSEI) j(ano)
tostring DSEI ano, replace

save "C:\Users\Matheus\Desktop\stata\prop\polobasedsei.dta", replace


*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
**BASE PIB
use "C:\Users\Matheus\Desktop\PIB.dta", clear

*ajustando a base
keep ibgecode2 year vabagropecuária impostos população pibpreçoscorrentes
rename ibgecode2 ibge
rename year ano
rename vabagropecuária agro
rename pibpreçoscorrentes pib
rename população popmun
destring agro impostos pib popmun, force replace

*criando variaveis de interesse
gen anosagro = 1 if agro/pib>0.5
replace anosagro = 0 if anosagro ==.
gen pibpcap = pib/popmun

*duplicates indo pro espaço - todos eram sem valor no ibge
duplicates tag ibge ano, gen(tag)
drop if tag>0
drop tag
save "C:\Users\Matheus\Desktop\stata\prop\basepiblong.dta", replace

*colapsar para ter anos agricolas
collapse (sum)anosagro, by (ibge)
save "C:\Users\Matheus\Desktop\stata\prop\basepibanosagro.dta", replace

*obtendo pibpercapita so de 2015
u "C:\Users\Matheus\Desktop\stata\prop\basepiblong.dta"
drop if ano != "2015"
keep ibge pibpcap
save "C:\Users\Matheus\Desktop\stata\prop\basepibpibpcapita.dta", replace


*mergindo tudo em base do pib unica
u "C:\Users\Matheus\Desktop\stata\prop\basepiblong.dta", clear
drop if ano != "2015"
keep ibge
merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\prop\basepibanosagro.dta", generate(_mergepib1)
drop _mergepib
merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\prop\basepibpibpcapita.dta", generate(_mergepib1)
drop _mergepib

save "C:\Users\Matheus\Desktop\stata\prop\basepibunica.dta", replace



*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
*BASE DAS TERRAS INDIGENAS - PEGANDO JA COLAPSADO POR MUNICIPIO
*abrindo a base
*use "C:\Users\Matheus\Desktop\munti.dta", clear
*tirando duplicates
*duplicates tag ibge, gen(tag)
*duplicates drop ibge tag, force
*save "C:\Users\Matheus\Desktop\stata\prop\munti.dta", replace


*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
*BASE DA POLITICA - PEGANDO JA COLAPSADO POR MUNICIPIO

*abrindo a base
use "C:\Users\Matheus\Desktop\stata\vieramdor\politica.dta", clear

*mantendo o necessario 
keep ibge muntevecand munelegeucand prefeito_ind_el prop_ind_eleito controlecamera prop_vereador_ind
 
*merge com a base historica
merge 1:1 ibge using "C:\Users\Matheus\Desktop\stata\baseantigapolicompleta2.dta", generate(_mergeentrpol)
drop if _mergeentrpol !=3
keep ibge muntevecand munelegeucand media_aprovacao_esq prefeito_ind_el controlecamera prop_vereador_ind

*salvando
save "C:\Users\Matheus\Desktop\stata\prop\politicacompleta.dta", replace













*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

u "C:\Users\Matheus\Desktop\stata\prop\baseMAE4.dta", clear

*merge com popdsei
merge m:1 DSEI ano using "C:\Users\Matheus\Desktop\stata\prop\popdsei.dta", generate(_mergepopdsei)
drop if _mergepopdsei !=3
drop _mergepopdsei
*merge com polodsei
merge m:1 DSEI ano using "C:\Users\Matheus\Desktop\stata\prop\polobasedsei.dta", generate(_mergepolodsei)
drop if _mergepolodsei !=3
drop _mergepolodsei

*merge com pib 
merge m:1 ibge using "C:\Users\Matheus\Desktop\stata\prop\basepibunica.dta", generate(_mergepibind)
keep if _mergepibind==3
drop _mergepibind 

*merge com terras indigenas
*merge m:1 ibge using "C:\Users\Matheus\Desktop\stata\prop\munti.dta", generate(_mergeti)
*keep if _mergeti == 3
*esse keep pode ser alterado caso nao se use essas caracteristicas
*drop _mergeti tag

*merge com desmatamento

merge m:1 ibge using "C:\Users\Matheus\Desktop\stata\desmata\desmata.dta", generate(_mergeflorest)
keep if _mergeflorest==3

*merge com condicoes sanitarias 

merge m:1 ibge using "C:\Users\Matheus\Desktop\stata\esgoto\esgoto.dta", generate(_mergesgoto)
keep if _mergesgoto==3






*merge com politica
merge m:1 ibge using "C:\Users\Matheus\Desktop\stata\prop\politicacompleta.dta", generate(_mergepol)
keep if _mergepol == 3
destring ano ibge, replace

*replace munelegeucand = 0 if ano == 2016
gen t = prefeito_ind_el 
replace t=0 if ano == 2016



*gerando identificacoes de mortalidades rapidamente reversiveis - taxas de mortalidade distritais de coisas rapidamente reversíveis
gen taxadseimort = (mort_totaisdsei/popdsei)*1000
gen taxadseinaoespec = (naoespecdsei/popdsei)*1000
gen taxadseiinfantil= (infantildsei/ popdsei)*1000
gen taxadseiprev= (prevdsei/popdsei)*1000
gen taxadseiagressao= (agressaodsei/popdsei)*1000
gen taxadseiprev3 = (prevtresdsei/popdsei)*1000
gen taxadseiprev2 = (prevdoisdsei/popdsei)*1000



save "C:\Users\Matheus\Desktop\stata\prop\basefinal.dta", replace

 *TESTES - PARTE 1: DIFERENCAS EM DIFERENCAS
 
 u"C:\Users\Matheus\Desktop\stata\prop\basefinal.dta", clear
*fazendo diferencas em diferencas com dois periodos - nao significativo
xtset ibge ano
xtreg partmort_totais t i.ano, fe rob 
*SIGNIFICATIVO
estimates table, star(.05 .01 .001)


xtreg partprevtres t i.ano , fe rob 
*SIGNIFICATIVO
estimates table, star(.05 .01 .001)



xtreg taxadseiprev2 t i.ano , fe rob 
*SIGNIFICATIVO
estimates table, star(.05 .01 .001)




*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
*fazendo o mesmo mas com DSEI
collapse (sum) mort_totais prev t, by(DSEI ano)
destring DSEI, replace
xtset DSEI ano







*gerar mortalidade dos dsei e talvez testar até para nível agregado dos dsei, depois ainda testar o psm
