
# carregando bibliotecas---------------
library(readxl)
library(lubridate)
library(stringr)
library(plm)
library(haven)
library(reshape2)
library(geobr)
library(xlsx)
library(readr)
library(tidyverse)


# Complementando as bases ----------------

obitoind0 <- read_excel("obitoind.xlsx")%>%
  set_names('dsei', 'polobase', 'municipio' , 'ano' , 'cid' , 'causa')%>%
  filter(ano  == '2017')

obitoind1 <- read_excel("obitodadecada.xlsx", 
                        sheet = "Plan1", col_types = c("skip", "text", "numeric", "text", "text", 
                                                       "text", "text", "text", "numeric", "date", 
                                                       "text", "text", "skip"))%>%
             set_names('dsei','npolobase','polobase','ibge', 'municipio',
                       'uf','sexo','idade', 'data' , 'cid' , 'causa')%>%
             filter(!data == is.na(data))

obitoind <- obitoind1%>%
 mutate(ano = as.numeric(format(data,'%Y')))%>%
 select(dsei, polobase, municipio , ano , cid , causa)%>%
 filter(ano<2020)%>%
 add_row(obitoind0)

# MODIFICANDO AS BASES E ADICIONANDO CODIGO IBGE -------------------

codigos_IBGE <- read_excel("Codigos IBGE.xlsx")

renomeadora <- function(x){
  x%>%
    mutate(Municipio = str_to_upper(Municipio),
           Municipio = str_replace_all(Municipio, "Â", "A" ),
           Municipio = str_replace_all(Municipio, "Ê", "E" ),
           Municipio = str_replace_all(Municipio, "Î", "I" ),
           Municipio = str_replace_all(Municipio, "Ô", "O" ),
           Municipio = str_replace_all(Municipio, "Û", "U" ),
           Municipio = str_replace_all(Municipio, "Á", "A" ),
           Municipio = str_replace_all(Municipio, "É", "E" ),
           Municipio = str_replace_all(Municipio, "Í", "I" ),
           Municipio = str_replace_all(Municipio, "Ó", "O" ),
           Municipio = str_replace_all(Municipio, "Ú", "U" ),
           Municipio = str_replace_all(Municipio, "Ã", "A" ),
           Municipio = str_replace_all(Municipio, "Õ", "O" ),
           Municipio = str_replace_all(Municipio, "Ç", "C" ))
}

codigos_IBGE_renomeado<- renomeadora(codigos_IBGE)

obitoind_renomeado <- obitoind%>%
  rename(Municipio = municipio)%>%
  renomeadora()


obitoindcomibge <- obitoind_renomeado%>%
  left_join(codigos_IBGE_renomeado, by = 'Municipio')%>%
  rename(id_municipio = Codigo1, id_municipio_6 = Codigo2)





# IDENTIFICACAO DE CIDS ----------------------------

obitoind_identificado <-obitoindcomibge%>%
  mutate(cid3 = str_sub(cid, end= 3),
         cid2 = str_sub(cid, end= 2),
         cap = str_sub(cid, end= 1),
         morte = 1,
         resp = case_when(cap == 'J' ~ 1,
                          cap != 'J' ~ 0),
  #definindo suicidio
         suicid = case_when(cid2=="X6" | cid2=="X7" |  cid3 == "X80" |
                            cid3 == "X81" | cid3 == "X82"|cid3 == "X83"|cid3 == "X84" ~ 1,
                            cid2 !="X6" | cid2!="X7" | cid2 != "X8" ~ 0),
  #definindo morte por conflito
         agressao = case_when(cid3 == "X85" |cid3 == "X86" |cid3 == "X87"|
                              cid3 == "X88" |cid3 == "X89"|
                              cid2 == "X9" | cid2 == "Y0" ~ 1,
                              cid3 != "X85" |cid3 != "X86" |cid3 != "X87"|
                              cid3 != "X88" |cid3 != "X89"|
                              cid2 != "X9" | cid2 != "Y0" ~0),
  #definindo mortes por ruins condicoes sanitarias
         saneamento = case_when(cid2 == 'A0'~ 1, cid2!= 'A0' ~ 0),
  #prev1 = reduziveis por imunoprevencao
         prev1 = case_when(cid3 == 'A15' | cid3 == 'A16'|cid3 == 'A17'|
                             cid3 == 'A34'|cid3 == 'A35'|cid3 == 'A36'|
                             cid3 == 'A37'| cid3 == 'A80'| cid3 == 'B05'|
                             cid3 == 'B06'| cid3 == 'B16'| cid == 'G00.0'~1),
         #definindo prev2 - previniveis transmissiveis por atuacao do sistema de saude       
         prev2 = case_when(cid3 == 'A00'| cid3 == 'A01'| cid3 == 'A02'|
                             cid3 == 'A03'| cid3 == 'A04'| cid3 == 'A05'|
                             cid3 == 'A06'| cid3 == 'A07'| cid3 == 'A08'|
                             cid3 == 'A09'| cid3 == 'B20'| cid3 == 'B21'|
                             cid3 == 'B22'| cid3 == 'B23'| cid3 == 'B24'|cid3 == 'B15'| 
                             cid3 == 'B17'| cid3 == 'B18'| cid3 == 'B19'|cid3 == 'A50' |
                             cid3 == 'A51' |cid3 == 'A52'|
                             cid3 == 'A53' |cid3 == 'A54' |cid3 == 'A55'| cid3 == 'A56'|
                             cid3 == 'A57' | cid3 == 'A58'|
                             cid3 == 'A59'|cid3 == 'A63' |cid3 == 'A64' | cid3 == 'N70'|
                             cid3 == 'N71'| cid3 == 'N72'|cid == 'N73.1'| cid3 == 'N73.2'|
                             cid3 == 'N73.3'| cid3 == 'N73.4'| cid == 'N73.5' | cid == 'N73.8'|
                             cid == 'N73.9'| cid3 == 'N75'| cid3 == 'N76'|
                             cid3 == 'A23'| cid3 == 'A24' | cid3 == 'A25'| 
                             cid3 == 'A26'|cid3 == 'A28'| cid3 == 'A29'| cid3 == 'A30'| cid3 == 'A31'| cid3 == 'A32'| 
                             cid3 == 'A39'|cid3 == 'A40'|cid3 == 'A41'| 
                             cid3 == 'A46'|cid == 'A69.2'|cid == 'J02.0'|
                             cid == 'J03.0'|cid3 == 'B50'|cid3 == 'B51'|
                             cid3 == 'B52'|cid3 == 'B53'|cid3 == 'B54'|
                             cid == 'G00.1'|cid == ' G00.2'|cid == ' G00.3'|
                             cid == ' G00.4'|cid == ' G00.5'|cid == ' G00.6'|
                             cid == ' G00.7'|cid == ' G00.8'|cid == ' G00.9'|
                             cid3 == ' G01'|cid3 == 'I00'|cid3 == 'I01'| cid3 == 'I02'|
                             cid3 == 'I03'|cid3 == 'I04'|cid3 == 'I05'|cid3 == 'I06'|
                             cid3 == 'I07'|cid3 == 'I08'|cid3 == 'I09'|cid3 == 'J00'|
                             cid3 == 'J01'|cid == 'J02.8'|cid == 'J02.9'|cid == 'J03.8'|
                             cid == 'J03.9'|cid3 == 'J04'|cid3 == 'J05'|cid == 'J06.0'|
                             cid3 == 'J10'|cid3 == 'J11'|cid3 == 'J12'|cid3 == 'J13'|
                             cid3 == 'J14'|cid3 == 'J15'|cid3 == 'J16'|cid3 == 'J17'|
                             cid3 == 'J18'|cid3 == 'J19'|cid3 == 'J20'|cid3 == 'J21'|
                             cid3 == 'J22'|cid3 == 'L02'|cid3 == 'L03'|cid3 == 'L04'|
                             cid3 == 'L05'|cid3 == 'L06'|cid3 == 'L07'|cid3 == 'L08'|
                             cid3 == 'A20'|cid3 == 'A21'|cid3 == 'A22'|cid3 == 'A27'|
                             cid3 == 'A30'| cid3 == 'A77'|cid3 == 'A82'|cid3 == 'A90'|
                             cid == 'A92.3'|cid3 == 'A95'|cid == 'A98.5'|cid3 == 'B03'|
                             cid3 == 'B55'|cid == 'B57.0'|cid == 'B57.1'|cid3 == 'B65'|
                             cid == 'N39.0'~1),
         #prev3 - previniveis nao-transmissiveis por atuacao do sistema de saude
         prev3 = case_when(cid == 'B57.2'| cid3 == 'C00'| cid3 == 'C43'| cid3 == 'C44'|
                             cid3 == 'C22'| cid3 == 'C16'| cid3 == 'C18'| cid3 == 'C19'|
                             cid3 == 'C20'|cid3 == 'C21'|cid3 == 'C01'|cid3 == 'C02'|
                             cid3 == 'C03'|cid3 == 'C04'|cid3 == 'C05'|cid3 == 'C06'|
                             cid3 == 'C09'|cid3 == 'C10'|cid3 == 'C12'|cid3 == 'C13'|
                             cid3 == 'C14'|cid3 == 'C32'|cid3 == 'C15'|cid3 == 'C33'|
                             cid3 == 'C34'|cid3 == 'C50'|cid3 == 'C53'|cid3 == 'C54'|
                             cid3 == 'C55'|cid3 == 'C62'|cid3 == 'C73'|cid3 == 'C81'|
                             cid3 == 'C91'|cid3 == 'E01'|cid3 == 'E02'|cid3 == 'E03'|
                             cid3 == 'E04'|cid3 == 'E05'|cid3 == 'E00'|cid3 == 'E25.0'|
                             cid3 == 'E70.0'|cid3 == 'E74.2'|cid3 == 'E10'|cid3 == 'E11'|
                             cid3 == 'E12'|cid3 == 'E13'|cid3 == 'E14'|cid3 == 'E40'|
                             cid3 == 'E41'|cid3 == 'E42'|cid3 == 'E43'|cid3 == 'E44'|
                             cid3 == 'E45'|cid3 == 'E46'|cid3 == 'E50'|cid3 == 'E51'|
                             cid3 == 'E52'|cid3 == 'E53'|cid3 == 'E54'|cid3 == 'E55'|
                             cid3 == 'E56'|cid3 == 'E57'|cid3 == 'E58'|cid3 == 'E59'|
                             cid3 == 'E60'|cid3 == 'E61'|cid3 == 'E62'|cid3 == 'E63'|
                             cid3 == 'E64'|cid3 == 'D50'|cid3 == 'D51'|cid3 == 'D52'|
                             cid3 == 'D53'|cid3 == 'E86'|cid3 == 'F10'|cid == 'I42.6'|
                             cid == 'K29.2'|cid3 == 'K70'|cid3 == 'I85'|cid3 == 'G40'|
                             cid3 == 'G41'|cid3 == 'I10'|cid3 == 'I11'|cid3 == 'I12'|
                             cid3 == 'I13'|cid3 == 'I20'|cid3 == 'I21'|cid3 == 'I22'|
                             cid3 == 'I23'|cid3 == 'I24'|cid3 == 'I25'|cid3 == 'I70'|
                             cid3 == 'I50'|cid3 == 'I61'|cid == 'I63.0'|cid == 'I63.5'|
                             cid3 == 'I63.8'|cid3 == 'I63.9'|cid3 == 'I64'|cid3 == 'I65'|
                             cid3 == 'I66'|cid3 == 'J40'|cid3 == 'J41'|cid3 == 'J42'|
                             cid3 == 'J43'|cid3 == 'J45'|cid3 == 'J46'|cid3 == 'K25'|
                             cid3 == 'K28'|cid3 == 'K35'|cid3 == 'J60'|cid3 == 'J61'|
                             cid3 == 'J62'|cid3 == 'J63'|cid3 == 'J64'|cid3 == 'J65'|
                             cid3 == 'J66'|cid3 == 'J67'|cid3 == 'J68'|cid3 == 'J69'|
                             cid3 == 'J70'|cid3 == 'K40'|cid3 == 'K41'|cid3 == 'K42'|
                             cid3 == 'K43'|cid3 == 'K44'|cid3 == 'K45'|cid3 == 'K46'|
                             cid3 == 'K56'| cid3 == 'K80'|cid3 == 'K81'|cid3 == 'K82'|
                             cid3 == 'K83'|cid3 == 'N18'|cid3 == 'O00'|cid3 == 'O01'|
                             cid3 == 'O02'|cid3 == 'O03'|cid3 == 'O04'|cid3 == 'O05'|
                             cid3 == 'O06'|cid3 == 'O07'|cid3 == 'O08'|cid3 == 'O09'|
                             cid3 == 'O10'|cid3 == 'O11'|cid3 == 'O12'|cid3 == 'O13'|
                             cid3 == 'O14'|cid3 == 'O15'|cid3 == 'O16'|cid3 == 'O17'|
                             cid3 == 'O18'|cid3 == 'O19'|cid3 == 'O20'|cid3 == 'O21'|
                             cid3 == 'O22'|cid3 == 'O23'|cid3 == 'O24'|cid3 == 'O25'|
                             cid3 == 'O26'|cid2 == 'O3'|cid2 == 'O4'|cid2 == 'O5'|
                             cid2 == 'O6'|cid2 == 'O7'|cid2 == 'O8'|cid2 == 'O9'|
                             cid2 == 'V0'|cid2 == 'V1'|cid2 == 'V2'| cid2 == 'V3'|
                             cid2 == 'V4'|cid2 == 'V5'|cid2 == 'V6'|cid2== 'V7'|
                             cid2 == 'V8'|cid2 == 'V9'|cid3== 'W65'|cid3 == 'W66'|
                             cid3 == 'W67'|cid3 == 'W68'|cid3 == 'W69'|cid3 == 'W70'|
                             cid3 == 'W71'|cid3 == 'W72'|cid3 == 'W73'|cid3 == 'W74'|
                             cid2 == 'X0'| cid2 == 'X4' | cid2 == 'X6' |cid2 == 'X7'| 
                             cid3 == "X80" |cid3 == "X81" | cid3 == "X82"|cid3 == "X83" |cid3 == "X84"|
                             cid3 == "X85" |cid3 == "X86" |cid3 == "X87" |cid3 == "X88" |cid3 == "X89"|
                             cid2 == "X9" | cid2 == "Y0"|cid2 == "Y1" | cid2 == "Y2" |cid3 == "Y31" |cid3 == "Y32" |
                             cid3 == "Y33" |cid3 == "Y34" | cid2 == "W0" | cid2 == "W1" | cid2 == "Y6" |cid3 == "Y83" |
                             cid3 == "Y84" ~ 1),
         cardiaco = case_when(cap == 'I'~1),
         naoespec = case_when(cid3 == 'R99' | cid3 == 'R98' ~1))%>%
 ungroup() %>%
  replace_na(list(morte=0, suicid=0, agressao=0, 
                  resp=0, saneamento=0, prev=0,
                  prev1=0, prev2=0, prev3=0, cardiaco =0, naoespec =0))%>%
mutate(prev = prev1+prev2+prev3 )
  

#COLAPSANDO POR MUNICIPIO ----------------------

obitoind_agregada <- obitoind_identificado%>%
  #aqui pus para agrupar as mortes por municipio
  group_by(id_municipio_6,ano)%>%
  summarize(mortes_totais = sum(morte), 
            suicidio = sum(suicid),
            agressao = sum(agressao), 
            resp = sum(resp), 
            saneamento = sum(saneamento),
            prev = sum(prev), 
            prevum = sum(prev1), 
            prevdois = sum(prev2), 
            prevtres = sum(prev3),
            cardiaco = sum(cardiaco), 
            naoespec = sum(naoespec))%>%
  mutate(regiao = substr(id_municipio_6, 1,1))%>%
  ungroup()


# Linkando municípios a DSEIs --------

# município a um DSEI
dsei_mun <- read_excel("Recurso Demografico 2010-2019.xlsx",
                sheet = "MUNICIPIO", col_types = c("text","text", "text"))%>%
            mutate(CO_MUNICIPIO_IBGE = as.numeric(CO_MUNICIPIO_IBGE))%>%
            transmute(id_municipio_6 = CO_MUNICIPIO_IBGE, dsei = DSEI_GESTAO)%>%
            distinct(id_municipio_6, dsei)

# definindo a abridora de recursos
ab_recurso <- function(nome, sheet, valor){
  read_excel(nome,sheet = sheet)%>%
  pivot_longer(cols = c("2010":"2019"),names_to = valor)%>%
    set_names("dsei", "ano", valor)%>%
    filter(! dsei == "Total Geral")
}

# aplicando a abridora de recursos para populacao, aldeias e polobase
pop<-ab_recurso("Recurso Demografico 2010-2019.xlsx","DEMOGRAFICO", "pop")
polobase<-ab_recurso("saudeinf/saudeinfra.xlsx","polos base", "polobase")
aldeias<-ab_recurso("saudeinf/saudeinfra.xlsx","aldeias", "aldeias")

pop%>%
  inner_join(polobase, by = c("dsei", "ano"))%>%
  inner_join(aldeias, by =c("dsei", "ano"))%>%
  inner_join(dsei_mun, by = "dsei")%>%
  mutate(ano = as.numeric(ano))%>%
  filter(!ano == 2020) -> dsei


obitoind_agregada_dsei <- obitoind_agregada%>%
  left_join(dsei, by = c('id_municipio_6', "ano"))










# PIB 

  mutate(pibpercapita = pibpreçoscorrentes/população)


 # INFRAESTRUTURA DO DSEI
saudeinfra <- read_excel("saudeinf/saudeinfra.xlsx",
                           sheet = "polos base")

etnias_dsei <- read_excel("saudeinf/saudeinfra.xlsx",
                              sheet = "etnia")

aldeias_dsei <- read_excel("saudeinf/saudeinfra.xlsx",
                            sheet = "aldeias")


  mutate( poloporaldeia = polobase/naldeias)



##############



# #  7) TERRAS INDÍGENAS
# #precisa de rodar a parte de terra indigena
# baseibgefinal <- baseibge5%>%
#   left_join(pop_por_dsei, by = c('dsei', 'ano'))%>%
#   #deixei sem ano na base da suposicao que as terras indigenas tem as suas condicoes representadas fixamente
#   left_join(munti_g, by = c('ibge'))%>%
#   filter(ano != 2020)

  
#adicionando informacoes de terras indigenas para 2 anos


#mergindo com a base poli16 que tem para cada municipio valores dos candidatos

#right join?
baseauxdsei <- baseibge2 %>%
  # left_join(poli16, by = 'ibge')%>%
  # mutate (muntevecand  = if_else (cand_ind>0 ,1,0))%>%
  # mutate (munelegeucand  = if_else (ind_eleito>0 ,1,0))
  group_by(dsei, ano)%>%
  summarize(m_totais_do_dsei = sum(mortes_totais),
         suicidio_total_dsei = sum(suicidio),
         agressao_total_dsei = sum(agressao),
         saneamento_total_dsei = sum(saneamento),
         resp_total_dsei = sum(resp),
         cardiaco_total_dsei = sum(cardiaco),
         naoespec_total_dsei = sum(naoespec),
         prev_total_dsei = sum(prev))%>%
  ungroup()


basecompleta16<- baseibge2%>%
  left_join(baseauxdsei, by = c('dsei', 'ano'))%>%
  mutate(tx_mortalidade_dsei = (m_totais_do_dsei/popind)*1000)%>%
  mutate(part_nas_mortes = (mortes_totais/m_totais_do_dsei)*100)%>%
  mutate(txmsuicidio = (suicidio_total_dsei/popind)*1000,
         txmsaneamento = (saneamento_total_dsei/popind)*1000,
         txmagressao = (agressao_total_dsei/popind)*1000,
         txmresp = (resp_total_dsei/popind)*1000,
         txmcard = (cardiaco_total_dsei/popind)*1000,
         txmnaoespec = (naoespec_total_dsei/popind)*1000,
         txmprev =(prev_total_dsei/popind)*1000 )









