library(foreign)
library(tidyverse)

ingresos<-read.dbf("ingresos.dbf")

ingreso<-ingresos%>%group_by(folioviv,foliohog,clave)%>%summarise(suma=sum(ing_tri))

ingreso<-ingreso%>%
  mutate(P001=ifelse(clave=="P001",suma,0),
         P002=ifelse(clave=="P002",suma,0),
         P003=ifelse(clave=="P003",suma,0), 
         P004=ifelse(clave=="P004",suma,0),
         P005=ifelse(clave=="P005",suma,0),
         P006=ifelse(clave=="P006",suma,0),
         P007=ifelse(clave=="P007",suma,0),
         P008=ifelse(clave=="P008",suma,0),
         P009=ifelse(clave=="P009",suma,0),
         P011=ifelse(clave=="P011",suma,0),
         P012=ifelse(clave=="P012",suma,0),
         P013=ifelse(clave=="P013",suma,0),
         P014=ifelse(clave=="P014",suma,0),
         P015=ifelse(clave=="P015",suma,0),
         P016=ifelse(clave=="P016",suma,0),
         P018=ifelse(clave=="P018",suma,0),
         P019=ifelse(clave=="P019",suma,0),        
         P021=ifelse(clave=="P021",suma,0),
         P022=ifelse(clave=="P022",suma,0),
         P023=ifelse(clave=="P023",suma,0),
         P024=ifelse(clave=="P024",suma,0),
         P025=ifelse(clave=="P025",suma,0),
         P026=ifelse(clave=="P026",suma,0),
         P027=ifelse(clave=="P027",suma,0),
         P028=ifelse(clave=="P028",suma,0),
         P029=ifelse(clave=="P029",suma,0),
         P030=ifelse(clave=="P030",suma,0),
         P031=ifelse(clave=="P031",suma,0),
         P032=ifelse(clave=="P032",suma,0),
         P033=ifelse(clave=="P033",suma,0),
         P034=ifelse(clave=="P034",suma,0),
         P035=ifelse(clave=="P035",suma,0),
         P036=ifelse(clave=="P036",suma,0),
         P037=ifelse(clave=="P037",suma,0),
         P038=ifelse(clave=="P038",suma,0),
         P039=ifelse(clave=="P039",suma,0),
         P040=ifelse(clave=="P040",suma,0),
         P041=ifelse(clave=="P041",suma,0),
         P042=ifelse(clave=="P042",suma,0),
         P043=ifelse(clave=="P043",suma,0),
         P044=ifelse(clave=="P044",suma,0),
         P045=ifelse(clave=="P045",suma,0),
         P046=ifelse(clave=="P046",suma,0),
         P047=ifelse(clave=="P047",suma,0),
         P048=ifelse(clave=="P048",suma,0),
         P049=ifelse(clave=="P049",suma,0),
         P050=ifelse(clave=="P050",suma,0),
         P051=ifelse(clave=="P051",suma,0),
         P052=ifelse(clave=="P052",suma,0),
         P053=ifelse(clave=="P053",suma,0),
         P054=ifelse(clave=="P054",suma,0),
         P055=ifelse(clave=="P055",suma,0),
         P056=ifelse(clave=="P056",suma,0),
         P057=ifelse(clave=="P057",suma,0),
         P058=ifelse(clave=="P058",suma,0),
         P059=ifelse(clave=="P059",suma,0),
         P060=ifelse(clave=="P060",suma,0),
         P061=ifelse(clave=="P061",suma,0),
         P062=ifelse(clave=="P062",suma,0),
         P063=ifelse(clave=="P063",suma,0),
         P064=ifelse(clave=="P064",suma,0),
         P065=ifelse(clave=="P065",suma,0),
         P066=ifelse(clave=="P066",suma,0),
         P067=ifelse(clave=="P067",suma,0),
         P068=ifelse(clave=="P068",suma,0),
         P069=ifelse(clave=="P069",suma,0),
         P070=ifelse(clave=="P070",suma,0),
         P071=ifelse(clave=="P071",suma,0),
         P072=ifelse(clave=="P072",suma,0),
         P073=ifelse(clave=="P073",suma,0),
         P074=ifelse(clave=="P074",suma,0),
         P075=ifelse(clave=="P075",suma,0),
         P076=ifelse(clave=="P076",suma,0),
         P077=ifelse(clave=="P077",suma,0),
         P078=ifelse(clave=="P078",suma,0),
         P079=ifelse(clave=="P079",suma,0),
         P080=ifelse(clave=="P080",suma,0),
         P081=ifelse(clave=="P081",suma,0))

ingreso<-ingreso%>%
  mutate(labor=P001+P002+P003+P004+P005+P006+P007+P008+P009+P011+P013+P014+P015+P016+P018+P021+P022+P035+P036+P067+P068+P069+P070+P071+P072+P073+P074+P075+P076+P077+P078+P079+P080+P081,
         capital=P012+P019+P023+P024+P025+P026+P027+P028+P029+P030+P031,
         pensions=P032+P033,
         private_trans=P037+P039+P040+P049,
         remittances=P041,
         government=P038+P042+P043+P044+P045+P046+P047+P048)

ingreso<-ingreso%>%
  mutate(total=labor+capital+pensions+private_trans+remittances+government)


ingreso<-ingreso %>%
  group_by(folioviv,foliohog)%>%
  summarize(labor=sum(labor),
            capital=sum(capital),
            pensions=sum(pensions),
            private_trans=sum(private_trans),
            remittances=sum(remittances),
            government=sum(government),
            total=sum(total))


conc<-read.dbf("concentradohogar.dbf")

conc<-merge(conc,ingreso,by = c("folioviv","foliohog"))

conc<-conc%>%
select(folioviv,foliohog,tam_loc,upm,factor,ing_cor,ingtrab,labor,trabajo,sueldos,horas_extr,comisiones,aguinaldo,
       indemtrab,otra_rem,remu_espec,negocio,noagrop,industria,comercio,servicios,agrope,agricolas,pecuarios,
       reproducc,pesca,otros_trab,rentas,utilidad,arrenda,transfer,jubilacion,becas,donativos,
       remesas,bene_gob,transf_hog,trans_inst,estim_alqu,otros_ing,capital,pensions,private_trans,
       remittances,government,total)

conc<-conc%>%
  mutate(ing_cor=ing_cor-estim_alqu)

conc<-conc%>%
  mutate(private_trans=private_trans+transf_hog+trans_inst)

conc<-conc%>%
  mutate(labor=labor+remu_espec)

conc<-conc%>%
  mutate(labor=round(labor,2))

conc<-conc%>%
  mutate(ingtrab=round(ingtrab,2))

mean(conc$labor)

mean(conc$ingtrab)


mean(conc$rentas)

mean(conc$capital)

all.equal(conc$ingtrab,conc$labor)
all.equal(conc$rentas,conc$capital)
all.equal(conc$jubilacion,conc$pensions)
all.equal(conc$remesas,conc$remittances)

conc<-conc%>%
  mutate(total=labor+capital+pensions+private_trans+remittances+government)

mean(conc$ing_cor)
mean(conc$total)

conc<-conc%>%
  mutate(prueba=round(ing_cor-total,2))

summary(conc$prueba)


