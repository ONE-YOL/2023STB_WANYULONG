library(dplyr)
library(ggplot2)

install.packages("foreign")
library(foreign)

mental <-read.spss("KIPA_DATA_2022.SAV")

class(mental)
mental <-as.data.frame(mental)
class(mental)

mental <-mental %>%select(q32_2,q1_4,q32_1,q34_1,q55,d17,d1,d2,ara)%>%rename(suicide=q32_2, satisfaction=q1_4, loneliness=q32_1, family_belief=q34_1, wealth=q55, health=d17,sex=d1, age=d2, area=ara)

str(mental)

table(mental$suicide)
table(mental$health)
table(mental$satisfaction)

mental$suicide<-as.integer(mental$suicide)
mental$satisfaction<-as.integer(mental$satisfaction)
mental$loneliness<-as.integer(mental$loneliness)
mental$family_belief<-as.integer(mental$family_belief)
mental$wealth<-as.integer(mental$wealth)
mental$health<-as.integer(mental$health)

mental$satisfaction<-mental$satisfaction-1
mental$wealth<-mental$wealth-1

table(mental$wealth)
table(mental$satisfaction)

table(mental$sex)
table(mental$age)
table(mental$area)

mental$age<-ifelse(mental$age=="19~29세","20대",mental$age)

table(mental$age)

summary(mental)

#1(빈도분석) 성별
mental%>%
  group_by(sex)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),
         pct=round(n/total*100,1))

#2(빈도분석) 연령대
mental%>%
  group_by(age)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),
         pct=round(n/total*100,1))

#3(교차분석) 성별과연령대의교차분석–chisq.test
table(mental$sex,mental$age)
round(prop.table(table(mental$
                         sex,
                       mental$age),1)*100,1)

chisq.test(mental$sex,mental$age)

#4(평균분석) 6개분석변수의평균분석
mental%>%
  summarise(m1=mean(suicide),m2=mean(satisfaction),m3=mean(loneliness),m4=mean(family_belief),m5=mean(wealth),m6=mean(health))

#5(회귀분석)삶의만족도와외로움이자살충동에미치는영향
RA <-lm(data=mental,suicide~satisfaction+loneliness)
summary(RA)

#6(상관분석) 삶의만족도와외로움의상관관계
cor.test(mental$satisfaction,mental$loneliness)

#7(회귀분석) 가족신뢰도, 경제안정도, 건강상태가삶의만족도에미치는영향
installed.packages("ztable")
library(ztable)
RA <-lm(data=mental,satisfaction~family_belief+wealth+health)
summary(RA)
options(ztable.type="viewer")
ztable(RA)

#8(회귀분석) 가족신뢰도, 경제안정도, 건강상태가외로움에미치는영향
RA <-lm(data=mental,loneliness~family_belief+wealth+health)
summary(RA)
options(ztable.type="viewer")
ztable(RA)

#9(독립표본t검정) 성별삶의만족도차이
t.test(data=mental,satisfaction~sex)

#10(평균분석) 연령대별삶의만족도차이
mental%>%
  group_by(age)%>%
  summarise(m=mean(satisfaction))%>%
  arrange(desc(m))

#11(평균분석-그래프작성) 지역별삶의만족도분석과그래프그리기
area_satisfaction <-mental%>%
  group_by(area) %>%
  summarise(m=mean(satisfaction)) %>%
  arrange(desc(m))

ggplot(data=area_satisfaction, aes(x=reorder(area,m),y=m))+
  geom_col()+
  ggtitle("지역별 만족도")+
  xlab("지역")+
  ylab("만족도")+
  coord_flip()



