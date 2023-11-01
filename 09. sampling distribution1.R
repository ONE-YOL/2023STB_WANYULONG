#1
salary=c(45,50,50,55,55,60,60,65,70,90)
com2=combn(salary,2)
print(com2)

#2
com2t=t(com2)
z=rowMeans(com2t)
barplot(table(z),xlab="연봉평균",ylab="도수",main="연봉평균의 분포")
meanofz=mean(z)
print(meanofz)

#3
data50=c(1:50)
com2=combn(data50,2)
com2t=t(com2)
z2=rowMeans(com2t)
meanofz2=mean(z2)
print(meanofz2)
barplot(table(z2),xlab="평균2",ylab="도수",main="표본크기2의 도수분포포")

#4
com3=combn(data50,3)
com3t=t(com3)
z3=rowMeans(com3t)
meanofz3=mean(z3)
print(meanofz3)
barplot(table(z3),xlab="평균3",ylab="도수",main="표본크기3의 도수분포포")

#5
com5=combn(data50,5)
com5t=t(com5)
z5=rowMeans(com5t)
meanofz5=mean(z5)
print(meanofz5)
barplot(table(z5),xlab="평균3",ylab="도수",main="표본크기5의 도수분포포")

#6
sampling=c(1:100)
print(sampling)
sample(x=sampling, size=10)

#7
install.packages("sampling")
library(sampling)
sam2=strata(data=iris,stratanames=c("Species"),size=c(3,3,3),method='srswor') #iris데이터 사용하여 표본추출, Species층을 기준으로
sample2=getdata(data=iris,m=sam2) 
print(sample2)



















