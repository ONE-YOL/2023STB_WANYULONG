table(X2023_STB_survey $성별)

 
ECN <- table(X2023_STB_survey $성별)
prop.table(ECN)


table(X2023_STB_survey $성별, X2023_STB_survey $등급)


barplot(table(X2023_STB_survey $국적))


entry <- table(X2023_STB_survey $주택가)
barplot(entry, horiz = TRUE)


entry <- table(X2023_STB_survey $성별, X2023_STB_survey $등급)
barplot(entry, legend = TRUE)


pie(table(X2023_STB_survey $등급))

hist(X2023_STB_survey$나이, main="나이", col=terrain.colors(12))

boxplot(X2023_STB_survey$나이,X2023_STB_survey$등급, main="Grade Age", col="blue", names = c("나이","등급"))



plot(x=X2023_STB_survey$등급, y=X2023_STB_survey$나이, xlab="등급", ylab="나이", main="Grade Age")
plot(x=X2023_STB_survey$등급, y=X2023_STB_survey$나이, xlab="등급", ylab="나이", main="Grade Age",pch=24, col="red", bg="yellow", cex=1.5)
plot(x=X2023_STB_survey$등급, y=X2023_STB_survey$나이, xlab="등급", ylab="나이", main="Grade Age",type="h")
