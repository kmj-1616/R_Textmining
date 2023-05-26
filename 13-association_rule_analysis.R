install.packages("installr")
library(installr)
check.for.updates.R()
install.R()
version


install.packages("arules")
library(arules)

# convert transactions into a list
a_list = list(
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("c","e"),
  c("a","b","d","e")
)
a_list

# set transaction names
names(a_list) = paste("Tr",c(1:5), sep = "")
a_list

trans = as(a_list, "transactions")

summary(trans)

image(trans)

# Relative Frequency Plot by items
itemFrequencyPlot(trans, support=0.1, cex.names=0.8)

# 연관분석
library(arules)
m<-apriori(trans, parameter=list(support=0.2, confidence=0.5))
summary(m)

# 연관규칙 조회 (5개)
inspect(m[1:12])
inspect(sort(m, by="support"))
inspect(sort(m, by="lift"))
###############################
##################  Association Rule #####################

# list로 부터 생성된 Tansction Data
data <- list(Tran1=c("오렌지", "주스", "사이다"),
             Tran2=c("우유", "오렌지", "주스", "식기세척제"),
             Tran3=c("오렌지", "주스", "세제"),
             Tran4=c("오렌지", "식기세척제", "사이다"),
             Tran5=c("식기세척제", "사이다"))
data
trans<-as(data,"transactions")
trans


##################################
# Transaction 데이터와 관련된 함수 #
# length(trans) : Transaction 갯수
# inspect(trans) : Transaction 조회
# as(trans, "list") : Transaction 데이터를 리스트로 변환
# transactionInfo(trans) : TransactionID 조회
# itemsetInfor(trans) : itemsetID 조회
# itemInfo(trans) : 모든 items 목록 조회
############################################################

# Relative Frequency Plot by items
itemFrequencyPlot(trans, support=0.1, cex.names=0.8)

# 연관분석
library(arules)
m<-apriori(trans, parameter=list(support=0.4, confidence=0.5))
summary(m)

# 연관규칙 조회 (5개)
inspect(m[1:12])
inspect(sort(m, by="support"))
inspect(sort(m, by="lift"))

############################
#연관규칙시각화
###########################
install.packages("arulesViz")
library(arulesViz)
plot(m, method="graph")
plot(m, method="graph", control=list(type="items"))
# grouped는 LHS와 RHS간으 연관규칙을 그룹화하여 시각화
# LHS에 중요항목을 표시하고 건수를 표시
# 지지도는 원의 크기로 표시하고, 향상도는 원의 색으로 표시

plot(m, method="grouped", control=list(k=5))