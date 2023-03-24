## Apply() 계열 함수

apply()
lapply()
sapply()
?apply()

x <- matrix(1:20, 4, 5)
x

apply(X=x, MARGIN = 1, FUN = max) #1은 행, 2는 열 
apply(X=x, MARGIN = 2, FUN = max)
apply(X=x, MARGIN = 2, FUN = min)
apply(X=x, MARGIN = 2, FUN = mean)
apply(X=x, MARGIN = 2, FUN = sum)
apply(x, 1, sum)
apply(x, 2, sum)

sum <- apply(x, 1, sum) #제일 오른쪽 열에 합계 붙이기 
cbind(x, sum)

class(x)
as.data.frame(x)
x$sum <- apply(x, 1, sum) #데이터프레임 형태로 바꿔서 붙이기 


y <- array(1:24, c(4,3,2))
y
apply(y, 1, paste, collapse =",") #행끼리 합침 
apply(y, 1, paste)

a <- c(1,5,9,13,17,21)
a
paste(a) #벡터를 문자열로 바꿔서 하나하나
paste(a, collapse = " ")
paste(a, collapse = ",") #구분되어 있는 것을 무너뜨림 

apply(y, 2, paste, collapse = ",")
apply(y, 3, paste, collapse = ",")
apply(y, c(1,2), paste, collapse = ",")

paste("a","b", sep = "-")


Titanic
str(Titanic)

apply(Titanic, 1, sum)
apply(Titanic, 4, sum)
apply(Titanic, 2, sum)
apply(Titanic, "Class", sum)

apply(Titanic, c(1,4), sum)


lapply() #list
sapply() #자동

exams <- list(Spring_2020 = c(78,60,89,90,96,54),
              Spring_2021 = c(85,78,69,90,95),
              Spring_2022 = c(98,96,94,89,99,100,87),
              Spring_2023 = c(86,98,76,89,57,79))

exams

lapply(exams, length)
sapply(exams, length)
sapply(exams, mean)
sapply(exams, sd)
sapply(exams, range)


?iris
head(iris)

str(iris)

lapply(iris, class)

sapply(iris, class)
sapply(iris, mean)

sapply(iris, function(x) ifelse(is.numeric(x), mean(x), NA))

