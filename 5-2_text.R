# 5-2

month.abb
paste(month.abb, 1:12)
paste(month.abb, 1:12, sep = "-")
paste(month.abb, 1:12, sep = "-", collapse = "_")
paste(month.abb, 1:12, sep = "-", collapse = " ")


outer(1:3, 1:3) #모든 가능한 곱하기 조합 
outer(c(1,2,3), c(1,2,3))

countries <- c("KOR","US","EU")
stat <- c("GDP","Pop","Area")
outer(countries, stat, FUN = paste, sep ="-") #각 조합이 구분자로 붙여짐 


# 고객 주문 결과를 보여주는 시나리오
customer <- "Kwon"
buysize <- 10
deliveryday <- 2

paste("hello", customer, ", your order of ", buysize,
      "product(s) will be delivered within ", deliveryday)


# sprintf()

sprintf("hello %s, your order of %s product(s) will be delivered within %s",
        customer, buysize, deliveryday) #한꺼번에 지정 가능

customer <- c("Kwon", "Ryu","Kim")
buysize <- c(10, 7, 9)
deliveryday <- c(2, 3, 8.5)

sprintf("hello %s, your order of %s product(s) will be delivered within %s",
        customer, buysize, deliveryday) #각각 출력 

?sprintf

sprintf("hello %s, your order of %s product(s) will be delivered within %.2f",
        customer, buysize, deliveryday)


# substr()
?substr
substr("Text Analytics", start = 1, stop = 4) #시작과 끝 지정해서 문자열 뽑아냄
substr("Text Analytics", start = 6, stop = 14)

substring("Text Analytics", 6 ) #지정한 곳부터 끝까지 뽑고 싶을 때 

class <- c("Data analytics","Data visualiztion","Data Science introduction")
substr(class, 1, 4) #각각의 1~4번째
substring(class, 6) #각각 6번째부터 끝까지(data 빼고)

countries <- c("Korea, KR", "United States, US", "China, CN")
substring(countries, nchar(countries)-1 ) #끝에서 하나까지 뽑음 
substr(countries, nchar(countries)-1, nchar(countries) )


# grep()

?islands
head(islands)
landnames <- names(islands)

index <- grep(pattern = "New", x= landnames)

landnames[index]
landnames[grep(pattern = "New", x= landnames)]
grep(pattern = "New", x= landnames, value = TRUE) #다 같은 결과


