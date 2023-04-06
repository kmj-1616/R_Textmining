txt <- "Data Analytics is useful. Data Analytics is also interesting."
words <- c("at","bat","cat","chaenomeloes","chase",
           "cheep","check","cheese","chick","hat")
words2 <- c("12 Dec","OK","http://","<TITLE>Time?<TITLE>","12345","Hi there")

## 텍스트의 치환
sub()
gsub()
?sub()

txt <- "Data Analytics is useful. Data Analytics is also interesting."
sub(pattern = "Data", replacement = "Business", x= txt) #첫 번째 문자열만 치환됨
gsub(pattern = "Data", replacement = "Business", x= txt) #모든 문자열 치환
gsub(pattern = "Data", replacement = "", x= txt)

text2 <- c("product.csv","order.csv","customer.csv")
gsub(".csv", "", text2)


## 정규 표현식

words <- c("at","bat","cat","chaenomeloes","chase",
           "cheep","check","cheese","chick","hat","chasse","ca-t")

grep("che",words, value = TRUE)
grep("a",words, value = TRUE)
grep("at",words, value = TRUE)

grep("[ch]", words, value = TRUE) #c 또는 h가 포함된 문자열 
grep("[at]", words, value = TRUE)

grep("ch|at", words, value = TRUE) #ch 또는 at가 포함된 문자열 
grep("che|at", words, value = TRUE)

grep("ch(e|i)ck", words, value = TRUE)
#ch와 ck 사이에 가운데에 e나 i가 들어간 문자열


## 수량자 ? * + {n} {n,} {n,m}

grep("chas?e", words, value = TRUE)
#cha 다음에 s가 없거나 1회 있고 뒤에 e가 있는 문자열 

grep("chas*e", words, value = TRUE) #s가 0회 이상

grep("chas+e", words, value = TRUE) #s가 1회 이상

grep("ch(a*|e*)se", words, value = TRUE) #ch와 se 사이에 a가 1회 이상 또는 e가 1회 이상 


## 메타문자 ^ $ 

grep("^c", words, value = TRUE) #c로 시작하는
grep("t$", words, value = TRUE) #t로 끝나는

grep("^c.t$", words, value = TRUE) #c로 시작하고 t로 끝나는데 사이에 문자가 1개 있는
grep("^c.*t$", words, value = TRUE)#c로 시작하고 t로 끝나는데 사이에 문자가 0개 이상 있는

grep("^[ch]?at$", words, value = TRUE)


## 문자 클래스

words2 <- c("12 Dec","OK","http://","<TITLE>Time?<TITLE>","12345","Hi there")

grep("[[:alnum:]]", words2, value = TRUE) #알파벳과 숫자를 포함한 모든 문자열 
grep("[[:alpha:]]", words2, value = TRUE) #알파벳이 포함된 
grep("[[:digit:]]", words2, value = TRUE) #숫자가 포함된
grep("[[:punct:]]", words2, value = TRUE) #특수문자가 포함된
grep("[[:space:]]", words2, value = TRUE) #공백이 포함된

## 문자 클래스 시퀀스

grep("|w+", words2, value = TRUE) #단어 문자
grep("|s+", words2, value = TRUE) #스페이스 문자
grep("|d+", words2, value = TRUE) #숫자
