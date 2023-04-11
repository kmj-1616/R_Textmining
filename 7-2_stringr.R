library(stringr)

string <- c("data analysis is useful",
            "business analytics is helpful",
            "visualization of data is interesting for data scientists")

## str_detect : grepl()과 동일 기능

str_detect(string = string, pattern = "data") #논리값으로 패턴존재여부 검출

str_detect(string, "DATA") #대소문자 구분
str_detect(string, fixed("DATA", ignore_case = TRUE)) #대소문자 무시 

str_detect(c("aby","acy","a.y"), "a.y") # .은 모든 문자 뜻하는 메타문자기 때문에 모두 true
str_detect(c("aby","acy","a.y"), fixed("a.y")) #fixed로 문자 그대로 고정 
str_detect(c("aby","acy","a.y"), "a\\.y") #fixed와 동일 효과 

## 위치 검출
str_locate(string, "data") #첫 번째만 검출
str_locate_all(string, "data") #모두 검출, 리스트형태 

##문자열 검출 : regmatches()와 동일 기능
str_extract(string, "data")
str_extract_all(string, "data") #리스트형태 반환 
str_extract_all(string, "data", simplify = TRUE) #행렬형태로

unlist(str_extract_all(string, "data"))



sentences5 <- sentences[1:5]

str_extract(sentences5, "(a|A|the|The) (\\w+)") #정관사와 같이 쓰인 단어 첫번째만추출
str_extract_all(sentences5, "(a|A|the|The) (\\w+)") #같이 쓰인 모든 단어 추출

str_match(sentences5, "(a|A|the|The) (\\w+)") #단어와 매치된 단어 같이 추출(첫번째만)
str_match_all(sentences5, "(a|A|the|The) (\\w+)") #문장에 대한 모든 매치 추출


## 치환
str_replace(string, "data", "text") #첫 번째만
str_replace_all(string, "data", "text") #모든 단어 치환

## 분할
str_split(string, " ") #리스트형태로
str_split(sentences5, " ")
unlist(str_split(sentences5, " ")) #하나의 벡터로
unique(unlist(str_split(sentences5, " "))) #중복은 한 번으로

str_split(sentences5, " ", n=3) #3번째까지만 분할 설정 
str_split(sentences5, " ", n=3, simplify = TRUE) #행렬 형태로


## 추가 기능들 
str_length(string) #길이

str_count(string, "data") #몇 번 사용되었는지
str_count(string, "\\w+")

mon <- 1:12
str_pad(mon, width = 2, side = "left", pad = "0") #01 02 03...12

string_pad <- str_pad(string, width = max(str_length(string)),
        side = "both", pad = " ") #길이가 가장 긴 것 기준 양쪽 공백 채움
str_trim(string_pad, side = "both") #양쪽 공백 지움
