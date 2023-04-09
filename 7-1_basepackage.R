string <- c("data analysis is useful",
            "business analytics is helpful",
            "visualization of data is interesting for data scientists")

string

## grep(), grepl() 몇 번째 요소인지

grep("data", string)
grep("data", string, value = TRUE)

string[grep("data", string)]

grepl("data", string) #true/false로 리턴

## regrexper(), gregexpr()

regexpr(pattern = "data", text = string) #첫 번째 data길이(시작위치), 타입, 존재 여부 논리값 

gregexpr("data", string) #결과를 리스트로 출력(중복도 같이) 

## regmatches() 패턴추출 

regmatches(x=string, m= regexpr(pattern = "data", text = string))

regmatches(x=string, m= gregexpr(pattern = "data", text = string)) #리스트형태 

regmatches(string, regexpr("data",string), invert = TRUE) #첫 번째 등장하는 data를 제외한 나머지 출력 

regmatches(string, gregexpr("data",string), invert = TRUE) #모든 data 제외


## sub(), gsub() 치환

sub(pattern = "data", replacement = "text", x=string) #첫 번째 data만 치환
gsub(pattern = "data", replacement = "text", x=string) #모든 data 치환

## strsplit() 분할

strsplit(string, " ") #리스트형태
unlist(strsplit(string, " ")) #리스트를 벡터형태로 
unique(unlist(strsplit(string, " "))) #동일한 단어 하나로 처리

gsub("is|of|for", "", unique(unlist(strsplit(string, " "))) ) #불용어 동의어 처리 
gsub("date", "data", unique(unlist(strsplit(string, " "))) )