text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetr$ian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the ecocomy added 160,000 jobs.")

install.packages("tm")
library(tm)
data(crude)
crude

crude[[1]]
crude[[1]]$content
crude[[1]]$meta

text

VCorpus()
getSources()

corpus.docs <- VCorpus(VectorSource(text))
class(corpus.docs) #corpus 형태

inspect(corpus.docs[1])
inspect(corpus.docs[[1]]) #더 자세함

as.character(corpus.docs[[1]]) #문자열만 가져옴

lapply(corpus.docs, as.character) #전부 다 문자열만

str(corpus.docs)

corpus.docs[[1]]$content #첫 번째 콘텐트만 뽑음
lapply(corpus.docs, content) #콘텐트들만 리스트로 뽑음

as.vector(unlist(lapply(corpus.docs, content))) #리스트 해제하고 벡터 형태

paste(as.vector(unlist(lapply(corpus.docs, content))), collapse=" ") #하나로 만듦


#메타 정보 가져옴
corpus.docs[[1]]$meta

meta(corpus.docs[[1]])
meta(corpus.docs[[1]], tag = "author") #구체적으로
meta(corpus.docs[[1]], tag = "id")

#비어 있는 메타 채우기
meta(corpus.docs[[1]], tag = "author", type="local") <- "Dong-A"
meta(corpus.docs[[1]], tag = "author")

#3개 문서 전체 채우기
cor.author <- c("Dong-A","Ryu","Kwon")
meta(corpus.docs, tag = "author", type="local") <- cor.author
lapply(corpus.docs, meta, tag = "author")


##9-2

lapply(corpus.docs, meta)

#새 메타 추가하기
category <- c("health","lifestyle","business")
meta(corpus.docs, tag = "category", type="local") <- category

#메타 삭제하기
meta(corpus.docs, tag = "origin", type="local") <- NULL

#메타데이터 중에 특정 조건을 만족하는 것 추출
corpus.docs.filter <- tm_filter(corpus.docs, FUN = function(x)
    any(grep("weight|diet", content(x))))

lapply(corpus.docs.filter, content) #조건 만족하는 것 중 콘텐트만 뽑아서 보기

#작가이름을 만족하는 것만 뽑기(T/F)
index <- meta(corpus.docs, "author") == "Dong-A" | meta(corpus.docs, "author") == "Ryu"

lapply(corpus.docs[index], content) #그 중 콘텐트만

#작업폴더에 텍스트파일로 저장
writeCorpus(corpus.docs)
list.files(pattern = "\\.txt")


## 텍스트 정제
getTransformations() #tm패키지의 기능 확인

tm_map()

toupper()
tolower()

content_transformer()


lapply(corpus.docs, content)

#모두 소문자로
corpus.docs <- tm_map(corpus.docs, content_transformer(tolower))


#불용어 stopwords 제거
stopwords("english")

corpus.docs <- tm_map(corpus.docs, removeWords, stopwords("english"))

lapply(corpus.docs, content)

#불용어사전 만들어서 url은 공백으로 처리
myRemoves <- content_transformer(function(x, pattern)
  {return(gsub(pattern, "", x))})

corpus.docs <- tm_map(corpus.docs, myRemoves, "(f|ht)tp\\S+||s*")

lapply(corpus.docs, content)

#문장부호 제거
corpus.docs <- tm_map(corpus.docs, removePunctuation)
lapply(corpus.docs, content)

#숫자 제거
corpus.docs <- tm_map(corpus.docs, removeNumbers)
lapply(corpus.docs, content)

#중간에 있는 공백 제거
corpus.docs <- tm_map(corpus.docs, stripWhitespace)
lapply(corpus.docs, content)

#문장 앞뒤에 있는 공백 제거
corpus.docs <- tm_map(corpus.docs, content_transformer(trimws))
lapply(corpus.docs, content)

#어간어미 추출
corpus.docs <- tm_map(corpus.docs, stemDocument)
lapply(corpus.docs, content)

#동의어 
corpus.docs <- tm_map(corpus.docs, content_transformer(gsub),
                      pattern="economist", replacement="economi")

lapply(corpus.docs, content)


