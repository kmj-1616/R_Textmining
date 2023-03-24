x <- "We have a dream"
x
class(x)
nchar(x) #전체 문자열이 몇 개인지
length(x) #전체 벡터가 몇 개인지

y <- c("We","have","a","dream")
y
nchar(y)
length(y)
length(y[4])
nchar(y[4])

letters
sort(letters, decreasing = TRUE)

tolower() #소문자로-보통 전처리할 땐 소문자로 처리 
toupper() #대문자로

fox.says <- "It is only with the HEART that one can See Rightly"
fox.says
tolower(fox.says)
toupper(fox.says)


# text 분할
strsplit()

fox.said <- "what is essential is invisible to the eye"
fox.said
strsplit(fox.said, split = " ") #리스트
strsplit(fox.said, split = "")

fox.said.words <- unlist(strsplit(fox.said, split = " ")) #벡터 
fox.said.words
fox.said.words[3]
fox.said.words[1]

unlist(strsplit(fox.said, split = " "))[3]

strsplit(fox.said, split = " ")[[1]]
strsplit(fox.said, split = " ")[[1]][[3]]



p1 <- "You come at four in the afternoon, then at three I shall bebine to be happy"
p2 <- "One runs the irst of weeping a litter, if one lets himself be tamed"
p3 <- "what makes the desert beautiful is that somewhere it hides a well"

littleprince <- c(p1,p2,p3)
littleprince
strsplit(littleprince, split = " ")
strsplit(littleprince, split = " ")[[3]]
strsplit(littleprince, split = " ")[[3]][[5]]
strsplit(littleprince, split = " ")[[1]][[7]]


unique()

fox.said <- "WHAT IS ESSENTIAL is invisible to the Eye"
strsplit(fox.said, split = " ")
fox.said.words <- strsplit(fox.said, split = " ")[[1]]
fox.said.words

unique(fox.said.words) #unique는 대소문자를 구별함
unique(tolower(fox.said.words)) #소문자 처리해서 is를 하나로
unique(toupper(fox.said.words))


# text 결합 
paste()

paste("Everyone","wants","to","fly")
paste("Everyone","wants","to","fly",sep = "-")
paste("Everyone","wants","to","fly",sep = "")
paste0("Everyone","wants","to","fly")

paste(fox.said.words)

paste(pi, sqrt(pi))

paste("25 degress Celsius is", 25*1.8+32, "degree Fahrenheit")

heros <- c("Batman","Captin America","Hulk")
colors <- c("Black","Blue","Green")
paste(heros, colors)

paste("Type", 1:10)
paste(heros, "wants","to","fly")


fox.said.words
paste(fox.said.words, collapse = " ")
paste(fox.said.words, collapse = "-")
