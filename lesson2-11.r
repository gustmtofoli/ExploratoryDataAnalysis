getwd()

statesInfo <- read.csv('stateData.csv')
statesInfo

stateSubset <- subset(statesInfo, state.region == 1)
stateSubset <- subset(stateSubset, highSchoolGrad > 50)
stateSubset

head(stateSubset, 1) # segundo parametro é a quantidade de linhas
dim(stateSubset)

stateSubsetBracket <- statesInfo[statesInfo$state.region == 1, ]
stateSubsetBracket
