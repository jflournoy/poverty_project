data = read.table("/Users/rx/Documents/Research/Poverty/PhaseI_StudyOne/Data/Poverty_PhaseI_StudyOne_Mturk_INR", header=TRUE, sep=",")

data$Sex=revalue(data$Gender,c("Male  "="1","Female"="2"))
data$Education=revalue(data$Education, c("Less than high school"="1","High school graduate "="2","Some college         "="3","2 year degree        "="4","4 year degree        "="5","Professional degree  "="6","Masters              "="7"))
data$Race=revalue(data$Ethnicity, c("White  not of Hispanic Origin    "="1","Black  not of Hispanic Origin    "="2","American Indian or Alaskan Native"="3","Asian or Pacific Islander        "="4","South Asian or Indian            "="4","Hispanic                         "="5","Other                            "="5"))

write.csv(data, file="Poverty_PhaseI_StudyOne_Mturk_INR")