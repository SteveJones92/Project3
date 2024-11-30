library(ggplot2)

# TODO best model

# TODO - 3 endpoints
# 1. pred
# 2. info
# 3. confusion


#* Find multiple of two numbers
#* @param param1 1st param
#* @param param2 2nd param
#* @param param3 3rd param
#* @get /pred
function(param1="hello", param2="world", param3="!"){
  paste(param1, param2, param3, sep=" ")
}


#* Info
#* @get /info
function(){
  c("Steven Jones", "https://stevejones92.github.io/ST558-DSF/homeworks/Homework_9.html")
}


#* Confusion
#* @serializer png
#* @get /confusion
function(){
  TP <- 30
  TN <- 100
  FP <- 70
  FN <- 20
  
  print(
    ggplot(data.frame(
      Prediction = c("Pos", "Pos", "Neg", "Neg"),
      Actual = c("Pos", "Neg", "Pos", "Neg"),
      Count = c(TP, FP, FN, TN)
    ), aes(x = Actual, y = Prediction)) +
      geom_tile(fill = "white", color = "black") + 
      geom_text(aes(label = Count), color = "black") +
      scale_x_discrete(limits = c("Pos", "Neg"))
  )
}

