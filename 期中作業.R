# 第一題
scores <- c(51, 32, 6)
# 學生姓名
student_names <- c("A", "B", "C")
# 調整成績
adjusted_scores <- round(sqrt(scores) * 10)
# 判斷是否被當
status <- ifelse(adjusted_scores >= 60, "過", "當")
result <- data.frame(學生姓名 = student_names, 調整前成績 = scores, 調整後成績 = adjusted_scores, 過或當 = status,stringsAsFactors = FALSE)
# 測試結果
print(result)

# 第二題
球魔方 <- function(n) {
  line <- matrix(c(1,2,3,4,5,6,7,8,9,1,4,7,2,5,8,3,6,9,1,5,9,3,5,7), nrow=8, ncol=3, byrow=TRUE)
  count <- 0
  print("==================")
  for (i in 1:n) {
    print(i)
    print("預設中獎:")
    vec <- rep(NA, 9)
    vec[line[sample(1:8, 1),]] <- "O"
    mat <- matrix(vec, nrow=3, byrow=TRUE)
    print(mat)
    print("玩家連線:")
    vec <- rep(NA, 9)
    vec[sample(1:9, 3)] <- "O"
    mat2 <- matrix(vec, nrow=3, byrow=TRUE)
    print(mat2)
    if (identical(mat, mat2)) {
      print("結果 : 中獎")
      count <- count + 1
    } else {
      print("結果 : 再一次")
    }
    print("==================")
  }
  result <- c(未中獎=n-count, 中獎=count)
  print(result)
  barplot(result, main='球魔方中獎統計圖表')
}
球魔方(100000)