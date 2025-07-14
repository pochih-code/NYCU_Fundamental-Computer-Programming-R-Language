library(rvest)
stock<-read_html("https://tw.stock.yahoo.com/d/s/dividend_0050.html")
tables<-html_nodes(stock, ".M\\(0\\)")
table<-tables[1]
lists<-html_nodes(table, "li")
y<-c()
p<-c()
for (i in length(lists):1){
  cols<-html_nodes(lists[i], "div")
  year<-html_text(cols[2])
  profit<-html_text(cols[5])
  
  if (year!=''){
    y<-c(y, year)
    p<-c(p, profit)
  }
}
p<-as.numeric(p)
barplot(p, names.arg = y, main="台灣五十現金股利", xlab="年份", ylab="現金股利")