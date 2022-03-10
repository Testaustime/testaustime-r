qry <- "https://time.lajp.fi/activity/data?min_duration=1"
bearer <- "Bearer <YOURTOKEN>"

library("httr")
res <- GET(qry, add_headers(Authorization=bearer))

library("rjson")
result <- fromJSON(content(res,as="text"))

result <- lapply(result, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

df <- as.data.frame(do.call("rbind", result))

df$duration <- strtoi(df$duration)
df$start_time <- as.Date(df$start_time)

data <- aggregate(duration~language+project_name+start_time, data=df, FUN=sum, na.rm=TRUE)
print(data)

library("ggplot2")
library("ggthemes")

png(file="language_and_duration_by_date.png", width=1024, height=720)
par(bg="#1c1c1c")

ggplot(data, aes(fill=language, y=duration, x=start_time)) + geom_bar(position="stack", stat="identity") + theme_dark() +
    theme(plot.background = element_rect(fill = "#3c3835", color = "black"), panel.background = element_rect("#242221", color="black")) +
    labs(x="Date", y="Time coded") + theme(text=element_text(size=20, color="#fbf1c7"), legend.background = element_rect("#4c4541", color="black")) +
    theme(axis.text=element_text(color="#b8bb26")) + theme(plot.title=element_text(color="#fb4934"))

png(file="project_and_duration_by_date.png", width=1024, height=720)
ggplot(data, aes(fill=project_name, y=duration, x=start_time)) + geom_bar(position="stack", stat="identity") + theme_dark() +
    theme(plot.background = element_rect(fill = "#3c3835", color = "black"), panel.background = element_rect("#242221", color="black")) +
    labs(x="Date", y="Time coded") + theme(text=element_text(size=20, color="#fbf1c7"), legend.background = element_rect("#4c4541", color="black")) +
    theme(axis.text=element_text(color="#b8bb26")) + theme(plot.title=element_text(color="#fb4934"))

while (!is.null(dev.list()))
    dev.off()
