set.seed(123)
#library checker 

libs = c("rfm", "lubridate","knitr", "kableExtra", "magrittr", "dplyr", "ggplot2", "DT", "grDevices", "RColorBrewer", "treemap", "readxl")

for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
  library(i,character.only = TRUE)
}
lapply(libs, require, character.only = TRUE)


# Set working directory to the folder where I saved my code 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#read file
EU_Superstore <- read_excel("EU Superstore.xlsx")
View (EU_Superstore)

#check if all data are input properly if not make changes
sapply(EU_Superstore, class)

#change the date-related factor to date
EU_Superstore$`Order Date` <- as.Date(EU_Superstore$`Order Date`, format="%y-%m-%d")

#Basic rfm analaysis
#set the analysis date (to calculate recency)
analysis_date <-lubridate::as_date("2019-01-01")

#Change the column name. This shouldn't be necessary, but I found the code to be often problematic if not changed. 
names(EU_Superstore)[names(EU_Superstore) == "Customer ID"] <- "customer_id"
names(EU_Superstore)[names(EU_Superstore) == "Order Date"] <- "order_date"
names(EU_Superstore)[names(EU_Superstore) == "Sales"] <- "revenue"

rfm_result <- rfm_table_order(EU_Superstore, customer_id, order_date, revenue, analysis_date)
rfm_result

#Produce a table
rfm_result %>%
  use_series(rfm) %>%
  slice(1:10) %>%
  kable() %>%
  kable_styling()

#histogram
rfm_histograms(rfm_result)

#heatmap
rfm_heatmap(rfm_result)

#Barchart
rfm_bar_chart(rfm_result)

#Customers by orders
rfm_order_dist(rfm_result)

#Scatter plot (recencey vs freqeuncy)
rfm_rf_plot(rfm_result)

#Scatter plot (recency vs monetary)
rfm_rm_plot(rfm_result)

#Scatter plot (frequency vs monetary)
rfm_fm_plot(rfm_result)

#Perform segmentation based on the rfm score
segment <- c(
  "Champions", "Loyal Customers", "Potential Loyalist",
  "Recent Customers", "Promising", "Need Attention",
  "About To Sleep", "At Risk", "Can't Lose Them", "Hibernating",
  "Lost"
)

description <- c(
  "Bought recently, buy often and spend the most",
  "Spend good money with us often. Responsive to promotions",
  "Recent customers, spent good amount, bought more than once",
  "Bought more recently, but not often",
  "Recent shoppers, but haven't spent much",
  "Above average recency, frequency & monetary values, may not have bought recently",
  "Below average recency, frequency & monetary values, will lose them if not reactivate",
  "Spent big money, purchased often but long time ago",
  "Made the biggest purchases and often, but long time ago",
  "Low spenders, low frequency, purchased long time ago",
  "Lowest recency, frequency & monetary scores"
)

action <- c("Show them your love. Can be early adopters for new products. Brand ambassdors",
            "They just need to do everything they do, but more. Engage them. Ask for more interactions",
            "Offer membership / loyalty program, recommend other products",
            "keep it heated, entice them with CRM",
            "Offer free trials, promotions, offer to have a second look",
            "Reactivation: Make limited time offers, recommend based on past purchases",
            "Share valuable resources, recommend popular products / renewals at discount, reconnect with them",
            "Ask them to come back! Reconnect",
            "Understand why they're gone. What went wrong? Talk to them",
            "Offer other relevant products, remind them about you, recreate brand value",
            "If must: revive interest with reach out campaign, ignore otherwise"
)

recency <- c("4 - 5", "2 - 5", "3 - 5", "4 - 5", "3 - 4", "2 - 3", "2 - 3", "<= 2", "<= 1", "1 - 2", "<= 2")
frequency <- c("4 - 5", "3 - 5", "1 - 3", "<= 1", "<= 1", "2 - 3", "<= 2", "2 - 5", "4 - 5", "1 - 2", "<= 2")
monetary <- c("4 - 5", "3 - 5", "1 - 3", "<= 1", "<= 1", "2 - 3", "<= 2", "2 - 5", "4 - 5", "1 - 2", "<= 2")

segments <- tibble(
  Segment = segment, Description = description, Action = action, R = recency, 'F'= frequency, M = monetary
)

segments %>%
  kable() %>%
  kable_styling(full_width = TRUE, font_size = 12)

#RFM segments
rfm_segments <- rfm_result %>%
  use_series(rfm) %>%
  mutate(
    segment = case_when(
      (recency_score %>% between(4,5)) & (frequency_score %>% between(4,5)) & 
        (monetary_score %>% between(4,5)) ~ "Champions",
      (recency_score %>% between(2, 5)) & (frequency_score %>% between(3, 5)) &
        (monetary_score %>% between(3, 5)) ~ "Loyal Customers",
      (recency_score %>% between(3, 5)) & (frequency_score %>% between(1, 3)) &
        (monetary_score %>% between(1, 3)) ~ "Potential Loyalist",
      (recency_score %>% between(4, 5)) & (frequency_score == 1) &
        (monetary_score == 1) ~ "New Customers",
      (recency_score %>% between(3, 4)) & (frequency_score == 1) &
        (monetary_score == 1) ~ "Promising",
      (recency_score %>% between(2, 3)) & (frequency_score %>% between(2, 3)) &
        (monetary_score %>% between(2, 3)) ~ "Needs Attention",
      (recency_score %>% between(2, 3)) & (frequency_score <= 2) &
        (monetary_score <= 2) ~ "About To Sleep",
      (recency_score <= 2) & (frequency_score %>% between(2, 5)) &
        (monetary_score %>% between(2, 5)) ~ "At Risk",
      (recency_score == 1) & (frequency_score %>% between(4, 5)) &
        (monetary_score %>% between(4, 5)) ~ "Cant Lose Them",
      (recency_score %>% between(1, 2)) & (frequency_score %>% between(1, 2)) &
        (monetary_score %>% between(1, 2)) ~ "Hibernating",
      (recency_score <= 2) & (frequency_score <= 2) &
        (monetary_score <= 2) ~ "Lost",
      TRUE ~ "Others"
    )
  ) %>%
  select(
    customer_id,segment, rfm_score, transaction_count, recency_days, amount
  )

# use datatable (interactive table)
rfm_segments %>%
  datatable(
    filter = "top",
    options = list(pageLength = 5, autoWidth = TRUE),
    colnames = c(
      "Customer ID", "RFM Segment","RFM Score","Orders", "Recency", "Total Spend"
    )
  )

#Checking Segment Size
rfm_segments %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

#Plotting segments
# Median recency
data <- rfm_segments %>%
  group_by(segment) %>%
  select(segment, recency_days) %>%
  summarize(median(recency_days)) %>%
  rename(segment = segment, avg_recency = 'median(recency_days)') %>%
  arrange(avg_recency)

n_fill <- nrow(data)

ggplot(data, aes(segment, avg_recency)) +
  geom_bar(stat="identity", fill = brewer.pal(n = n_fill, name= "Set1")) +
  xlab("Segment") + ylab("Median Recency") +
  ggtitle("Median Recency by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Median frequency
data <- 
  rfm_segments %>%
  group_by(segment) %>%
  select(segment, transaction_count) %>%
  summarize(median(transaction_count)) %>%
  rename(segment = segment, avg_frequency = `median(transaction_count)`) %>%
  arrange(avg_frequency) 

n_fill <- nrow(data)

ggplot(data, aes(segment, avg_frequency)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Frequency") +
  ggtitle("Median Frequency by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#Median Monetary
data <- 
  rfm_segments %>%
  group_by(segment) %>%
  select(segment, amount) %>%
  summarize(median(amount)) %>%
  rename(segment = segment, avg_monetary = `median(amount)`) %>%
  arrange(avg_monetary) 

n_fill <- nrow(data)

ggplot(data, aes(segment, avg_monetary)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Monetary Value") +
  ggtitle("Median Monetary Value by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#Summary of the segments
rfm_summary <- rfm_segments %>%
  group_by(segment) %>%
  tally() %>%
  rename(Segment = segment, Count = n) %>%
  mutate(percent = round((percent = Count / sum(Count) * 100),2)) %>%
  arrange(-Count)

rfm_summary

# Plotting an overview treemap
rfm_summary$label <- paste(rfm_summary$Segment, rfm_summary$Count, rfm_summary$percent, "%", sep = "\n")

png(filename="tree.png",width=800, height=600)
treemap(rfm_summary,
        index=c("label"),
        vSize="Count",
        type="index",
        algorithm="pivotSize"
)

