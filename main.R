set.seed(123)
num_records <- 1000

timestamps <- seq.POSIXt(from = as.POSIXct("2024-06-03 00:00:00"),  # nolint
                         by = "hour", length.out = num_records)

generate_ipv4 <- function(n) {
  paste(sample(0:255, n, replace = TRUE), sample(0:255, n, replace = TRUE),  # nolint
        sample(0:255, n, replace = TRUE), sample(0:255, n, replace = TRUE), sep = ".") # nolint
}

source_ips <- generate_ipv4(num_records)
destination_ips <- generate_ipv4(num_records)

bytes_transferred <- sample(100:10000, num_records, replace = TRUE)

traffic_data <- data.frame(
  timestamp = timestamps,
  source_ip = source_ips,
  destination_ip = destination_ips,
  bytes_transferred = bytes_transferred
)

head(traffic_data)
  # nolint
# Visualize the traffic data

install.packages("ggplot2")
library(ggplot2)

options(repr.plot.width=10, repr.plot.height=6) # nolint

ggplot(traffic_data, aes(x = timestamp, y = bytes_transferred)) +
  geom_line() +
  labs(title = "Network Traffic Over Time",
       x = "Timestamp",
       y = "Bytes Transferred")

#Top Talkers Analysis

top_talkers <- aggregate(bytes_transferred ~ source_ip, data = traffic_data, FUN = sum) # nolint
top_talkers <- top_talkers[order(top_talkers$bytes_transferred, decreasing = TRUE), ] # nolint
top_talkers <- head(top_talkers, 10) # Selecting top 10 talkers for visualization # nolint

ggplot(top_talkers, aes(x = reorder(source_ip, bytes_transferred),
                        y = bytes_transferred, fill = bytes_transferred)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Top 10 Network Talkers",
       x = "Source IP Address",
       y = "Bytes Transferred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "none") +
  geom_text(aes(label = scales::comma(bytes_transferred)), vjust = -0.3, size = 3.5) # nolint