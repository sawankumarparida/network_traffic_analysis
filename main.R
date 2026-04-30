set.seed(123)
num_records <- 1000

timestamps <- seq.POSIXt(from = as.POSIXct("2024-06-03 00:00:00"), 
                         by = "hour", length.out = num_records)

generate_ipv4 <- function(n) {
  paste(sample(0:255, n, replace = TRUE), sample(0:255, n, replace = TRUE), 
        sample(0:255, n, replace = TRUE), sample(0:255, n, replace = TRUE), sep = ".")
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
 
# Visualize the traffic data

install.packages("ggplot2")
library(ggplot2)

options(repr.plot.width=10, repr.plot.height=6)

ggplot(traffic_data, aes(x = timestamp, y = bytes_transferred)) +
  geom_line() +
  labs(title = "Network Traffic Over Time",
       x = "Timestamp",
       y = "Bytes Transferred")