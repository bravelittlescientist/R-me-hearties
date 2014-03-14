# Helper functions
source("~/Code/R-me-hearties/R/botscale_dataframe.R")
source("~/Code/R-me-hearties/R/greyscale_multibar.R")
source("~/Code/R-me-hearties/R/multiplot.R")
se <- function(x) sd(x)/sqrt(length(x))

# Read log files
cpu_log = read.csv("~/Code/R-me-hearties/data/mar-14/cpu.csv") 
processed_log = read.csv("~/Code/R-me-hearties/data/mar-14/processed.csv")
total_inc_log = read.csv("~/Code/R-me-hearties/data/mar-14/incoming_total.csv")
total_out_log = read.csv("~/Code/R-me-hearties/data/mar-14/outgoing.csv")

# Plot titles and labels
cpu_title = "% CPU Usage of Sitting OpenSimulator Bots"
processed_title = "Incoming Packets Processed by Region Server"
total_inc_title = "Average Packets Received by Region Server"
total_out_title = "Average Packets Sent by Region Server"

bot_xlabel = "Number of Bots"
cpu_ylabel = "% CPU Usage"
packets_ylabel = "Average packets/s"

# Means and errors of data
cpu_means = colMeans(cpu_log)
cpu_errors = apply(cpu_log, 2, se)
processed_means = colMeans(processed_log)
processed_errors = apply(processed_log, 2, se)
total_inc_means = colMeans(total_inc_log)
total_inc_errors = apply(total_inc_log, 2, se)
total_out_means = colMeans(total_out_log)
total_out_errors = apply(total_out_log, 2, se)

# Create dataframes
cpu_frame = botscale_dataframe(cpu_means, cpu_errors)
processed_frame = botscale_dataframe(processed_means, processed_errors)
total_inc_frame = botscale_dataframe(total_inc_means, total_inc_errors)
total_out_frame = botscale_dataframe(total_out_means, total_out_errors)

# CPU
cpu_plot = greyscale_multibar(cpu_frame, cpu_title, bot_xlabel, cpu_ylabel)
#print(cpu_plot)

# Baseline Histogram
m <- ggplot(cpu_log[c(2)], aes(x=baseline_sitting_50)) 
m + geom_histogram() + ylab("count") + xlab("Average CPU load") + ggtitle("Distribution of CPU Load for 50 Bots, Baseline Configuration")
  
# Packets Processed, use Multiplot
cpu_plot = greyscale_multibar(cpu_frame, cpu_title, bot_xlabel, cpu_ylabel)
processed_plot = greyscale_multibar(processed_frame, processed_title, bot_xlabel, packets_ylabel, aux=theme(legend.position="none"))
total_inc_plot = greyscale_multibar(total_inc_frame, total_inc_title, bot_xlabel, packets_ylabel, aux=theme(legend.position="none"))
total_out_plot = greyscale_multibar(total_out_frame, total_out_title, bot_xlabel, packets_ylabel)

#multiplot(total_inc_plot, processed_plot, total_out_plot, cols=3)