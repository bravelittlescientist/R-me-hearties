require(ggplot2)

# Helper functions
source("~/Code/R-me-hearties/R/botscale_dataframe.R")
source("~/Code/R-me-hearties/R/greyscale_multibar.R")
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

# CPU - Done
#greyscale_multibar(cpu_frame, cpu_title, bot_xlabel, cpu_ylabel)

# Packets Processed
