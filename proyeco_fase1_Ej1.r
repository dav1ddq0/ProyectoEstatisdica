#compute statistics for a variable
stats <- function(x){
    round_x = as.numeric(as.character(formatC(x, digits = 4, format = "f")))
    x_mean = as.numeric(as.character(formatC(mean(x), digits = 4, format = "f")))
    ux <- unique(round_x)
    tab <- tabulate(match(round_x, ux))
    x_mode <- ux[tab == max(tab)]
    x_median <- as.numeric(as.character(formatC(median(x), digits = 4, format = "f")))
    x_sd <- as.numeric(as.character(formatC(sd(x), digits = 4, format = "f")))
    x_varc <- as.numeric(as.character(formatC(var(x), digits = 4, format = "f")))
    x_var_coeff <- as.numeric(as.character(formatC(x_sd/x_mean, digits = 4, format = "f")))
    quantiles <- as.numeric(as.character(formatC(quantile(x), digits = 4, format = "f")))
    answer <- list("mean" = x_mean, "mode" = x_mode, "median" = x_median, "sd" = x_sd, "varc" = x_varc, "var_coeff" = x_var_coeff, "quantiles" = quantiles)
}

#export data from a variable to a .txt
export_data <- function(data, statistics, file_name){
    this_mean = statistics$mean
    this_sd = statistics$sd
    this_size = length(data)
    this_varc = statistics$varc

    mean_error <- as.numeric(as.character(formatC(qnorm(0.975)*this_sd/sqrt(this_size), digits = 6, format = "f")))
    conf_range_lower_bound <- as.numeric(as.character(formatC(this_mean - mean_error, digits = 6, format = "f")))
    conf_range_upper_bound <- as.numeric(as.character(formatC(this_mean + mean_error, digits = 6, format = "f")))

    conf_range_varc_bounds <- as.numeric(as.character(formatC((this_size-1)*this_varc/qchisq(c(.975,.025), this_size - 1), digits = 6, format = "f")))
    
    fd <- file(file_name)
    open(fd, "w+b")
    write("Data :" , file = file_name, sep = "")
    write("", file = file_name, append = TRUE)
    write(data, file = file_name, ncolumns = 6 , sep = "\t")
    write(paste("\n\tmean ------> ", statistics$mean), file = file_name, append = TRUE)
    write(paste("\n\tmode ------> ", statistics$mode), file = file_name, append = TRUE)
    write(paste("\n\tmedian ------> ", statistics$median), file = file_name, append = TRUE)
    write(paste("\n\tsd ------> ", statistics$sd), file = file_name, append = TRUE)
    write(paste("\n\tvarc ------> ", statistics$varc), file = file_name, append = TRUE)
    write(paste("\n\tvar_coeff ------> ", statistics$var_coeff), file = file_name, append = TRUE)
    write(paste("\n\t1st quantile ------> ", statistics$quantiles[2]), file = file_name, append = TRUE)
    write(paste("\n\t2rd quantile ------> ", statistics$quantiles[3]), file = file_name, append = TRUE)
    write(paste("\n\t3rd quantile ------> ", statistics$quantiles[4]), file = file_name, append = TRUE) 
    write(paste("\n\tconf_range_mean ------> [", paste(conf_range_lower_bound, paste(", ", paste(conf_range_upper_bound, "]", sep = ""))), sep = ""), file = file_name, append = TRUE)
    write(paste("\n\tconf_range_varc ------> [", paste(conf_range_varc_bounds[1], paste(", ", paste(conf_range_varc_bounds[2], "]", sep = ""))), sep = ""), file = file_name, append = TRUE)
    write("\n\tAssuming confidence level ----> 95, so alpha ----> 0.05", file = file_name, append = TRUE)
    close(fd)
}   

#generate mean and sd
r_mean <- as.numeric(as.character(formatC(runif(1, min = 1.60, max = 1.70), digits = 2, format = "f")))
r_sd <- as.numeric(as.character(formatC(runif(1, min = 0.10, max = 0.25), digits = 2, format = "f")))
ppln <- as.numeric(as.character(formatC(rnorm(500, r_mean, r_sd), digits = 2, format = "f")))

#samples with replacement
rplc_sample1 <- sample(ppln, 100, replace = TRUE)
rplc_sample2 <- sample(ppln, 40, replace = TRUE)
rplc_sample3 <- sample(ppln, 20, replace = TRUE)
rplc_sample4 <- sample(ppln, 30, replace = TRUE)

#samples without replacement
nrplc_sample1 <- sample(ppln, 100, replace = FALSE)
nrplc_sample2 <- sample(ppln, 40, replace = FALSE)
nrplc_sample3 <- sample(ppln, 20, replace = FALSE)
nrplc_sample4 <- sample(ppln, 30, replace = FALSE)

#saving data to a txt
export_data(ppln, stats(ppln), "./datos generados (Ejercicio 1)/population_data.txt")

export_data(rplc_sample1, stats(rplc_sample1), "./datos generados (Ejercicio 1)/rplc_sample1.txt")
export_data(rplc_sample2, stats(rplc_sample2), "./datos generados (Ejercicio 1)/rplc_sample2.txt")
export_data(rplc_sample3, stats(rplc_sample3), "./datos generados (Ejercicio 1)/rplc_sample3.txt")
export_data(rplc_sample4, stats(rplc_sample4), "./datos generados (Ejercicio 1)/rplc_sample4.txt")

export_data(nrplc_sample1, stats(nrplc_sample1), "./datos generados (Ejercicio 1)/nrplc_sample1.txt")
export_data(nrplc_sample2, stats(nrplc_sample2), "./datos generados (Ejercicio 1)/nrplc_sample2.txt")
export_data(nrplc_sample3, stats(nrplc_sample3), "./datos generados (Ejercicio 1)/nrplc_sample3.txt")
export_data(nrplc_sample4, stats(nrplc_sample4), "./datos generados (Ejercicio 1)/nrplc_sample4.txt")

#generating histograms
png("./datos generados (Ejercicio 1)/population_hist.png")
hist(ppln)
dev.off()

png("./datos generados (Ejercicio 1)/replacement_sample1_hist.png")
hist(rplc_sample1)
dev.off()

png("./datos generados (Ejercicio 1)/replacement_sample2_hist.png")
hist(rplc_sample2)
dev.off()

png("./datos generados (Ejercicio 1)/replacement_sample3_hist.png")
hist(rplc_sample3)
dev.off()

png("./datos generados (Ejercicio 1)/replacement_sample4_hist.png")
hist(rplc_sample4)
dev.off()

png("./datos generados (Ejercicio 1)/no_replacement_sample1_hist.png")
hist(nrplc_sample1)
dev.off()

png("./datos generados (Ejercicio 1)/no_replacement_sample2_hist.png")
hist(nrplc_sample2)
dev.off()

png("./datos generados (Ejercicio 1)/no_replacement_sample3_hist.png")
hist(nrplc_sample3)
dev.off()

png("./datos generados (Ejercicio 1)/no_replacement_sample4_hist.png")
hist(nrplc_sample4)
dev.off()

#generating boxplots
png("./datos generados (Ejercicio 1)/population_boxplot.png")
dev.new(boxplot(ppln, main = "Boxplot of population"))
dev.off()

png("./datos generados (Ejercicio 1)/replacement_sample1_boxplot.png")
dev.new(boxplot(rplc_sample1, main = "replacement sample 1"))
dev.off()

png("./datos generados (Ejercicio 1)/replacement_sample2_boxplot.png")
dev.new(boxplot(rplc_sample2, main = "replacement sample 2"))
dev.off()

png("./datos generados (Ejercicio 1)/replacement_sample3_boxplot.png")
dev.new(boxplot(rplc_sample3, main = "replacement sample 3"))
dev.off()

png("./datos generados (Ejercicio 1)/replacement_sample4_boxplot.png")
dev.new(boxplot(rplc_sample4, main = "replacement sample 4"))
dev.off()

png("./datos generados (Ejercicio 1)/no_replacement_sample1_boxplot.png")
dev.new(boxplot(nrplc_sample1, main = "no replacement sample 1"))
dev.off()

png("./datos generados (Ejercicio 1)/no_replacement_sample2_boxplot.png")
dev.new(boxplot(nrplc_sample2, main = "no replacement sample 2"))
dev.off()

png("./datos generados (Ejercicio 1)/no_replacement_sample3_boxplot.png")
dev.new(boxplot(nrplc_sample3, main = "no replacement sample 3"))
dev.off()

png("./datos generados (Ejercicio 1)/no_replacement_sample4_boxplot.png")
boxplot(nrplc_sample4, main = "no replacement sample 4")
dev.off()

