library(abind)
##Need this to compute 95% confidence intervals
wilson_interval <- function(k, n, alpha = 0.05){
    p <- k/n
    z <- qnorm(p = 1-alpha/2)
    return(
        list(
            p = 100*p,
            ci = 100*((p+z^2/2/n)/(1+z^2/n)+c(-1, 1)*(z/(1+z^2/n))*sqrt((p*(1-p))/n+(z^2)/(4*n^2)))
        )
    )
}

##Read the latest version of the data in
sero <- read.csv(
    file = "labresults_dedup_4-20-2020_AM.csv",
    as.is = TRUE,
    na.strings = "N/A"
)

##Use age groups as a factor
sero$agegroup <- factor(
    x = sero$agegroup,
    levels = c(
        "5-17",
        "18-49",
        "50-64",
        "65-79",
        "80+"
    ),
    labels = c(
        "5-17",
        "18-49",
        "50-64",
        "65+",
        "65+"
    )
)
##Use sex as factor
sero$sex <- factor(
    x = sero$sex,
    levels = c(
        "Male",
        "Female",
        "Unknown"
    )
)

##Find the observations which we want to make crosstabs for
WA_row_index <- which(
sero$state == "WA" &
(sero$results == "REACTIVE" | sero$results == "NON-REACTIVE")
)
NY_row_index <- which(
sero$state == "NY" &
(sero$results == "REACTIVE" | sero$results == "NON-REACTIVE")
)

##Start a CSV for appending our results
write.table(
    x = table(
        sero$result,
        sero$state
    ),
    file = "./tabular_stats.csv",
    append = FALSE,
    sep = ",",
    row.names = TRUE,
    col.names = NA
)
##Loop over the tabulation, once for each catchment area
for(k in 1:2){
    row_index <- list(
        WA_row_index,
        NY_row_index
    )[[k]]
    ##Write a header to see which tables belong to which catchment areas
    write.table(
        x = matrix(
            data = c(
                rep("", 3),
                c("WA", "NY")[k],
                rep("", 3)
            ),
            ncol = 1
        ),
        file = "./tabular_stats.csv",
        append = TRUE,
        col.names = FALSE,
        row.names = FALSE
    )
    ##Iterate over variables / columns to tabulate results
    for(this_column in c("agegroup", "sex")){
        ##Find the counts
        tbl_results <- table(
            sero[row_index, this_column],
            sero[row_index, "results"]
        )
        ##Find the marginals
        tbl_results <- cbind(
            tbl_results,
            TOTAL = rowSums(tbl_results)
        )
        tbl_results <- rbind(
            tbl_results,
            TOTAL = colSums(tbl_results)
        )
        ##Find the seroprevalence and 95% confidence intervals
        tbl_results <- cbind(
            tbl_results,
            sero = rep(
                NA,
                nrow(tbl_results)
            ),
            ci_lb = rep(
                NA,
                nrow(tbl_results)
            ),
            ci_ub = rep(
                NA,
                nrow(tbl_results)
            )
        )
        for(i in 1:nrow(tbl_results)){
            tbl_results[
                i,
                c(
                    "sero",
                    "ci_lb",
                    "ci_ub"
                )
            ] <- round(
                unlist(
                    x = wilson_interval(
                        k = tbl_results[i, "REACTIVE"],
                        n = tbl_results[i, "TOTAL"],
                        alpha = 0.05
                    )
                ),
                digits = 2
            )
        }
        ##Make the results in the format for a presentation
        tbl_results_csv <- cbind(
            tbl_results,
            ci = paste0(
                "(",
                tbl_results[, "ci_lb"],
                ", ",
                tbl_results[, "ci_ub"],
                ")"
            )
        )
        ##Write the table to our CSV
        write.table(
            x = tbl_results_csv,
            file = "./tabular_stats.csv",
            append = TRUE,
            sep = ",",
            row.names = TRUE,
            col.names = NA
        )
        ##Add some white space in our CSV
        write.table(
            x = matrix(
                data = rep("", 2),
                ncol = 1
            ),
            file = "./tabular_stats.csv",
            append = TRUE,
            col.names = FALSE,
            row.names = FALSE
        )
    }
    ##Make tables of age group by sex
    for(sex in c("Male","Female")){
        write.table(
            x = matrix(
                data = c("", sex),
                ncol = 1
            ),
            file = "./tabular_stats.csv",
            append = TRUE,
            col.names = FALSE,
            row.names = FALSE
        )

        ##Make a subindex of rows
        row_sub_index <- which(
            sero[row_index , "sex"] == sex
        )
        ##Find the counts
        tbl_results <- table(
            sero[row_index[row_sub_index], "agegroup"],
            sero[row_index[row_sub_index], "results"]
        )
        ##Find the marginals
        tbl_results <- cbind(
            tbl_results,
            TOTAL = rowSums(tbl_results)
        )
        tbl_results <- rbind(
            tbl_results,
            TOTAL = colSums(tbl_results)
        )
        ##Find the seroprevalence and 95% confidence intervals
        tbl_results <- cbind(
            tbl_results,
            sero = rep(
                NA,
                nrow(tbl_results)
            ),
            ci_lb = rep(
                NA,
                nrow(tbl_results)
            ),
            ci_ub = rep(
                NA,
                nrow(tbl_results)
            )
        )
        for(i in 1:nrow(tbl_results)){
            tbl_results[
                i,
                c(
                    "sero",
                    "ci_lb",
                    "ci_ub"
                )
            ] <- round(
                unlist(
                    x = wilson_interval(
                        k = tbl_results[i, "REACTIVE"],
                        n = tbl_results[i, "TOTAL"],
                        alpha = 0.05
                    )
                ),
                digits = 2
            )
        }
        ##Make the results in the format for a presentation
        tbl_results_csv <- cbind(
            tbl_results,
            ci = paste0(
                "(",
                tbl_results[, "ci_lb"],
                ", ",
                tbl_results[, "ci_ub"],
                ")"
            )
        )
        ##Write the table to our CSV
        write.table(
            x = tbl_results_csv,
            file = "./tabular_stats.csv",
            append = TRUE,
            sep = ",",
            row.names = TRUE,
            col.names = NA
        )
        ##Add some white space in our CSV
        write.table(
            x = matrix(
                data = rep("", 2),
                ncol = 1
            ),
            file = "./tabular_stats.csv",
            append = TRUE,
            col.names = FALSE,
            row.names = FALSE
        )
    }
}


##Make plots and iterate over catchment areas
for(n in 1:2){
    if(n == 1){
        png(
            filename = "Washington.png",
            width = 1000,
            height = 1000
        )
        tbl_results <- table(
            sero[WA_row_index, "results"],
            sero[WA_row_index, "agegroup"]
        )
        y1.max <- 450
        y2.max <- 25
        ticks1 <- 10
        ticks2 <- 6
    }
    if(n == 2){
        png(
            filename = "New_York.png",
            width = 1000,
            height = 1000
        )
        tbl_results <- table(
            sero[NY_row_index, "results"],
            sero[NY_row_index, "agegroup"]
        )
        y1.max <- 450
        y2.max <- 65
        ticks1 <- 10
        ticks2 <- 5
    }
    ci<-matrix(
        nrow = ncol(tbl_results),
        ncol = 2
    )
    for(i in 1:nrow(ci)){
        ci[i,] <- wilson_interval(
            k = tbl_results["REACTIVE",i],
            n = colSums(tbl_results)[i]
        )[["ci"]]
    }
    this.cex <- 2
    par(
        mar = c(5, 4, 4, 4),
        oma = c(4, 0, 0, 0),
        cex = this.cex,
        lwd = 2,
        xpd = NA
    )
    bp <- barplot(
        tbl_results/y1.max,
        col = c("blue", "orange"),
        ann = FALSE,
        axes = FALSE,
        ylim = c(0, 1),
        border = NA
    )
    points(
        x = bp,
        y = 100*tbl_results["REACTIVE", ] /
            colSums(tbl_results)/y2.max,
        pch = 16,
        cex = 2
    )
    for(i in 1:nrow(ci)){
        segments(
            x0 = bp[i],
            y0 = ci[i,1]/y2.max,
            y1 = ci[i,2]/y2.max
        )
        segments(
            x0 = bp[i] - 0.1,
            x1 = bp[i] + 0.1,
            y0 = ci[i,1]/y2.max
        )
        segments(
            x0 = bp[i] - 0.1,
            x1 = bp[i] + 0.1,
            y0 = ci[i,2]/y2.max
        )
    }
    axis(
        2,
        at = seq(0, 1, length.out = ticks1),
        labels = seq(0, y1.max, length.out = ticks1)
    )
    axis(
        4,
        at = seq(0, 1, length.out = ticks2),
        labels = seq(0, y2.max, length.out = ticks2)
    )
    mtext(
        text = "Age Group",
        side = 1,
        line = 3,
        cex = this.cex
    )
    mtext(
        text = "Specimens Tested",
        side = 2,
        line = 3,
        cex = this.cex
    )
    mtext(
        text = "Percent Reactive",
        side = 4,
        line = 3,
        cex = this.cex
    )
    legend(
        x = bp[1],
        y = -0.3,
        legend = c("Non-reactive", "Reactive", "% Reactive"),
        col = c("blue", "orange", "black"),
        pch = c(15, 15, 16),
        pt.cex = c(3, 3, 2),
        ncol = 3,
        xpd = NA
    )
    dev.off()
}
