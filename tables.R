wilson_interval<-function(k,n,alpha=0.05){
    p<-k/n
    z<-qnorm(p=1-alpha/2)
    return(
        list(
            p=100*p,
            ci=100*((p+z^2/2/n)/(1+z^2/n)+c(-1,1)*(z/(1+z^2/n))*sqrt((p*(1-p))/n+(z^2)/(4*n^2)))
        )
    )
}

sero <- read.csv(
    file="klena_4-16-2020.csv",
    as.is=TRUE,
    na.strings="N/A"
)

sero$agegroup <- factor(
    x=sero$agegroup,
    levels=c(
        "5-17",
        "18-49",
        "50-64",
        "65-79",
        "80+"
    )
)

WA_row_index <- which(
(sero$state == "WA") &
(sero$results == "REACTIVE" | sero$results == "NON-REACTIVE")
)

NY_row_index <- which(
(sero$state == "NY" | sero$state == "NJ" ) &
(sero$results == "REACTIVE" | sero$results == "NON-REACTIVE")
)

sink("./table_stats.text")
print(
    table(
        sero$result,
        sero$state
    )
)
cat("\n\n\n\n")
for(k in 1:2){
    row_index<-list(
        WA_row_index,
        NY_row_index
    )[[k]]
    cat(c("WA","NY")[k])
    cat("\n\n")
    age_results<-table(
        sero[row_index,"agegroup"],
        sero[row_index,"results"]
    )
    age_results<-cbind(
        age_results,
        TOTAL=rowSums(age_results)
    )
    age_results<-rbind(
        age_results,
        allages=colSums(age_results)
    )
    age_results<-cbind(
        age_results,
        sero=rep(NA,nrow(age_results)),
        ci_lb=rep(NA,nrow(age_results)),
        ci_ub=rep(NA,nrow(age_results))
    )
    for(i in 1:nrow(age_results)){
        age_results[
            i,
            c("sero","ci_lb","ci_ub")
        ] <- round(
            unlist(
                x=wilson_interval(
                    k=age_results[i,"REACTIVE"],
                    n=age_results[i,"TOTAL"],
                    alpha=0.05
                )
            ),
            digits=2
        )
    }
    print(age_results)
    cat("\n\n")
}
sink()
