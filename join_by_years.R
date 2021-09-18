#Here we row-bind the yearly files.
library(foreach)
library(data.table)

vax <- foreach(i=c(1990:2021), .combine=rbind) %do% {
    fread(paste0(i,"VAERSVAX.csv"))
}

data <- foreach(i=c(1990:2021), .combine=rbind) %do% {
    fread(paste0(i,"VAERSDATA.csv"))
}

symptoms <- foreach(i=c(1990:2021), .combine=rbind) %do% {
    fread(paste0(i,"VAERSSYMPTOMS.csv"))
}


f.vax <- fread("NonDomesticVAERSVAX.csv")
f.data <- fread("NonDomesticVAERSDATA.csv")
f.symptoms <- fread("NonDomesticVAERSSYMPTOMS.csv")

save(vax, data, symptoms, f.vax, f.data, f.symptoms, file="raw_vaers.RData")
