library(data.table)
setwd("")
hs96 <- fread("data/year_origin_hs96_6.tsv",sep="\t", header=TRUE,
              colClasses=c(
                    hs96="character",
                    export_val="numeric",
                    import_val="numeric"),
)

str(hs96)
setkey(hs96,origin,year)
hs96[,export_val:= as.numeric(export_val)]
hs96[,import_val:= as.numeric(import_val)]
hs96.med <- hs96[hs96 %like% "3006.",lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
med <- hs96.med[hs96 %like% "3006.",
                .(import= sum(import_val)/1e6, export= sum(export_val)/1e6, balance= (sum(export_val)-sum(import_val))/1e6 ),
                by = list(origin,year)]
med[,surplus:= 1-(import/export)]
med[,trade.vol:= export+import]

med[,trade.adj := trade.vol*exp(surplus)]
save(med, file = "Medicine.Rdata")


p1 <- fread("2000-2008_medals.csv" ,header=TRUE)
str(p1)
p1 <- dcast(p1, Edition + NOC + Event + Event_gender ~ Medal)
l1 <- p1[,.(Gold= sum(Gold), Silver= sum(Silver), Bronze= sum(Bronze)),by=list(Edition,NOC)][,Total:= Gold + Silver + Bronze]

l1[Edition==2000& NOC=='USA']
l1[Edition==2000& NOC=='CHN']
write.csv(l1,file = "l1.csv")


countryname <- fread("country_ico.csv",head=TRUE)
save(countryname, file = "IOC.Rdata")
allmedals <- fread("allmedals.csv",head=TRUE)
save(allmedals, file = "allmedals.Rdata")
