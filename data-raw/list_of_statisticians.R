library("rvest")
library("magrittr")

# Download table from wiki
d = html_session("https://en.wikipedia.org/wiki/List_of_statisticians")

# Extract
stats = d %>% html_nodes("#mw-content-text ul li a") %>% html_text()

# Remove non-statistician nodes
stats = stats[-c(1:38,661:685)]

# Unlist
db = matrix(unlist(strsplit(stats,",")), ncol = 2, byrow = T)

# Remove punctuation

# R way
# gsub("([[:punct:]]|[[:blank:]])","", db)
db[,1] = stri_replace_all_fixed(db[,1], "\\.", "")

# Use stringi
db[,1] = stri_trim(db[,1])
db[,2] = stri_trim(db[,2])


# Remove statisticians with tildas in first name
# iconv() is too platform dependent...
# grep("dat2", iconv(db[,2], "latin1", "ASCII", sub="dat2"))
db[,1] = stri_trans_general(db[, 1], "Latin-ASCII")
db[,2] = stri_trans_general(db[, 2], "Latin-ASCII")

#
db_stats = as.data.frame(db[-dat3,])
colnames(db_stats) = c("Last", "First")

# Save a copy as a csv in data-raw
write.csv(db_stats, file = "data-raw/statisticians.csv", row.names = F)

# Save list
devtools::use_data(db_stats, internal = TRUE)


