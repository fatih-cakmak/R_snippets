# Run in shell command line as "Rscript file_name.R", no  additional packages required
cat("Please enter the number of rows ")
N_rows <- as.numeric(readLines(con="stdin", 1))

# evaluation function
test_polindrom<-function(string_1){
  temp<-strsplit(tolower(string_1),""); temp<-temp[[1]][grep("[a-z]", temp[[1]])]
  rev_n<-length(temp):1; rev_string<-temp[rev_n]
  if(identical(temp,rev_string)) return ("Palindrome") else return("Not Palindrome")
}

# now read and concatenate the string
new_string<-vector()
for(i in 1:N_rows){
  new_string<-c(new_string,as.character(readLines(con="stdin", 1)) )
}
new_string<-paste0(new_string,collapse = "")
test_polindrom(new_string)

