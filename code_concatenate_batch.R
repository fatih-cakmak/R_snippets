df1 <- data.frame(name = c("tim", "tim", "tim", "ron"), val = 1:4)
df2 <- data.frame(name = c("tim", "tim", "ron","ron"), val = 1:4)

df1
df2
rbind(df1, df2)
merge(df1, df2, all.x = T, all.y =T)



save.xls <- function(df, filename, sheetname="Sheet", create=TRUE, rownames=NULL, startRow=1, zebra=F) {
  require(XLConnect)
  require(stringr)
  
  if (is.matrix(df)) df <- as.data.frame(df)
  
  if (!str_detect(filename, "\\.xlsx$")) filename <- str_c(filename, ".xlsx")
  
  wb <- loadWorkbook(filename, create=create)
  
  if (existsSheet(wb, sheetname))
    warning(sprintf("Sheet %s already existed and was overwritten", sheetname))
  
  createSheet(wb, name=sheetname)
  if (!is.null(rownames)) df <- transform(df, rownames = row.names(df))
  writeWorksheet(wb, df, startRow=startRow, sheet=sheetname, rownames=rownames)
  
  if (zebra) {
    color <- createCellStyle(wb)
    setFillForegroundColor(color, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
    setFillPattern(color, fill = XLC$FILL.SOLID_FOREGROUND)
    
    for (i in 1:ncol(df)) {
      setCellStyle(wb, sheet = sheetname, row = seq(startRow+1, nrow(df)+2, 2), col = i, 
                   cellstyle = color)
    }
    
    #prcntg <- createCellStyle(wb)  see my script of XLConnect.R for how it worked
    #dollar <- createCellStyle(wb)
    #setDataFormat(prcntg, format = "0.0")
    #setDataFormat(dollar, format = "$ 0.00")
    
    border <- createCellStyle(wb)
    setBorder(border, side = c("bottom","top"), type = XLC$"BORDER.THICK", color = XLC$"COLOR.RED")
    setCellStyle(wb, sheet = "Sheet", row = startRow, col = 1:ncol(df), cellstyle = border)
    setColumnWidth(wb, sheet = "Sheet", column = 1:ncol(df), width = -1) # this autosizes each column
  }
  
  saveWorkbook(wb)
}






dat <- read.table(textConnection( 
  "C1  C2  C3  C4  C5 
  A   B   F   C   Q 
  G   H   I   J   T 
  K   D   R   S   E 
  P   L   M   N   O" 
), header = TRUE) 
closeAllConnections() 

dat$NewCol <- do.call(paste, c(dat[c("C3", "C4")], sep = "")) 
dat 