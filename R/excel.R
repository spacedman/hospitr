
writePredictionXL <- function(efit, fp){
    require(xlsx)
    wb = createWorkbook(type="xlsx")

    cs <- CellStyle(wb, font=Font(wb, isBold=TRUE), alignment=Alignment(h="ALIGN_RIGHT"))
    
    medical = createSheet(wb, "Medical")
    m = sectionTable(efit, "Medical")
    addDataFrame(m, medical, row.names=FALSE, colnamesStyle=cs)
    for(i in 1:ncol(m)){autoSizeColumn(medical, i)}
    
    surgical = createSheet(wb, "Surgical")
    s = sectionTable(efit, "Surgical")
    
    addDataFrame(s, surgical, row.names=FALSE, colnamesStyle=cs)
    for(i in 1:ncol(s)){autoSizeColumn(surgical, i)}

    info = createSheet(wb,"Info")
    fields = data.frame(matrix(
        c(
            "Created", format(Sys.time(), "%a %b %d %H:%M:%S %Y"),
            "By","Barry Rowlingson"
            ),
        ncol=2,byrow=TRUE)
        )
    
    addDataFrame(fields, info, col.names=FALSE, row.names=FALSE, colStyle=list("1"=cs))
    autoSizeColumn(info,1)
    autoSizeColumn(info,2)
    
    saveWorkbook(wb, fp)
    
}

