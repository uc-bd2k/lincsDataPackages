esetToGct1.3 <- function(eset, gctFile=NA, gctDir) {
	require(Biobase)
#	assign(esetName, get(load(eset)))
	edata <- exprs(eset)
	fdata <- fData(eset)
	pdata <- pData(eset)

	fdataFrame <- as.data.frame(matrix("na", nrow=dim(pdata)[2], ncol=dim(fdata)[2]), stringsAsFactors=FALSE)
	names(fdataFrame) <- names(fdata)
	fdataFrame <- rbind(fdataFrame, fdata)
	df <- cbind(fdataFrame, rbind(t(pdata), edata), stringsAsFactors=FALSE)
	df <- rbind(colnames(df), df)
	df <- cbind(c("id", colnames(pdata), rownames(fdata)), df, stringsAsFactors=FALSE)
	colnames(df) <- NULL
	rownames(df) <- NULL

	gctDir <- sub("/?$", "", gctDir)
	gctDir <- sub("$", "/", gctDir)
	if (is.na(gctFile)) {
#		gctFile <- paste(gctDir, sub("_*eset\\.RData$", "\\.gct", deparse(substitute(esetName))), sep="")
		print(deparse(substitute(eset)))
		gctFile <- paste(gctDir, deparse(substitute(eset)), ".gct", sep="")
#		print(gctFile)
	} else {
		gctFile <- paste(gctDir, gctFile, sep="")
	}
	write(paste("#1.3\n", dim(edata)[1], "\t", dim(edata)[2], "\t", dim(fdata)[2], "\t", dim(pdata)[2], sep=""), file=gctFile)
	write.table(df, file=gctFile, sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
}

