gct1.3ToEset <- function(gctFile, esetName, esetDir) {
	cat("begin reading gct file:", date(), "\n")
	require(data.table)
	require(Biobase)

	header <- scan(file=gctFile, nlines=2, what="character", quiet=TRUE)
	version <- header[1]
	nrowannot <- as.integer(header[4])
	ncolannot <- as.integer(header[5])

	if (version != "#1.3") {
		return("Not GCT version 1.3\n")
	} else if (length(header) != 5) {
		return("Missing/incorrect header information\n")
	} 

	ds <- fread(input=gctFile, skip=2, header=FALSE, data.table=FALSE)

	## Pheno data
	pdata <- as.data.frame(t(ds[1:(ncolannot+1), ]), stringsAsFactors=FALSE)
	colids <- pdata[1, -1]
	rowids <- pdata[-(1:(nrowannot+1)), 1]
	pdata <- pdata[-(1:(nrowannot+1)), -1]
	colnames(pdata) <- colids
	rownames(pdata) <- rowids

	## Feature data
	fdata <- ds[-((1+1):(ncolannot+1)), 1:(nrowannot+1)]
	colids <- fdata[1, -1]
	rowids <- fdata[-1, 1]
	fdata <- fdata[-1, -1]
	rownames(fdata) <- rowids
	colnames(fdata) <- colids

	## Experiment / assay data
	edata <- ds[-((1+1):(ncolannot+1)), -((1+1):(nrowannot+1))]
	rowids <- edata[-1, 1]
	colids <- edata[1, -1]
	edata <- edata[-1, -1]
	edata.dim <- dim(edata)
	
	## Free some memory
	rm(ds)
	gc()

	edata <- as.matrix(edata)
	edata <- as.numeric(edata)
	dim(edata) <- edata.dim
	colnames(edata) <- colids
	rownames(edata) <- rowids

	## Create eset
	pheno <- new("AnnotatedDataFrame", data=pdata)
	eset <- new("ExpressionSet", exprs=edata, phenoData=pheno)
	featureData(eset) <- as(fdata, "AnnotatedDataFrame")
	assign(esetName, eset)
	save(list=(esetName), file=paste(esetDir, esetName, ".RData", sep=''))
	cat("saved eset file:", date(), "\n")
}


