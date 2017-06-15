#==============================
#The OCR_DOCS() function.  Original version By D. Graham Andrews and Ryan Scott, first finished 5/16/2017
#==============================
#' @Title \code{OCR_DOCS} will read a given folder for all PDFs in it
#' @description This function uses Google's "parsey mcparseface" neural network to read PDFs and output machine readable text files of the content.
#' @param path: A path to a directory containing mixed pdf/other documents.  Runs OCR on all the PDF documents. (Does not detect which PDFs have existing text.)
#' @returns output path: creates text files in the local working directory with the same names as the PDFs to be OCR'd.
#' @examples
#' text_files_location <- OCR_DOCS(path_to_pdf_files)
#' @export
#TODO: remove all "\u00" 4 digit number fragments.
#TODO: remove files after copy


OCR_DOCS<-function(path){
#save the working directory
oldWD <- getwd()
#change working directory
setwd(path)
#build list of all PDFs at "path" and put that list in "listOfPDFs"
listOfPDFs <- list.files(path,"\\.pdf")
 
outputPath <- toString(paste(path,"OCR_Results/",sep=""))
dir.create(outputPath)

	for(eachPDFpath in listOfPDFs){
		pdfInfo<-pdftools::pdf_info(eachPDFpath)
		numPages<-pdfInfo$pages
		fileName<-eachPDFpath
		if(.Platform$OS.type == "windows"){
			fileName<-strsplit(eachPDFpath, "/") #if we have a full path, cut it into chunks based on the /
			fileName<-fileName[length(fileName)] #set the filename to the last chunk of the path
		}
		else{print("Warning: OS is not windows, so remember that OCR_DOCs() function must be called with filenames and not paths on other OS's.")}
		
		#prep the output filename (remove the ".pdf" so as not to confuse later parts of texttools
		outputFileName <- gsub(".pdf", "", fileName)
		#prep a file connection to output the raw text
		outputFile <- file(description=paste("rawText_",toString(outputFileName),".txt",sep=""), open="a", blocking=FALSE, encoding="", raw=FALSE, method="internal")

		tout<-lapply(1:numPages,function(PageNumber){
		
			#make a new temporary png image file
			tmpf<-tempfile("page", fileext = c(".png"))
		
			#Render a page and save it to a new png file
			print(paste("Now rendering page ", PageNumber, " of ", numPages, " in  document ", fileName, ".", sep=""))
			pdftools::pdf_render_page(toString(fileName),page=PageNumber,dpi=300, numeric=TRUE) %>% png::writePNG(tmpf)
			
			#run the OCR and then discard the temp file
			raw_text<-tesseract::ocr(tmpf,engine=tesseract("eng"))
			unlink(tmpf)
	
			#save the raw text
		
			write(raw_text,outputFile)		
		}) #this is the end of the lapply function definition, so a ) looks odd here but belongs
	close(outputFile)
	}
#Move all files except the PDFs to a new folder
listOfFiles <- setdiff(list.files(path),listOfPDFs)
file.copy(file.path(path, listOfFiles),outputPath)

#return the working directory
setwd(oldWD)

return(outputPath)
}
