#' Write Html output file for pfam enrichment file
#'
#' @author praveen Baskran \email{spemannpraveen@gmail.com}
#' @description write sortable html file using Pfam enrichment table
#' @description This is a modified version of "writeProbeData" function from "probeAnnotation" package
#' see \link{writeProbeData} for more details and for options
#' @return Save files in  html files
#' @import probeAnnotation
## @source ~/ownCloud/Scripts/writeProbeData_forpfam.R
#' @export

writeProbeDataHTML <- function(x, filename, col.format, id.header="id", col.names=NA, title="Gene List", header=NA, text=NA, indices=TRUE, datestamp=TRUE, css=TRUE, js=TRUE, pad="",weblink_col="gene_id",weblink=""){
  #### if http_link is set to true it create a link to pfam for that domain  using the pfam id on the dim(x) -4 column (i.e pfam_domain column)
  require("probeAnnotation")
  # Check that the column names mentioned in col.format and col.names exist:
  if(!missing(col.format) && any(names(col.format) %in% colnames(x) == FALSE)) stop("invalid column names specified in col.format")
  if(!missing(col.names) && any(names(col.names) %in% colnames(x) == FALSE)) stop("invalid column names specified in col.names")

  output.file <- file(filename, 'w', encoding='utf8')
  on.exit(close(output.file))
  writeLines(probeAnnotation:::html.headers, con=output.file)
  writeLines("<html xmlns=\"http://www.w3.org/1999/xhtml\">", con=output.file)
  writeLines(padLines("<head>", n=1, padding=pad), con=output.file)
  writeLines(padLines(sprintf("<title>%s</title>", title), n=2, padding=pad), con=output.file)
  if(!(is.na(js) || is.null(js) || identical(js, FALSE))){
    if(identical(js, TRUE)){
      writeLines(padLines("<script type=\"text/javascript\">", n=2, padding=pad), con=output.file)
      # writeLines(padLines("<!-- Begin to hide script contents from old browsers.", n=3, padding=pad), con=output.file)
      writeLines(padLines("//<![CDATA[", n=3, padding=pad), con=output.file)
      writeLines(padLines(probeAnnotation:::js.sort, n=3, padding=pad), con=output.file)
      # writeLines(padLines("// End the hiding here. -->", n=3, padding=pad), con=output.file)
      writeLines(padLines("//]]>", n=3, padding=pad), con=output.file)
      writeLines(padLines("</script>", n=2, padding=pad), con=output.file)
    } else writeLines(padLines(sprintf("<script type=\"text/javascript\" src=\"%s\"></script>", js), n=2, padding=pad), con=output.file)
  }
  if(!(is.na(css) || is.null(css) || identical(css, FALSE))){
    if(identical(css, TRUE)){
      writeLines(padLines("<style type=\"text/css\">", n=2, padding=pad), con=output.file)
      writeLines(padLines(probeAnnotation:::default.css, n=3, padding=pad), con=output.file)
      writeLines(padLines("</style>", n=2, padding=pad), con=output.file)
    } else writeLines(padLines(sprintf("<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" title=\"default stylesheet\" />", css), n=2, padding=pad), con=output.file)
  }
  writeLines(padLines("</head>", n=1, padding=pad), con=output.file)
  writeLines(padLines("<body>", n=1, padding=pad), con=output.file)

  # Add the page header, if present:
  if(!is.na(header)) writeLines(padLines(sprintf("<h1>%s</h1>", header), n=2, padding=pad), con=output.file)

  # Add the page text, if present:
  if(!all(is.na(text))) for(line in text) writeLines(padLines(sprintf("<p>%s</p>", line), n=2, padding=pad), con=output.file)

  # Get the column classes:
  col.class <- sapply(1:ncol(x), function(i){class(x[,i])})

  if(nrow(x) != 0){
    # Format the columns, if required:
    if(!missing(col.format)) for(column.name in names(col.format)) x[[column.name]] <- sprintf(col.format[[column.name]], x[[column.name]])

    # Print the table:
    writeLines(padLines("<table id=\"data\" class=\"sortable\">", n=2, padding=pad), con=output.file)

    # Table header row:
    column.header.text <- colnames(x)
    if(identical(is.list(col.names), TRUE)) column.header.text[match(names(col.names), colnames(x))] <- col.names
    writeLines(padLines("<tr class=\"header\">", n=3, padding=pad), con=output.file)
    if(identical(indices, TRUE)) writeLines(padLines("<td class=\"index-header\">index</td>", n=4, padding=pad), con=output.file)
    writeLines(padLines(sprintf("<td class=\"id-header\" title=\"%s\">%s</td>", id.header, id.header), n=4, padding=pad), con=output.file)
    sapply(1:ncol(x), function(i){writeLines(padLines(sprintf("<td class=\"header\">%s</td>", column.header.text[i]), n=4, padding=pad), con=output.file)})
    writeLines(padLines("</tr>", n=3, padding=pad), con=output.file)

    # Table data rows:
    for(i in 1:nrow(x)){
      row.name <- rownames(x)[i]
      writeLines(padLines("<tr>", n=3, padding=pad), con=output.file)
      if(identical(indices, TRUE)) writeLines(padLines(sprintf("<td class=\"numeric index\">%d</td>", i), n=4, padding=pad), con=output.file)
      writeLines(padLines(sprintf("<td class=\"character id\">%s</td>", row.name), n=4, padding=pad), con=output.file)
      for(column in 1:ncol(x)) {
        if((!weblink %in% "FALSE") && (column == which(colnames(x) %in% weblink_col)) ){
            #weblink  <-gsub(pattern = "/$",replacement = "",x = weblink) ## remove / in the end of thelink
            weblink=gsub(pattern = "%s",replacement = x[i, column],weblink)
            #col.class[column] = html class; first %s
            ## second %s == weblink eg (http://http://pfam.xfam.org/family/)
            ## third and fourth %s == gene_id or pfam id.
          writeLines(padLines(sprintf("<td class=\"%s\"><a href=\"%s\">%s</a></td>", col.class[column], weblink ,x[i, column]), n=4, padding=pad), con=output.file)
        }
        else{
          writeLines(padLines(sprintf("<td class=\"%s\">%s</td>", col.class[column], x[i, column]), n=4, padding=pad), con=output.file)
        }
      }
      writeLines(padLines("</tr>", n=3, padding=pad), con=output.file)
    }
    writeLines(padLines("</table>", n=2, padding=pad), con=output.file)
  } else {
    writeLines(padLines("<p class=\"no_data\">(no data to display)</p>", n=2, padding=pad), con=output.file)
  }

  # Add the boilerplate text, if present:
  if(identical(datestamp, TRUE)) {
    writeLines(padLines(sprintf("<p class=\"boilerplate\">%s</p>", format(Sys.time(), "Generated: %b %d, %Y at %H:%M:%S")), n=2, padding=pad), con=output.file)
    writeLines(padLines(sprintf("<p class=\"boilerplate\">%s</p>", format(Sys.time(), "Click the link on pfam_domain column to get more info")), n=2, padding=pad), con=output.file)

  }
  # Finish up the file:
  writeLines(padLines("</body>", n=1, padding=pad), con=output.file)
  writeLines("</html>", con=output.file)

  # Return the processed dataframe:
  return(invisible(x))
}
