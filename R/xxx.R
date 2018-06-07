#.onAttach <- function(...) {
 #   cat("message from .onAttach via cat\n")
 #   message("message from .onAttach via message")
 #   packageStartupMessage("message from .onAttach via packageStartupMessage\n")
#}
.onLoad <- function(...) {

    packageStartupMessage("Loading Function:::\n myMiscFunc::Chromosomecoverageplot\n myMiscFunc::create_gene_expressiontable\n myMiscFunc::multi_greplength\n myMiscFunc::mgsub\n myMiscFunc::Write_Multifiles\n")
    packageStartupMessage(" myMiscFunc:::pkgTest\n myMiscFunc:::read_excel_allsheets\nmyMiscFunc::::donuts_plot\n")
     }
