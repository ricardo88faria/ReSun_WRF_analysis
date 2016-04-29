inclRmd <- function(path, r_env = parent.frame()) {
  paste(readLines(path, warn = FALSE), collapse = '\n') %>%
    knitr::knit2html(text = ., fragment.only = TRUE, envir = r_env,  options = "",
                     stylesheet = "") %>%
    gsub("&lt;!--/html_preserve--&gt;","",.) %>%  ## knitr adds this
    gsub("&lt;!--html_preserve--&gt;","",.) %>%   ## knitr adds this
    HTML
}

about_tab <- tabPanel("Relatório",theme="http://bootswatch.com/spacelab/bootstrap.css", inverse=F,
                  div(
                    #position = "fixed-top",
                    tags$head(includeCSS("www/styles.css")),
                      #HTML(markdown::markdownToHTML(knit('static_doc.Rmd', quiet = TRUE))))
                    #wellPanel(inclRmd("static_doc.Rmd"))
                  #includeMarkdown("static_doc.Rmd")
                  includeHTML("static_doc.html")
                  #inclRmd("static_doc.Rmd")
                  #uiOutput("page1")
                  )
)
