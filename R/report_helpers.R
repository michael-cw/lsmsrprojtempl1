#' Function to create Word report paragraph element
#'
#' @param bodytext text of paragraph
#' @param text.align alignment of text
#'
#' @return Word report paragraph element
#'
#' @import officer
#' @import ggplot2
#'
#' @export

freestyler <- function(bodytext = "Lorem Ipsium", text.align = "left") {
  ## style
  fp_qte <- officer::fp_text(color = "#546e7a", font.size = 12, bold = F)
  fp_qte_sty <- officer::fp_par(text.align = text.align, padding.bottom = 2, keep_with_next = T)
  ## text
  qte <- ftext(bodytext, fp_qte)
  ## block list
  out <- block_list(
    fpar(qte,
         fp_p = fp_qte_sty,
         run_linebreak()
    )
  )
  return(out)
}

#' Function to create Word report title element
#'
#'
#' @param qTitle title of report
#'
#' @return Word report title element
#'
#' @export

report_title<-function(qTitle) {
  # report title
  # qTitle <- paste(dots$report_name)
  ## Fonts title
  fp_title <- officer::fp_text(color = "#546e7a", font.size = 20, bold = T)
  fp_title_sty <- officer::fp_par(
    text.align = "center", padding.bottom = 1,
    border.bottom = officer::fp_border(color = "black")
  )
  fp_stitle <- officer::fp_text(color = "#546e7a", font.size = 14, bold = F)
  fp_stitle_sty <- officer::fp_par(text.align = "center", padding.bottom = 0)
  ##
  doc_title <- officer::block_list(
    officer::fpar(
      officer::ftext(qTitle, prop = fp_title),
      officer::run_linebreak(), officer::run_linebreak(),
      fp_p = officer::fp_par(text.align = "center")
    ),
    officer::fpar(officer::ftext("DECLS REPORT", prop = fp_stitle), fp_p = fp_stitle_sty),
    officer::fpar(
      officer::ftext(as.character(Sys.Date()), prop = fp_stitle),
      officer::run_linebreak(), officer::run_linebreak(),
      officer::run_linebreak(), officer::run_linebreak(),
      fp_p = fp_stitle_sty
    ),
    officer::fpar(
      officer::external_img(src = file.path(".", "templates", "word", "logoWBDG.png"), height = 0.82, width = 2.9),
      fp_p = officer::fp_par(text.align = "center", padding.top = 5)
    )
  )
}

#' Function to create Word report
#'
#' @param rep_cont list of report content, see details for the structure of the required list
#' @param template path to template
#' @param title title of report
#' @param creator creator of report
#' @param target path to target
#'
#' @details The list rep_cont must contain the following elements:
#' \itemize{
#'  \item doc_title: title of report
#'  \item sec_title: list of section titles
#'  \item sec_para: list of section paragraphs
#'  \item sec_table: list of section tables
#'  \item sec_graph: list of section graphs
#'  }
#'  sec_para, sec_table and sec_graph subsequently require a list of paragraphs, tables and graphs respectively,
#'  named para1, para2, para3, etc.
#'
#' @return Word report in target path
#'
#'
#' @import officer
#' @import ggplot2
#'
#' @export
genreport<-function(rep_cont = NULL,
                    template = file.path(".", "templates", "word", "report_template_DECLS_v2.docx"),
                    title = "Data Preparation Report",
                    creator = "Michael Wild",
                    target = "./DataPrepReportYemen.docx") {

  doc.full<-read_docx(template) %>%
    set_doc_properties(title = title,
                       creator = creator,
                       created = Sys.time())




  ## 2.2 DOC TITLE & DATE
  doc.full <- doc.full  %>%
    body_add(rep_cont$doc_title) %>%
    body_add_break()
  ## 2.2.2. TOC
  doc.full <- doc.full %>%
    body_add_par("Table of Contents", style = "heading 4") %>%
    body_add_toc(level = 2)%>%
    body_add_break()
    # body_add_par("Figures", style = "heading 4") %>%
    # body_add_toc(style = "Image Caption")%>%
    # body_add_break() %>%
    # body_add_par("Tables", style = "heading 4") %>%
    # body_add_toc(style = "Table Caption")%>%
    # body_add_break()

  ## 2.3. Add Section Para and Tables
  ##    i. loop over section
  ##        ii. loop over para
  for (sec_para in names(rep_cont$sec_para)) {
    #incProgress(0.2)
    p<-rep_cont$sec_para[[sec_para]]
    t<-rep_cont$sec_table[[sec_para]]
    i<-rep_cont$sec_graph[[sec_para]]
    sectitle<-rep_cont$sec_title[[sec_para]]
    doc.full <- doc.full  %>%
      body_add_par(sectitle, style = "heading 2") %>%
      body_add_par(NULL, style = "Normal")
    for (para in names(p)) {
      pp<-p[[para]]
      tt<-t[[para]]
      ii<-i[[para]]
      ## add para
      if (!is.null(pp)) {
        doc.full <- doc.full  %>%
          body_add(pp, style = "Normal")
      }
      ## add table
      if (!is.null(tt)) {
        doc.full<-doc.full %>%
          body_add_table(tt$table,
                         style = "Grid Table 6 Colorful Accent 2",
                         header = T) %>%
          body_add_caption(
            block_caption(
              tt$caption,
              style = "caption",
              autonum = run_autonum(seq_id = "table", pre_label = "Table ")
            )
          ) %>%
          #body_add_par(NULL, style = "Normal") %>%
          body_add_par(NULL, style = "Normal")%>%
          body_add_break()

      }
      ## add graph
      if (!is.null(ii)){
        doc.full<-doc.full %>%
          body_add_gg(ii$plot, style = "Figure") %>%
          #body_add_par(NULL, style = "Normal") %>%
          body_add_caption(
            block_caption(
              ii$caption,
              style = "caption",
              autonum = run_autonum(seq_id = "fig", pre_label = "Figure ")
            )
          ) %>%
          body_add_par(NULL, style = "Normal") %>%
          body_add_break()
      }

    }
    ## page break after each section
    doc.full<-doc.full %>%
      body_add_break()
  }


  doc.full %>%
    print(target = target)

}







