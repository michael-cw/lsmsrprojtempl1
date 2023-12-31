# to be used in RStudio "new project" GUI
create_proj_report_gui <- function(path, ...) {

  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # create data directory
  dir.create(file.path(path, "data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "data", "in"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "data", "out"), recursive = TRUE, showWarnings = FALSE)

  # create templates directory
  dir.create(file.path(path, "templates"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "templates", "word"), recursive = TRUE, showWarnings = FALSE)

  # copy system files to templates directory
  file.copy(
    from = system.file("office/word", "report_template_DECLS_v2.docx", package = "lsmsrprojtempl1"),
    to = file.path(path, "templates", "word", "report_template_DECLS_v2.docx")
  )
  file.copy(
    from = system.file("office/img", "logoWBDG.png", package = "lsmsrprojtempl1"),
    to = file.path(path, "templates", "word", "logoWBDG.png")
  )

  # create text input directory
  dir.create(file.path(path, "text"), recursive = TRUE, showWarnings = FALSE)

  # create R files directory
  dir.create(file.path(path, "R"), recursive = TRUE, showWarnings = FALSE)

  # create doc files directory
  dir.create(file.path(path, "doc"), recursive = TRUE, showWarnings = FALSE)

  # generate header
  header <- c(
    "# Data processing file for the creation of LSMS style Word document for technical reports.",
    "# This file was generated by the lsmsrprojtempl1 R package.",
    "#",
    "#",
    "# The following elements have been added to the top of the file, modify if required:",
    "#",
    "#     - packages ",
    "#     - report set-up",
    "#",
    "# During the project set-up process, the required report templates had been copied to the /templates directory.",
    "# In the /text directory, you will find some example text input files.",
    ""
  )

# collect inputs
  dots <- list(...)

# packages
  text<-trimws(unlist(strsplit(dots$packages,split=",")))
  text<-sprintf("library(%s)",text)
  text<-c("# 1. LOAD PACKAGES", text)

# officer elements
# i. title
  report_name <- dots$report_name
  text <- c(text, "\n", "# 2. REPORT SETUP", "# 2.1. Report title", sprintf('\tdoc_title<-report_title("%s")', report_name))
# ii. content
  cont<-paste('
# Input list for report generation (Modify as required)

  rep_cont<-list(
  doc_title = doc_title,
  sec_title = list(
    sec1 = "Section 1",
    sec2 = "Section 2",
    sec3 = "Section 3"
  ),
  sec_para = list(
    sec1 = list(
      para1 = freestyler(stringr::str_squish(readLines("./text/sec1_para1.txt"))),
      para2 = freestyler(stringr::str_squish(readLines("./text/sec1_para2.txt")))
    ),
    sec2 = list(
      para1 = freestyler(stringr::str_squish(readLines("./text/sec2_para1.txt")))
    ),
    sec3 = list(
      para1 = freestyler(stringr::str_squish(readLines("./text/sec3_para1.txt"))),
      para2 = freestyler(stringr::str_squish(readLines("./text/sec3_para2.txt"))),
      para3 = freestyler(stringr::str_squish(readLines("./text/sec3_para3.txt")))
    )
  ),
  sec_table = list(
    sec1 = NULL,
    sec2 = NULL,
    sec3 = NULL
    ),
  sec_graph = list(
    sec1 = NULL,
    sec2 = NULL,
    sec3 = NULL
  )
)
        ')
text<-c(text,"\n", "# 2.1. Report content", cont)

# iii. report generation
cont<-sprintf('
  genreport(
  rep_cont = rep_cont,
  creator = "%s",
  template = "./templates/word/report_template_DECLS_v2.docx",
  title = "%s",
  target = "./doc/%s")
        ', dots$creator, report_name, dots$target
              )
  text<-c(text,"\n", "# 2.3. Generate report", cont)



# collect into single text string
contents <- paste(
  paste(header, collapse = "\n"),
  paste(text, collapse = "\n"),
  sep = "\n"
)

# write to data_preparation file
writeLines(contents, con = file.path(path, "R", "report_data_preparation.R"))

# write text input files
writeLines("This is the first paragraph of section 1", con = file.path(path, "text", "sec1_para1.txt"))
writeLines("This is the second paragraph of section 1", con = file.path(path, "text", "sec1_para2.txt"))
writeLines("This is the first paragraph of section 2", con = file.path(path, "text", "sec2_para1.txt"))
writeLines("This is the first paragraph of section 3", con = file.path(path, "text", "sec3_para1.txt"))
writeLines("This is the second paragraph of section 3", con = file.path(path, "text", "sec3_para2.txt"))
writeLines("This is the third paragraph of section 3", con = file.path(path, "text", "sec3_para3.txt"))

# write .Rprofile file
write_rprofile <- function(path) {
  # Define the path to the .Rprofile file in the current working directory
  rprofile_path <- file.path(path, ".Rprofile")

  # Define the line to write to the .Rprofile file
  line_to_write <- 'library("lsmsrprojtempl1")'

  # Write the line to the .Rprofile file
  write(line_to_write, rprofile_path)
}
write_rprofile(path)

}
