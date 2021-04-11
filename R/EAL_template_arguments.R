#' Create a list of EML func inputs
#'
#' @description  
#'     Initialize the full list of possible inputs to \code{make_eml()}
#'     functions (i.e. function arguments, metadata templates, and data 
#'     objects). This enables bridging of upstream metadata and data sources 
#'     (e.g. metabases, databases, Excel files, web forms, etc.).
#'
#' @param path 
#'     (character) Path to the directory containing \code{make_eml()} 
#'     metadata templates.
#' @param data.path
#'     (character) Path to the directory containing \code{data.table} and 
#'     \code{other.entity} data objects.
#' @param data.table
#'     (character) File names of data tables. If more than one, then supply as 
#'     a vector (e.g. \code{data.table = c('decomp.csv', 'nitrogen.csv')}).
#' @param other.entity
#'     (character) File names of other entity data objects. If more than one, 
#'     then supply as a vector (e.g. 
#'     \code{other.entity = c('ancillary_data.zip', 'processing_and_analysis.R')}).
#' @param sep
#'     (character) Data table field delimiter. Use this if this function fails
#'     to read your \code{data.table}.
#' @param empty
#'     (logical) Initialize the output with a set of empty metadata templates?
#'     This option is useful when wanting to transfer metadata directly into
#'     the template files without having to call the templating functions.
#'     
#' @details 
#'     Character encoding in tabular metadata templates is converted to UTF-8 
#'     via \code{enc2utf8()}. Characters in TextType metadata templates are not
#'     yet converted. Note: This may lead to an inaccuracy and disconnect 
#'     between values in the data objects and what is reported in the EML (e.g.
#'     a categorical variable listed in the EML may not be the same as it's 
#'     corresponding value in the data object). To reduce the chance of this,
#'     warnings are issued if the input data object from which the metadata was
#'     extracted is not UTF-8 encoded.
#'
#' @return 
#'     (named list) A list of all \code{make_eml()} arguments, specified 
#'     metadata templates and data objects.
#'     
EAL_template_arguments <- function(
  path = NULL, 
  data.path = NULL, 
  data.table = NULL,
  other.entity = NULL, 
  sep = NULL,
  empty = FALSE) {
  
  # # Validate arguments --------------------------------------------------------
  # 
  # validate_arguments(
  #   fun.name = 'template_arguments',
  #   fun.args = as.list(environment()))
  
  # Parameterize --------------------------------------------------------------
  
  # If empty = TRUE, then set path to /inst/templates so empty templates can
  # be read.
  
  if (isTRUE(empty)) {
    path <- system.file("/eal_templates", package = "ecocomDP")
  }
  
  # Get attributes of template files and arguments
  
  attr_arg <- data.table::fread(
    file = system.file(
      '/eal_templates/arguments.txt',
      package = 'ecocomDP'),
    fill = TRUE,
    blank.lines.skip = TRUE)
  
  attr_tmp <- read_template_attributes()
  
  # Initialize arguments ------------------------------------------------------
  
  output <- vector('list', nrow(attr_arg))
  names(output) <- attr_arg$argument_name
  
  # Initialize templates ------------------------------------------------------
  
  # Distinguish metadata templates from other files located at path.
  
  if (!is.null(path)) {
    path_files <- list.files(path)
    if (!length(path_files) == 0) {
      is_template <- rep(FALSE, length(path_files))
      for (i in 1:length(path_files)){
        is_template[i] <- any(
          stringr::str_detect(path_files[i], attr_tmp$regexpr))
      }
      templates <- vector('list', length(path_files[is_template]))
      names(templates) <- enc2utf8(path_files[is_template])
    }
  } else {
    templates <- NULL
  }
  
  # Add empty versions of all metadata templates if argument empty = TRUE.
  # Only include .txt extensions to prevent duplication.
  
  if (isTRUE(empty)) {
    path_files <- list.files(path)
    is_template <- rep(FALSE, length(path_files))
    for (i in 1:length(path_files)){
      is_template[i] <- any(
        stringr::str_detect(path_files[i], attr_tmp$regexpr))
    }
    templates <- vector('list', length(path_files[is_template]))
    names(templates) <- enc2utf8(path_files[is_template])
    templates[stringr::str_detect(names(templates), ".docx|.md")] <- NULL
  }
  
  # Initialize data tables ----------------------------------------------------
  
  if (!is.null(data.table)){
    data_table <- validate_file_names(
      path = data.path,
      data.files = data.table)
    data_tables <- vector('list', length(data_table))
    names(data_tables) <- enc2utf8(as.character(data_table))
  } else {
    data_tables <- NULL
  }
  
  # Initialize other entities -------------------------------------------------
  
  if (!is.null(other.entity)){
    other_entity <- validate_file_names(
      path = data.path,
      data.files = other.entity)
    other_entities <- vector('list', length(other_entity))
    names(other_entities) <- enc2utf8(as.character(other_entity))
  } else {
    other_entities <- NULL
  }
  
  # Read templates ------------------------------------------------------------
  
  if (!is.null(path)) {
    
    # Helper functions for reading templates
    
    read_tbl <- function(f) {
      
      # FIXME: Convert NA to "" expected by EAL funcitons. Assume NA listed
      # under missingValueCode of the attributes template is "" unless 
      # accompanied by a missingValueCodeExplanation
      
      d <- as.data.frame(
        data.table::fread(
          file = f,
          fill = TRUE,
          blank.lines.skip = TRUE,
          sep = "\t",
          quote = "",
          colClasses = list(
            character = 1:utils::count.fields(f, sep = "\t")[1])))
      
      # Clean up result
      for (k in 1:ncol(d)) {
        # Remove bookend quote characters mistakenly added by some file writers
        # where values contain commas
        d[ , k] <- stringr::str_remove_all(d[ , k], "\"")
        # Re-encode metadata in UTF-8
        d[ , k] <- enc2utf8(d[ , k])
      }
      d
      
    }
    
    read_txt <- function(f) {
      if (tools::file_ext(f) == "txt") {
        # .txt is not well supported by EML::set_TextType(). Reading the .txt
        # file as character strings and parsing to paragraphs works better.
        para <- as.list(
          unlist(
            stringr::str_split(
              data.table::fread(f), 
              pattern = "(\r\r)|(\n\n)|(\r\n\r\n)")))
      } else if (tools::file_ext(f) == "md") {
        # TODO: Update this section when the EML R library has better support 
        # for markdown
        if (stringr::str_detect(
          basename(f), 
          paste(
            attr_tmp$regexpr[
              (attr_tmp$type == "text") & (attr_tmp$template_name == "methods")],
            collapse = "|"))) {
          txt <- EML::set_methods(f)
          txt <- list(
            methodStep = list(
              description = txt$methodStep$description))
          return(txt)
        } else {
          para <- as.list(
            unlist(
              stringr::str_split(
                data.table::fread(f),
                pattern = "(\r\r)|(\n\n)|(\r\n\r\n)")))
        }
      } else if (tools::file_ext(f) == "docx") {
        # .docx is not well supported by EML::set_TextType() but a 
        # refactoring of some of this funcions underlying code improves 
        # performance.
        docbook <- to_docbook(f)
        use_i <- stringr::str_detect(
          xml2::xml_name(xml2::xml_children(docbook)), 
          "sect")
        if (any(use_i)) {
          # To simplify parsing, if <section> is present then section titles 
          # and children <para> will be flattened into <para>
          xpath <- paste0(
            "/article/", 
            unique(xml2::xml_name(xml2::xml_children(docbook))[use_i]))
          xml <- xml2::xml_new_root("article")
          xml2::xml_add_child(xml, "title")
          lapply(
            xml2::xml_find_all(docbook, xpath),
            function(m) {
              lapply(
                xml2::xml_children(m),
                function(n) {
                  xml2::xml_set_name(n, "para")
                  xml2::xml_add_child(xml, n)
                })
            })
          para <- emld::as_emld(xml)$para
        } else {
          para <- emld::as_emld(to_docbook(f))$para
        }
      }
      # Adjust outputs to match unique structure of abstract, additional_info, 
      # intellectual_rights, and methods nodes. Use default EML::set_TextType() 
      # output when NULL otherwise an asynchronous process will occur between 
      # between "templates" and "tfound" objects.
      if (stringr::str_detect(
        basename(f), 
        paste(
          attr_tmp$regexpr[
            (attr_tmp$type == "text") & (attr_tmp$template_name == "methods")],
          collapse = "|"))) {
        if (is.null(para)) {
          txt <- EML::set_methods(f)
        } else {
          txt <- list(
            methodStep = list(
              description = list(
                para = para)))
        }
      } else {
        if (is.null(para)) {
          txt <- EML::set_TextType(f)
        } else {
          txt <- list(
            section = list(),
            para = para)
        }
      }
      txt
    }
    
    # Loop through each metadata template found at path
    
    tfound <- names(templates)
    for (i in 1:length(tfound)){
      
      # Read abstract ---------------------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "abstract"])) {
        templates[[i]]$content <- read_txt(
          paste0(path, '/', tfound[i]))
      }
      
      # Read additional information -------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "additional_info"])) {
        templates[[i]]$content <- read_txt(
          paste0(path, '/', tfound[i]))
      }
      
      # Read annotations ------------------------------------------------------
      
      # FIXME: Use read_tbl()
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "annotations"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read attributes (data table) ------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "attributes"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
        if (any(is.na(templates[[i]]$content$missingValueCode))) {
          # Ensure any NA missing value codes are expressed as strings 
          # (i.e. "NA") otherwise they will not be recognized by downstream
          # functions
          templates[[i]]$content$missingValueCode[
            is.na(templates[[i]]$content$missingValueCode)] <- "NA"
        }
      }
      
      # Read categorical variables --------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "catvars"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read custom units -----------------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "custom_units"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read geographic bounding boxes ----------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "bounding_boxes"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read geographic coverage ----------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "geographic_coverage"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read intellectual rights ----------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "intellectual_rights"])) {
        templates[[i]]$content <- read_txt(
          paste0(path, '/', tfound[i]))
      }
      
      # Read keywords ---------------------------------------------------------
      
      # FIXME: Remove rows with empty keywords
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "keywords"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read methods ----------------------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "methods"])) {
        templates[[i]]$content <- read_txt(
          paste0(path, '/', tfound[i]))
      }
      
      # Read personnel --------------------------------------------------------
      
      # FIXME: Homogenize case of the 'role' field (i.e. use tolower())
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "personnel"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read provenance -------------------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "provenance"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read taxonomic coverage -----------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "taxonomicCoverage"])) {
        templates[[i]]$content <- list(
          taxonomicClassification = EML::read_eml(
            paste0(path, '/', tfound[i]))$taxonomicClassification)
      } else if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "taxonomic_coverage"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
    }
    
  }
  
  # Read data tables ----------------------------------------------------------
  
  # FIXME: Warn user of possible empty columns (i.e. "V([:digit:])*")
  if (!is.null(data.table)) {
    
    for (i in 1:length(data.table)) {
      f <- paste0(data.path, "/", data.table[i])
      
      if (is.null(sep)){
        
        data_tables[[i]]$content <- as.data.frame(
          data.table::fread(
            file = f,
            fill = TRUE,
            blank.lines.skip = TRUE))
        
      } else {
        
        data_tables[[i]]$content <- utils::read.table(
          file = f,
          header = T,
          sep = sep,
          quote = "\"",
          as.is = TRUE,
          comment.char = "")
        
      }
      
    }
    
  }
  
  # Read other entities -------------------------------------------------------
  
  # if (!is.null(other.entity)) {
  #   # for (i in 1:length(other.entity)) {
  #   #   other_entities[[i]]$content <- NA
  #   # }
  # }
  
  # Combine components & return -----------------------------------------------
  
  output$x <- list(
    template = templates,
    data.table = data_tables,
    other.entity = other_entities)
  
  output
  
}



# Helper functions ------------------------------------------------------------

#' Convert to docbook
#' 
#' This function is not exported from the EML R Package but useful here
#'
#' @param file
#'     (character) Full path to file to be read into docbook
#' @return
#'     (xml) Docbook XML R object
to_docbook <- function(file = NULL) {
  if (!tools::file_ext(file) %in% c("xml", "dbk", "db")) {
    ## Not xml yet, so use pandoc to generate docbook
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      stop("rmarkdown package required to convert to Docbook format",
           call. = FALSE
      )
    }
    if (!rmarkdown::pandoc_available()) {
      stop(paste("Pandoc is required to convert to Docbook format.",
                 "Please supply input text directly"),
           call. = FALSE
      )
    }
    pandoc_convert <-
      getExportedValue("rmarkdown", "pandoc_convert")
    wd <- getwd()
    dir <- tempdir()
    file.copy(file, file.path(dir, basename(file)), overwrite = TRUE)
    setwd(dir)
    docbook_file <- tempfile(tmpdir = ".", fileext = ".xml")
    pandoc_convert(
      basename(file),
      to = "docbook",
      output = normalizePath(docbook_file, winslash = "/", mustWork = FALSE),
      options = "-s"
    )
    docbook <- xml2::read_xml(docbook_file)
    on.exit(setwd(wd))
  } else {
    ## File is already xml/docbook, so no need for pandoc
    docbook <- xml2::read_xml(file)
  }
  ## Unlike EML, treat this as literal!
  xml2::xml_ns_strip(docbook)
  docbook
}