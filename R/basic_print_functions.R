#' Print Helper
#'
#' @param liheader text to print as header of list
#' @param litems list items
#' @param likeyword list items keyword or tag
#' @param lileadcol color of the leading list item identifier
#' @param likwbgfill fill color of the keyword background
#' @param likwtxtcol text color of the keyword
#' @param libgfill fill color of the leading list identifiers
#' @param sym symbol to use to print a sectioned list
#' @param symcol color of the list partitioning
#' @param is_ordered TRUE (default) means the leading list identifer is numbered bullet pts
#' @param msg message message for printstamp
#' @param t time object system time for print_runtime
#' @param stitle Title of section for PrintSection
#' @param header Section header text for PrintSection
#' @param bullets list of notes displayed as bullets in higher level PrintSection function
#' @param tag arg for higher level PrintSection function. Content of tag box fill
#' @param color arg for higher level PrintSection function. Color of tag box fill
#' @param face functions from crayon (one of bold or italic)
#'
#' @import crayon
#' @import stringr
#'
#' @name printer_funs
NULL

#' @describeIn printer_funs higher level function to print message on console
#' @export
PrintSection <- function(stitle = NULL,
                         header = NULL,
                         bullets = NULL,
                         tag = "NOTE",
                         color = "white"){

   if( !is.null(stitle) )
      printstamp(stitle, "%")

   if( is.null(bullets) )
      stop("Provide content for bulleted list")

   header[is.null(header)] <- "COMMENTS"
   printlines(liheader = header,
              litems = as.list(bullets),
              likeyword = tag,
              sym = "_",
              likwbgfill = color)
}

#' @describeIn printer_funs print message on console
#' @export
printstamp <- function(msg,
                       sym="#",
                       color = c("blue", "red", "cyan", "green", "magenta", "yellow", "white", "silver"),
                       face = c("bold", "italic")){

  if(stringr::str_length(msg) %% 2 == 1)
    msg <- stringr::str_c(msg, " ")

  msg <- stringr::str_c(" ", msg, " ")
  scount <- stringr::str_length(msg)
  cushion <- ceiling(scount*1.1) - scount

  cushion <- cushion + cushion %% 2
  topcount  <- scount + cushion - 1
  sidecount <- 3

  hdft   <- stringr::str_c(rep(sym, topcount), collapse = "")
  spaces <- stringr::str_c(rep(" ", topcount - 1), collapse = "")
  sides  <- rep(sym, sidecount)

  # grid_col <- topcount + 1
  # grid_row <- sidecount + 2

  tmp <- stringr::str_c(c(hdft, stringr::str_c(sides, spaces), hdft), sym, collapse = "\n")
  txt <- stringr::str_split(stringr::str_split(tmp, "\n")[[1]], "")

  pad.l <- c(sym, rep(" ", cushion/2-1))
  pad.r <- c(rep(" ", cushion/2-1), sym)
  txt[[3]] <- c(pad.l, stringr::str_split(msg, "")[[1]], pad.r)

  cat("\n\n")
  cat(paste0(sapply(txt, function(itxt) paste0(c(itxt, "\n"), collapse = "")), collapse = ""))
  cat("\n")
}


#' @describeIn printer_funs print message on console
#' @export
printlines <- function(liheader = NULL,
                       litems = NULL,
                       likeyword = "TAG",
                       lileadcol = "black",
                       likwbgfill = "yellow",
                       likwtxtcol = "black",
                       libgfill = "blue",
                       sym = "=",
                       symcol = "blue",
                       is_ordered = TRUE){
  fnbgFill <- paste0("bg", stringr::str_to_title(libgfill))
  fnKwFill <- paste0("bg", stringr::str_to_title(likwbgfill))
  sym <- do.call(symcol, list(sym))

  i <- 0
  N <- length(litems)
  topcount <- max(sapply(litems, function(msg){

    if(stringr::str_length(msg) %% 2 == 1)
      msg <- stringr::str_c(msg, " ")

    msg     <- stringr::str_c(" ", msg, " ")
    scount  <- stringr::str_length(msg)
    cushion <- ceiling(scount*1.1) - scount

    cushion  <- cushion + cushion %% 2
    cnt <- scount + cushion + 3
    return(cnt)
  })) + 6

  output <- stringr::str_c(
    unlist(lapply(litems, function(msg){
      i <<- i + 1

      if(stringr::str_length(msg) %% 2 == 1)
        msg <- stringr::str_c(msg, " ")

      msg     <- italic(stringr::str_c(" ", msg, " "))
      scount  <- stringr::str_length(msg)
      cushion <- ceiling(scount*1.1) - scount

      cushion   <- cushion + cushion %% 2
      sidecount <- 1

      hdft   <- stringr::str_c(rep(sym, topcount), collapse = "")
      spaces <- stringr::str_c(rep(" ", topcount - 1), collapse = "")
      r.side <- " "

      grid_col <- topcount + 1
      grid_row <- sidecount + 2

      tmp <- stringr::str_c(c(hdft, stringr::str_c(sym, spaces)), collapse = "\n")
      txt <- stringr::str_split(stringr::str_split(tmp, "\n")[[1]], "")

      if(is_ordered == TRUE){
        lilead <- paste0(" ", i, ". ")
      }else{
        lilead <- NULL
      }
      kw <- do.call(likwtxtcol, list(paste0(" [", likeyword, "] ")[ !is.null(likeyword) ]))
      keyword <- paste0(" ", do.call(fnKwFill, list( bold(kw) )))
      litxt <- paste0(do.call(fnbgFill, list(do.call(lileadcol, list( bold(lilead) )))), keyword)

      pad.l <- paste0(" ", litxt, " ")
      pad.r <- rep(" ", cushion/2-1)
      txt[[2]] <- c(pad.l, stringr::str_split(msg, "")[[1]], pad.r)

      res <- paste0(sapply(txt, function(itxt) paste0(c(itxt, "\n"), collapse = "")), collapse = "")

      # return bottom line label also
      if(i == N){
        return(paste0("\n", stringr::str_trim(paste0(res, hdft), "both")))
      }else{
        return(paste0("\n", stringr::str_trim(res, "both")))
      }

    })), collapse = ""
  )

  ul <- crayon::underline(paste0(" ", rep(" ", stringr::str_length(liheader)), collapse = ""))
  header <- paste0("\n\n", " ", do.call(fnbgFill, list(liheader)), "\n", ul, "\n")
  cat(header, "\n", output, "\n\n")
}





#' @describeIn printer_funs print timing on console
#' @export
print_runtime <- function(t, sym="#"){
   sym <- blue(sym)
   tf <- Sys.time()
   printstamp(paste0("RT: ", round(difftime(tf, t, units = "sec"), 0), " secs"), sym)
   invisible(NULL)
}


#' @describeIn printer_funs messing around with crayon
#' @export
buildPyramid <- function(){
   typeVector <- rep(c("left", "center", "right"), 4)
   for(i in 1:12){
      cat(crayon::col_align(crayon::red( paste0(rep("+", i), collapse = "") ), i, typeVector[i]))
      cat("\n")
   }
}

buildPyramid()

