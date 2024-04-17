#' VIAME CSV fixer
#' 
#' VIAME CSV fixer functions: read/write VIAME CSV files, and filter by thresholds
#' @rdname viame
#' 
#' @param file file path; see [readr::read_csv()]
#' @param x tibble; with format the same as the output of `read_viame_csv()`
#' @param threshold_base numeric; base confidence threshold from VIAME-Web
#' @param threshold_df tibble; data frame of individual type thresholds from
#'   VIAME-Web that are different from `threshold_base`. Must have two columns:
#'   'class' and 'threshold'
#' 
#' @return
#' * `read_viame_csv()`: tibble, output of [readr::read_csv()]
#' * `viame_csv_metadata()`: tibble with metadata to write to VIAME CSV
#' * `write_viame_csv()`: see [readr::write_csv()]
#' * `fix_viame_csv()`: tibble of a fixed VIAME CSV
#' 
#' @examplesIf FALSE
#' threshold.df <- data.frame(class = "surf", threshold = 0.89)
#' read.adv <- read_viame_csv("C:/Users/PinnipedData/Downloads/test-images-adv.csv")
#' fix.adv <- fix_viame_csv(read.adv, 0.02)
#' fix_viame_csv(read.adv, 0.02, threshold.df)
#' 
#' write_viame_csv(fix.adv, "C:/Users/PinnipedData/Downloads/tmp.csv")
#' read_viame_csv("C:/Users/PinnipedData/Downloads/tmp.csv")
#' 
#' @export
read_viame_csv <- function(file) {
  # Error check
  line1 <- read_csv(file, col_names = FALSE, col_types = cols(), skip = 0, n_max = 1)
  if (!identical(names(viame.csv.names), unname(unlist(line1))))
    stop("The column names of the input VIAME CSV are not as expected. ", 
         "If you are using a VIAME CSV file, please submit this as an issue")
  
  # Generate full set of columnnames
  line3 <- read_csv(file, col_names = FALSE, col_types = cols(), skip = 2, n_max = 1)
  n.cols.toadd <- ncol(line3) - length(viame.csv.names)
  n.classes.toadd <- n.cols.toadd / 2
  
  viame.colnames <- c(
    viame.csv.names, 
    paste(rep(c("class", "confidence"), n.classes.toadd), 
          str_pad(rep(2:(n.classes.toadd+1), each = 2) , 2, pad = "0"), 
          sep = "_")
  )
  
  # Read in and output file
  read_csv(file, skip = 2, col_names = viame.colnames, 
           col_types = paste0("iciiiiidi??", rep("?", n.cols.toadd)))
}


#' @rdname viame
#' @export
viame_csv_metadata <- function() {
  c(viame.csv.metadata, 
    str_glue("exported_time: \"", 
             "{strftime(Sys.time(), '%a %b %d %H:%M:%S %Y', tz='UTC')}\""))
  
}


#' @rdname viame
#' @export
write_viame_csv <- function(x, file) {
  ncol.vcsv <- seq_along(viame.csv.names)
  stopifnot(
    is.data.frame(x), 
    identical(names(x)[ncol.vcsv], unname(viame.csv.names))
  )
  
  # Prep lines to be written
  line1 <- paste(names(viame.csv.names), collapse = ", ")
  line2 <- paste(viame_csv_metadata(), collapse = ", ")
  
  # Write VIAME CSV. Column names, metadata, then data
  write_lines(line1, file = file, na = "")
  write_lines(line2, file = file, na = "", append = TRUE)
  write_csv(x, file, na = "", append = TRUE)
}


#' @rdname viame
#' @export
fix_viame_csv <- function(x, threshold_base, threshold_df = NULL) {
  stopifnot(
    is.data.frame(x), 
    is.numeric(threshold_base), 
    identical(names(x)[seq_along(viame.csv.names)], unname(viame.csv.names))
  )
  
  # x <- x %>% select(track_id:threshold_01)
  
  # If threshold_df is null, only use base filter. Else, use data frame
  if (is.null(threshold_df)) {
    x %>% filter(confidence > threshold_base)
  } else {
    # Check formatting
    if (!identical(names(threshold_df), c("class", "threshold"))) {
      stop("The names of threshold_df must be 'class' and 'threshold'")
    }
    x %>% 
      left_join(threshold_df, by = join_by(class_01 == class)) %>% 
      mutate(threshold = replace_na(threshold, threshold_base)) %>% 
      filter(confidence > threshold) %>% 
      select(-c(threshold))
  }
}


