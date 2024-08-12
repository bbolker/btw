# FUNCTION TO PARSE SCHEDULE FILES
parse_schedule <- function(file) {

    out <- scan(file = file, what="c", quiet=TRUE, sep="\n")

    sched_start <- grep("Default schedule", out)
    if (length(sched_start)==0) sched_start <- 0
    header_start <- grep("Rate Tried", out)
    
    ## SEPARATE SCHEDULE AND HEADER
    schedule <- read.table(file, skip = sched_start, nrow = (header_start - 1 - sched_start), sep = "\t", header = FALSE)
    names(schedule) <- c("operator","percent_tried")
    
    header <- read.table(file, skip = (header_start - 1), sep = "\t", header = TRUE)
    header <- header[,-ncol(header)]

    Schedule <- list(schedule=schedule, header=header)
    return(Schedule)
    
}
