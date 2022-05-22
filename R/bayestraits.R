## FUNCTION TO RUN BAYESTRAITS FROM R
bayestraits <- function (data=NULL, tree=NULL, commands=NULL, silent=TRUE, remove_files=TRUE) {

    bt_path <- getOption("bt_path")
    bt_bin <- getOption("bt_bin")
                                        # WARNINGS (check class of arguments, species names in the data and tree, BayesTraits is in your directory)
    if (!inherits(data, "data.frame")) stop("Data frame containing species data must be supplied")
    if (inherits(tree, "phylo")) {
        treelabs <- tree$tip.label
    } else if (inherits(tree, "multiPhylo")) {
        treelabs = attributes(tree)$TipLabel
    } else {
        stop("Tree must be of class phylo or multiPhylo")
    }
    if (!is.character(commands))
        stop("Character vector containing BayesTraits commands must be supplied.")
    nm <- data[[1]]
    if (!(is.factor(nm) || is.character(nm))) {
        stop("First column of data must contain species names.")
    }
    
    if (length(setdiff(treelabs, nm))>0) stop(paste("No match found in the data:", paste(setdiff(tree$tip.label, nm), collapse=", ")))
    if (length(setdiff(nm, treelabs))>0) stop(paste("No match found in the phylogeny:", paste(setdiff(nm, tree$tip.label), collapse=", ")))
    if (length(setdiff(treelabs, nm))>0 || length(setdiff(nm, treelabs))>0) stop("Species in your phylogeny and data must match up exactly.")

    ## DETECT SYSTEM
    windows <- .Platform$OS.type == "windows"

    ## CHECK FOR BAYESTRAITS IN WORKING DIRECTORY

    bt_full <- file.path(bt_path, bt_bin)
    if (windows) {
        if (!("BayesTraitsV3.exe" %in% list.files())) stop("BayesTraitsV3.exe is not in your current working directory.")
    } else if (!file.exists(bt_full)) stop("BayesTraits not found")
    dir <- getwd()


    ## WRITE INPUT FILE, TREE, AND DATA
    write(c(commands, "run"), file = "./inputfile.txt")
    ape::write.nexus(tree, file = "./tree.nex", translate = T)
    write.table(data, file = "./data.txt", quote = F, col.names = F, row.names = F)


    ## RUN BAYESTRAITS
    if (windows) {
        if (silent) {
            invisible(shell(sprintf("%s.exe tree.nex data.txt < inputfile.txt",
                                    bt_full),
                            intern = TRUE))
        } else {
            shell(sprintf("%s.exe tree.nex data.txt < inputfile.txt",
                          bt_full))
        }
        
    } else {
        system(sprintf("%s ./tree.nex ./data.txt  < ./inputfile.txt",
                       bt_full),
               ignore.stdout = silent)
    }

    ## CHECK WHICH OUTPUT IS THERE
    log <- "data.txt.Log.txt" %in% list.files()
    schedule <- "data.txt.Schedule.txt" %in% list.files()
    stones <- "data.txt.Stones.txt" %in% list.files()
    ancstates <- "data.txt.AncStates.txt" %in% list.files()
    output.trees <- "data.txt.Output.trees" %in% list.files()
    varrates <- "data.txt.VarRates" %in% list.files()
    if (!log) stop("Something went wrong: btw can't find a log file")
    if (varrates) warning("btw does not handle output from a variable rates model.")


    ## CAPTURE AND PARSE OUTPUT
    if (windows) {
        Log <- parse_log("data.txt.Log.txt")
        if (schedule) Schedule <- parse_schedule("data.txt.Schedule.txt") else Schedule <- NULL
        if (stones) Stones <- parse_stones("data.txt.Stones.txt") else Stones <- NULL
        if (ancstates) AncStates <- parse_ancstates("data.txt.AncStates.txt") else AncStates <- NULL
        if (output.trees) OutputTrees <- ape::read.nexus("data.txt.Output.trees") else OutputTrees <- NULL
    } else {
        Log <- parse_log(paste0(dir, "/data.txt.Log.txt"))
        if (schedule) Schedule <- parse_schedule(paste0(dir, "/data.txt.Schedule.txt")) else Schedule <- NULL
        if (stones) Stones <- parse_stones(paste0(dir, "/data.txt.Stones.txt")) else Stones <- NULL
        if (ancstates) AncStates <- parse_ancstates(paste0(dir, "/data.txt.AncStates.txt")) else AncStates <- NULL
        if (output.trees) OutputTrees <- ape::read.nexus(paste0(dir, "/data.txt.Output.trees")) else OutputTrees <- NULL
    }
    results <- list(Log=Log,
                    Schedule=Schedule,
                    Stones=Stones,
                    AncStates=AncStates,
                    OutputTrees=OutputTrees)

                                        # REMOVE OUTPUT FILES FROM DISK
    if (remove_files) {
        if (windows) {
            shell(paste("DEL", "data.txt.Log.txt"))
            shell(paste("DEL", "data.txt"))
            shell(paste("DEL", "tree.nex"))
            shell(paste("DEL", "inputfile.txt"))
            if (schedule) shell(paste("DEL", "data.txt.Schedule.txt"))
            if (stones) shell(paste("DEL", "data.txt.Stones.txt"))
            if (ancstates) shell(paste("DEL", "data.txt.AncStates.txt"))
            if (output.trees) shell(paste("DEL", "data.txt.Output.trees"))
        } else {
            system(paste0("rm ", dir, "/data.txt.Log.txt"))
            system(paste0("rm ", dir, "/data.txt"))
            system(paste0("rm ", dir, "/tree.nex"))
            system(paste0("rm ", dir, "/inputfile.txt"))
            if (schedule) system(paste0("rm ", dir, "/data.txt.Schedule.txt"))
            if (stones) system(paste0("rm ", dir, "/data.txt.Stones.txt"))
            if (ancstates) system(paste0("rm ", dir, "/data.txt.AncStates.txt"))
            if (output.trees) system(paste0("rm ", dir, "/data.txt.Output.trees"))
        }
    }

                                        # RETURN RESULTS
    return(results)
}
