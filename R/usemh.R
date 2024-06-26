#' @export
use_mh <- function(open = rlang::is_interactive()) {
    usethis:::check_is_package("use_mh()")
    ## Generate cff
    cffr::cff_write(cffr::cff_create()) ## auto add to .Rbuildignore
    usethis::use_build_ignore("CITATION.cff")
    desc <- usethis:::proj_desc()
    Package <- desc$get("Package")
    ## Capture the current postBuild in a temp. directory
    ## Not until this is fixed: quarto-dev/quarto-cli#9313
    if (!quarto::is_using_quarto()) {
        active_dir <- getwd()
        usethis::use_template("install.R", data = list("Package" = Package), ignore = TRUE, package = "usemh")
        withr::with_tempdir({
            quarto_proj_basepath <- file.path(getwd(), Package)
            if (dir.exists(quarto_proj_basepath)) {
                unlink(quarto_proj_basepath, recursive = TRUE)
            }
            x <- quarto::quarto_create_project(name = Package, quiet = TRUE, no_prompt = TRUE)
            ## copy some rubbish qmd so that it will generate R runtime.txt
            file.copy(system.file("templates", "rubbish.qmd", package = "usemh"), quarto_proj_basepath)
            file.copy(file.path(active_dir, "install.R"), quarto_proj_basepath)
            setwd(quarto_proj_basepath)
            quarto:::quarto_use(args = c("binder", "--no-prompt"))
            .copy_if_and_ignore("postBuild", quarto_proj_basepath, active_dir)
            .copy_if_and_ignore("apt.txt", quarto_proj_basepath, active_dir)
            .copy_if_and_ignore("runtime.txt", quarto_proj_basepath, active_dir)
            .copy_if_and_ignore(".jupyter", quarto_proj_basepath, active_dir)
        })
        usethis::use_template("quarto.yaml", "_quarto.yml", data = list("Package" = Package), package = "usemh")
        usethis::use_build_ignore(c("_quarto.yml", ".quarto"))
        usethis::use_build_ignore("^methodshub", escape = FALSE)
        bug_reports <- desc$get("BugReports")
        if (is.na(bug_reports)) {
            bug_reports <- ""
        } else {
            bug_reports <- paste0("Issue Tracker: [", bug_reports, "](", bug_reports, ")")
        }
        usethis::use_template("methodshub.qmd",
                              data = list("Package" = Package,
                                          "Title" = desc$get("Title"),
                                          "Description" = .fix_dois(desc$get("Description")),
                                          "Maintainer" = desc$get_maintainer(),
                                          "BugReports" = bug_reports),
                              ignore = FALSE, package = "usemh",
                              open = open)
    }
}

zap_mh <- function() {
    ## TODO: Clean .Rbuildignore
    usethis:::check_is_package("zap_mh()")
    .zap("CITATION.cff")
    .zap("_quarto.yml")
    .zap("apt.txt")
    .zap("install.R")
    .zap("postBuild")
    .zap("methodshub.qmd")
    .zap(".jupyter")
}

.zap <- function(file) {
    if (file.exists(file)) {
        unlink(file, recursive = TRUE, force = TRUE)
    }
}

.copy_if_and_ignore <- function(file, quarto_proj_basepath, active_dir = ".") {
    if (file.exists(file.path(quarto_proj_basepath, file))) {
        x <- file.copy(file.path(quarto_proj_basepath, file), active_dir, recursive = TRUE)
        usethis::use_build_ignore(file)
        return(invisible(TRUE))
    }
    return(invisible(FALSE))
}

.convert_doi_md <- function(doi) {
    doi <- stringr::str_replace(doi, "^\\<doi:", "")
    doi <- stringr::str_replace(doi, "\\>$", "")
    paste0("[<doi:", doi, ">](https://doi.org/", doi, ")")
}

.fix_dois <- function(description) {
    dois <- as.character(stringr::str_extract_all(description, "\\<doi:[0-9a-zA-Z\\-\\./]+\\>", simplify = TRUE))

    for (doi in dois) {
        description <- stringr::str_replace(description, stringr::fixed(doi), .convert_doi_md(doi))
    }
    return(description)
}

