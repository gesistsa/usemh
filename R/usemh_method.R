#' @export
use_mh_method <- function(open = rlang::is_interactive(), config_dir = c(".binder", "binder", ".")) {
    usethis:::check_is_package("use_mh_method()")
    config_dir <- match.arg(config_dir)
    ## Generate cff
    cffr::cff_write(cffr::cff_create()) ## auto add to .Rbuildignore
    usethis::use_build_ignore("CITATION.cff")
    desc <- usethis:::proj_desc()
    Package <- desc$get("Package")
    if (quarto::is_using_quarto()) {
        ## do nothing more
        return(invisible(NULL))
    }
    ## Capture the current postBuild in a temp. directory
    ## Not until this is fixed: quarto-dev/quarto-cli#9313
    active_dir <- getwd()
    resolved_config_dir <- normalizePath(file.path(active_dir, config_dir), mustWork = FALSE)
    config_dot <- config_dir == "." ## quarto default
    if (!config_dot && !dir.exists(resolved_config_dir)) {
        dir.create(resolved_config_dir)
    }
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
        .copy_if_and_ignore("postBuild", quarto_proj_basepath, active_dir, config_dot)
        .copy_if_and_ignore("apt.txt", quarto_proj_basepath, active_dir, config_dot)
        .copy_if_and_ignore("runtime.txt", quarto_proj_basepath, active_dir, config_dot)
        .copy_if_and_ignore(".jupyter", quarto_proj_basepath, active_dir, config_dot)
        if (!config_dot) {
            generated_config_files <- Filter(file.exists,
                                             file.path(active_dir, c("install.R", "postBuild", "apt.txt", "runtime.txt", ".jupyter")))
            file.copy(from = generated_config_files,
                      to = resolved_config_dir, recursive = TRUE)
            unlink(generated_config_files, recursive = TRUE)
            usethis::use_build_ignore(config_dir)
        }
    })
    usethis::use_template("quarto.yaml", "_quarto.yml", data = list("Package" = Package, "file" = "methodshub.qmd"), package = "usemh")
    usethis::use_build_ignore(c("_quarto.yml", ".quarto"))
    usethis::use_build_ignore("^methodshub", escape = FALSE)
    bug_reports <- desc$get("BugReports")
    if (is.na(bug_reports)) {
        bug_reports <- ""
    } else {
        bug_reports <- paste0("Issue Tracker: [", bug_reports, "](", bug_reports, ")")
    }
    usethis::use_template("methodshub.qmd",
                          data = list(
                              "Package" = Package,
                              "Title" = desc$get("Title"),
                              "Description" = .fix_cran_shorthands(desc$get("Description")),
                              "Maintainer" = desc$get_maintainer(),
                              "BugReports" = bug_reports
                          ),
                          ignore = FALSE, package = "usemh",
                          open = open
                          )
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
    .zap("binder")
    .zap(".binder")
}

.zap <- function(file) {
    if (file.exists(file)) {
        unlink(file, recursive = TRUE, force = TRUE)
    }
}

.copy_if_and_ignore <- function(file, quarto_proj_basepath, active_dir = ".", ignore = TRUE) {
    if (file.exists(file.path(quarto_proj_basepath, file))) {
        x <- file.copy(file.path(quarto_proj_basepath, file), active_dir, recursive = TRUE)
        if (ignore) {
            usethis::use_build_ignore(file)
        }
        return(invisible(TRUE))
    }
    return(invisible(FALSE))
}

.convert_md <- function(x, slug = "doi" , url_prefix = "https://doi.org/") {
    x <- stringr::str_replace(x, paste0("^\\<", slug, ":"), "")
    x <- stringr::str_replace(x, "\\>$", "")
    paste0("[", slug, ":", x, "](", url_prefix, x, ")")
}

.fix_cran_shorthands <- function(description) {
    dois <- as.character(stringr::str_extract_all(description, "\\<doi:[0-9a-zA-Z\\-\\./]+\\>", simplify = TRUE))

    for (doi in dois) {
        description <- stringr::str_replace(description, stringr::fixed(doi), .convert_md(doi))
    }
    arxivs <- as.character(stringr::str_extract_all(description, "\\<arXiv:[0-9a-zA-Z\\-\\./]+\\>", simplify = TRUE))
    for (arxiv in arxivs) {
        description <- stringr::str_replace(description, stringr::fixed(arxiv), .convert_md(arxiv, "arXiv", "https://arxiv.org/abs/"))
    }    
    return(description)
}
