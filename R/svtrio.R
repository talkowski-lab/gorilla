#' Create a new `svtrio` object
#'
#' The `svtrio` object represents SV evidence over a genomic region for a
#' child, father, mother trio of samples.
#'
#' @param evidence A [`svevidence`] object.
#' @param child The child sample ID.
#' @param father The father sample ID.
#' @param mother The mother sample ID.
#' @returns A `svtrio` object.
#' @export
#'
#' @examples
#' pe_path <- system.file(
#'     "extdata",
#'     "example.PE.txt.gz",
#'     package = "gorilla",
#'     mustWork = TRUE
#' )
#' sr_path <- system.file(
#'     "extdata",
#'     "example.SR.txt.gz",
#'     package = "gorilla",
#'     mustWork = TRUE
#' )
#' rd_path <- system.file(
#'     "extdata",
#'     "example.RD.txt.gz",
#'     package = "gorilla",
#'     mustWork = TRUE
#' )
#' medians_path <- system.file(
#'    "extdata",
#'    "example_medianCov.txt",
#'    package = "gorilla",
#'    mustWork = TRUE
#' )
#'
#' pe <- pe_file(pe_path)
#' sr <- sr_file(sr_path)
#' rd <- rd_file(rd_path, medians_path)
#'
#' sv <- svevidence("chr16", 28743149, 28745149, pe, sr, rd, "DUP")
#' trio <- svtrio(sv, "gorilla0000", "gorilla0001", "gorilla0002")
svtrio <- function(evidence, child, father, mother) {
    new_svtrio(evidence, child, father, mother)
}

new_svtrio <- function(evidence, child, father, mother) {
    stopifnot(inherits(evidence, "svevidence"))
    stopifnot(is_string(child))
    stopifnot(is_string(father))
    stopifnot(is_string(mother))

    evidence <- subset_samples(evidence, c(child, father, mother))
    trio <- list(child = child, father = father, mother = mother)

    structure(list(evidence = evidence, trio = trio), class = "svtrio")
}

#' Test if an SV is de novo
#'
#' @description
#' `is_denovo` is a generic, but it only really works for an
#' [`svtrio`][svtrio()] object. It uses some low effort heuristics to check
#' whether the PE/SR/RD evidence for an SV supports a de novo status. It is
#' only possible to confirm the de novo status of a variant if the evidence for
#' all members of the trio, i.e. child, mother, father, are available.
#'
#' @param x An object to test for de novo status.
#' @returns Is the object predicted to represent a de novo event?
#' @export
is_denovo <- function(x) {
    UseMethod("is_denovo")
}

#' @export
is_denovo.svtrio <- function(x) {
    supports_sv(x$evidence, x$trio$child) &&
        !supports_sv(x$evidence, x$trio$father) &&
        !supports_sv(x$evidence, x$trio$mother)
}
