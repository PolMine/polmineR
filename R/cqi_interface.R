#' as.cmd <- function(cmd) paste("perl -e '", paste(cmd, collapse = " "), "'", sep="")
#' 
#' .list_corpora <- list(
#'   rcqp = function() cqi_list_corpora(),
#'   perl = function() stop("list_corpora not implemented for perl interface")
#' )
#' 
#' list_corpora <- function(){
#'   .list_corpora[[getOption("polmineR.interface")]]()
#' }
#' 
#' .attribute_size <- list(
#'   rcqp = function(corpus, attribute) cqi_attribute_size(paste(corpus, attribute, sep=".")),
#'   perl = function(corpus, attribute) as.integer(system(sprintf(as.cmd(cqi_perl[["attribute_size"]]), corpus, attribute), intern=TRUE))
#' )
#' 
#' #' @rdname cwb-interface
#' attribute_size <- function(corpus, attribute){
#'   .attribute_size[[getOption("polmineR.interface")]](corpus, attribute)
#' }
#' 
#' .lexicon_size <- list(
#'   rcqp = function(corpus, pAttribute) cqi_lexicon_size(paste(corpus, pAttribute, sep=".")),
#'   perl = function(corpus, pAttribute) as.integer(system(sprintf(as.cmd(cqi_perl[["lexicon_size"]]), corpus, pAttribute), intern=TRUE))
#' )
#' 
#' #' @rdname cwb-interface
#' lexicon_size <- function(corpus, pAttribute){
#'   .lexicon_size[[getOption("polmineR.interface")]](corpus, pAttribute)
#' }
#' 
#' .cpos2struc <- list(
#'   rcqp = function(corpus, pAttribute, cpos) cqi_cpos2struc(paste(corpus, pAttribute, sep="."), cpos),
#'   perl = function(corpus, pAttribute, cpos) as.integer(system(sprintf(as.cmd(cqi_perl[["cpos2struc"]]), corpus, pAttribute, paste(as.character(cpos), collapse=",")), intern=TRUE))
#' )
#' 
#' #' @rdname cwb-interface
#' cpos2struc <- function(corpus, pAttribute, cpos){
#'   .cpos2struc[[getOption("polmineR.interface")]](corpus, pAttribute, cpos)
#' }
#' 
#' .cpos2str <- list(
#'   rcqp = function(corpus, pAttribute, cpos) cqi_cpos2str(paste(corpus, pAttribute, sep="."), cpos),
#'   perl = function(corpus, pAttribute, cpos) system(sprintf(as.cmd(cqi_perl[["cpos2str"]]), corpus, pAttribute, paste(as.character(cpos), collapse=",")), intern=TRUE)
#' )
#' 
#' #' @rdname cwb-interface
#' cpos2str <- function(corpus, pAttribute, cpos){
#'   .cpos2str[[getOption("polmineR.interface")]](corpus, pAttribute, cpos)
#' }
#' 
#' .cpos2id <- list(
#'   rcqp = function(corpus, pAttribute, cpos) cqi_cpos2str(paste(corpus, pAttribute, sep="."), cpos),
#'   perl = function(corpus, pAttribute, cpos) as.integer(system(sprintf(as.cmd(cqi_perl[["cpos2id"]]), corpus, pAttribute, paste(as.character(cpos), collapse=",")), intern=TRUE))
#' )
#' 
#' #' @rdname cwb-interface
#' cpos2id <- function(corpus, pAttribute, cpos){
#'   .cpos2str[[getOption("polmineR.interface")]](corpus, pAttribute, cpos)
#' }
#' 
#' .struc2cpos <- list(
#'   rcqp = function(corpus, sAttribute, struc) cqi_struc2cpos(paste(corpus, sAttribute, sep="."), struc),
#'   perl = function(corpus, sAttribute, struc) as.integer(system(sprintf(as.cmd(cqi_perl[["struc2cpos"]]), corpus, sAttribute, struc), intern=TRUE))
#' )
#' 
#' #' @rdname cwb-interface
#' struc2cpos <- function(corpus, sAttribute, struc){
#'   .struc2cpos[[getOption("polmineR.interface")]](corpus, sAttribute, struc)
#' }
#' 
#' .id2str <- list(
#'   rcqp = function(corpus, pAttribute, id) cqi_id2str(paste(corpus, pAttribute, sep="."), id),
#'   perl = function(corpus, pAttribute, id) system(sprintf(as.cmd(cqi_perl[["id2str"]]), corpus, pAttribute, paste(as.character(id), collapse=",")), intern=TRUE)
#' )
#' 
#' #' @rdname cwb-interface
#' id2str <- function(corpus, pAttribute, id){
#'   .id2str[[getOption("polmineR.interface")]](corpus, pAttribute, id)
#' }
#' 
#' .struc2str <- list(
#'   rcqp = function(corpus, sAttribute, struc) cqi_struc2str(paste(corpus, sAttribute, sep="."), struc),
#'   perl = function(corpus, sAttribute, struc) system(sprintf(as.cmd(cqi_perl[["struc2str"]]), corpus, sAttribute, paste(as.character(struc), collapse=",")), intern=TRUE)
#' )
#' 
#' #' @rdname cwb-interface
#' struc2str <- function(corpus, sAttribute, struc){
#'   .struc2str[[getOption("polmineR.interface")]](corpus, sAttribute, struc)
#' }
#' 
#' .regex2id <- list(
#'   rcqp = function(corpus, pAttribute, regex) cqi_regex2id(paste(corpus, pAttribute, sep="."), regex),
#'   perl = function(corpus, pAttribute, regex) as.integer(system(sprintf(as.cmd(cqi_perl[["regex2id"]]), corpus, pAttribute, regex), intern=TRUE))
#' )
#' 
#' regex2id <- function(corpus, pAttribute, regex){
#'   .regex2id[[getOption("polmineR.interface")]](corpus, pAttribute, regex)
#' }
#' 
#' .str2id <- list(
#'   rcqp = function(corpus, pAttribute, str) cqi_str2id(paste(corpus, pAttribute, sep="."), str),
#'   perl = function(corpus, pAttribute, str) as.integer(system(sprintf(as.cmd(cqi_perl[["str2id"]]), corpus, pAttribute, paste('"', str, '"', sep="", collapse=",")), intern=TRUE))
#' )
#' 
#' str2id <- function(corpus, pAttribute, str){
#'   .str2id[[getOption("polmineR.interface")]](corpus, pAttribute, str)
#' }
#' 
#' .id2freq <- list(
#'   rcqp = function(corpus, pAttribute, id) cqi_id2freq(paste(corpus, pAttribute, sep="."), id),
#'   perl = function(corpus, pAttribute, id) as.integer(system(sprintf(as.cmd(cqi_perl[["id2freq"]]), corpus, pAttribute, paste(id, collapse=",")), intern=TRUE))
#' )
#' 
#' id2freq <- function(corpus, pAttribute, id){
#'   .id2freq[[getOption("polmineR.interface")]](corpus, pAttribute, id)
#' }
#' 
#' 
#' .id2cpos <- list(
#'   rcqp = function(corpus, pAttribute, id) cqi_id2cpos(paste(corpus, pAttribute, sep="."), id),
#'   perl = function(corpus, pAttribute, id) as.integer(system(sprintf(as.cmd(cqi_perl[["id2cpos"]]), corpus, pAttribute, paste(id, collapse=",")), intern=TRUE))
#' )
#' 
#' id2cpos <- function(corpus, pAttribute, id){
#'   .id2cpos[[getOption("polmineR.interface")]](corpus, pAttribute, id)
#' }
#' 
#' .cpos2lbound <- list(
#'   rcqp = function(corpus, sAttribute, cpos) cqi_cpos2rbound(paste(corpus, sAttribute, sep="."), cpos),
#'   perl = function(corpus, sAttribute, cpos) message("cpos2lbound not implemented in the Perl-CL-Package")
#' )
#' 
#' cpos2lbound <- function(corpus, sAttribute, cpos){
#'   .cpos2lbound[[getOption("polmineR.interface")]](corpus, sAttribute, cpos)
#' }
#' 
#' .cpos2rbound <- list(
#'   rcqp = function(corpus, sAttribute, cpos) cqi_cpos2rbound(paste(corpus, sAttribute, sep="."), cpos),
#'   perl = function(corpus, sAttribute, cpos) message("cpos2rbound not implemented in the Perl-CL-Package")
#' )
#' 
#' cpos2rbound <- function(corpus, sAttribute, cpos){
#'   .cpos2rbound[[getOption("polmineR.interface")]](corpus, sAttribute, cpos)
#' }
#' 
#' query <- function(){}
#' dump_subcorpus <- function(){}
#' 
#' 
#' cqi_perl <- list(
#'   corpus_size = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "PLPRBT";',
#'     '$Word = $C->attribute("word", "p");',
#'     '$corpus_size = $Word->max_cpos;',
#'     'print $corpus_size;'
#'   ),
#'   attribute_size = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$Word = $C->attribute("%s", "p");',
#'     '$lex_size = $Word->max_id;',
#'     'print $lex_size;'
#'   ),
#'   str2freq = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$Word = $C->attribute("word", "p");',
#'     '$id = $Word->str2id("internet");',
#'     '$f = $Word->id2freq($id);',
#'     'print $f;'
#'   ),
#'   id2freq = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$Word = $C->attribute("%s", "p");',
#'     '@id = (%s);',
#'     'foreach $id (@id) {print $Word->id2freq($id); print "\n";};'
#'     # '@freq = $Word->idlist2freq(@id);',
#'     # 'foreach $freq (@freq){print $freq; print "\n";};'
#'   ),
#'   id2cpos = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$Word = $C->attribute("%s", "p");',
#'     '$id = (%s);',
#'     '@cpos = $Word->id2cpos($id);',
#'     'foreach $cpos (@cpos) {print $cpos; print "\n";};'
#'   ),
#'   id2str = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$Word = $C->attribute("%s", "p");',
#'     '@id = (%s);',
#'     'foreach $id (@id) {print $Word->id2str($id); print "\n";};'
#'   ),
#'   str2id = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$Word = $C->attribute("%s", "p");',
#'     # '@str = ("Migration", "Integration");',
#'     '@str = (%s);',
#'     # '@foo = (%s);',
#'     'foreach $str (@str) {print $Word->str2id($str); print "\n";};'
#'   ),
#'   cpos2struc = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$S = $C->attribute("%s", "s");',
#'     '@cpos = (%s);',
#'     'foreach $cpos (@cpos) {$num = $S->cpos2struc($cpos); print $num; print "\n";};'
#'   ),
#'   struc2cpos = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$S = $C->attribute("%s", "s");',
#'     '@struc = (%s);',
#'     # 'foreach $struc (@struc) {$num = $S->struc2cpos($struc); print $num; print "\n";};'
#'     'foreach $struc (@struc) {@cpos = $S->struc2cpos($struc); foreach $cpos (@cpos) {print $cpos; print "\n";}};'
#'   ),
#'   struc2str = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$S = $C->attribute("%s", "s");',
#'     '@struc = (%s);',
#'     'foreach $struc (@struc) {print $S->struc2str($struc); print "\n";};'
#'   ),
#'   cpos2str = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$Word = $C->attribute("%s", "p");',
#'     '@cpos = (%s);',
#'     'foreach $cpos (@cpos) {$token = $Word->cpos2str($cpos); print $token; print "\n";};'
#'   ),
#'   cpos2id = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$Word = $C->attribute("%s", "p");',
#'     '@cpos = (%s);',
#'     'foreach $cpos (@cpos) {$id = $Word->cpos2id($cpos); print $id; print "\n";};'
#'     # 'foreach $cpos (@cpos) {print $Word->cpos2id($cpos); print "\n";};'
#'   ),
#'   regex2id = c(
#'     'use CWB::CL;',
#'     '$C = new CWB::CL::Corpus "%s";',
#'     '$Word = $C->attribute("%s", "p");',
#'     '$regex = "%s";',
#'     '@id = $Word->regex2id($regex, "cd");',
#'     'foreach $id (@id){print $id; print "\n";};'
#'   )
#' )
#' 

#' use CWB::CL;
#' $cqp = new CWB::CQP;
#' $cqp = new CWB::CQP("-r /Users/blaette/Lab/cwb/registry", "-I /global/init.cqp");
#' @lines = $cqp->exec("Integration";);
#' unless ($cqp->ok) {
#'   @cqp_error_message = $cqp->error_message;
#'   my_error_handler();
#' }
#' @matches = $cqp->dump("Last" [, $from, $to]);
#' foreach $matches (@matches) {print $matches; print "\n"};