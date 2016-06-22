.cqi_perl = list(
  
  corpus_size = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "PLPRBT";',
    '$Word = $C->attribute("word", "p");',
    '$corpus_size = $Word->max_cpos;',
    'print $corpus_size;'
  ),
  
  attribute_size = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$Word = $C->attribute("%s", "p");',
    '$lex_size = $Word->max_id;',
    'print $lex_size;'
  ),
  
  str2freq = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$Word = $C->attribute("word", "p");',
    '$id = $Word->str2id("internet");',
    '$f = $Word->id2freq($id);',
    'print $f;'
  ),
  
  id2freq = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$Word = $C->attribute("%s", "p");',
    '@id = (%s);',
    'foreach $id (@id) {print $Word->id2freq($id); print "\n";};'
    # '@freq = $Word->idlist2freq(@id);',
    # 'foreach $freq (@freq){print $freq; print "\n";};'
  ),
  
  id2cpos = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$Word = $C->attribute("%s", "p");',
    '$id = (%s);',
    '@cpos = $Word->id2cpos($id);',
    'foreach $cpos (@cpos) {print $cpos; print "\n";};'
  ),
  
  id2str = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$Word = $C->attribute("%s", "p");',
    '@id = (%s);',
    'foreach $id (@id) {print $Word->id2str($id); print "\n";};'
  ),
  
  str2id = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$Word = $C->attribute("%s", "p");',
    # '@str = ("Migration", "Integration");',
    '@str = (%s);',
    # '@foo = (%s);',
    'foreach $str (@str) {print $Word->str2id($str); print "\n";};'
  ),
  
  cpos2struc = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$S = $C->attribute("%s", "s");',
    '@cpos = (%s);',
    'foreach $cpos (@cpos) {$num = $S->cpos2struc($cpos); print $num; print "\n";};'
  ),
  
  struc2cpos = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$S = $C->attribute("%s", "s");',
    '@struc = (%s);',
    # 'foreach $struc (@struc) {$num = $S->struc2cpos($struc); print $num; print "\n";};'
    'foreach $struc (@struc) {@cpos = $S->struc2cpos($struc); foreach $cpos (@cpos) {print $cpos; print "\n";}};'
  ),
  
  struc2str = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$S = $C->attribute("%s", "s");',
    '@struc = (%s);',
    'foreach $struc (@struc) {print $S->struc2str($struc); print "\n";};'
  ),
  
  cpos2str = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$Word = $C->attribute("%s", "p");',
    '@cpos = (%s);',
    'foreach $cpos (@cpos) {$token = $Word->cpos2str($cpos); print $token; print "\n";};'
  ),
  
  cpos2id = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$Word = $C->attribute("%s", "p");',
    '@cpos = (%s);',
    'foreach $cpos (@cpos) {$id = $Word->cpos2id($cpos); print $id; print "\n";};'
    # 'foreach $cpos (@cpos) {print $Word->cpos2id($cpos); print "\n";};'
  ),
  
  regex2id = c(
    'use CWB::CL;',
    '$C = new CWB::CL::Corpus "%s";',
    '$Word = $C->attribute("%s", "p");',
    '$regex = "%s";',
    '@id = $Word->regex2id($regex, "cd");',
    'foreach $id (@id){print $id; print "\n";};'
  )
)

#' @rdname CQI
#' @export CQI.perl
CQI.perl <- R6Class(
  
  "CQI.perl",
  inherit = CQI.super,
  public = list(
    
    as.cmd = function(cmd) paste("perl -e '", paste(cmd, collapse = " "), "'", sep=""),
    
    list_corpora = function() stop("list_corpora not implemented for perl interface"),
    
    attribute_size = function(corpus, attribute){
      as.integer(system(sprintf(self$as.cmd(.cqi_perl[["attribute_size"]]), corpus, attribute), intern=TRUE))
    },
    
    lexicon_size = function(corpus, pAttribute){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["lexicon_size"]]), corpus, pAttribute),
          intern=TRUE
          )
        )
      },
    
    cpos2struc = function(corpus, pAttribute, cpos){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["cpos2struc"]]), corpus, pAttribute, paste(as.character(cpos), collapse=",")),
          intern=TRUE)
        )
      },
    
    cpos2str = function(corpus, pAttribute, cpos){
      system(
        sprintf(self$as.cmd(.cqi_perl[["cpos2str"]]), corpus, pAttribute, paste(as.character(cpos), collapse=",")),
        intern=TRUE
        )
      },
    
    cpos2id = function(corpus, pAttribute, cpos){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["cpos2id"]]), corpus, pAttribute, paste(as.character(cpos), collapse=",")),
          intern=TRUE)
        )
      },
    
    struc2cpos = function(corpus, sAttribute, struc){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["struc2cpos"]]), corpus, sAttribute, struc),
          intern=TRUE)
        )
      },
    
    id2str = function(corpus, pAttribute, id){
      system(
        sprintf(self$as.cmd(.cqi_perl[["id2str"]]), corpus, pAttribute, paste(as.character(id), collapse=",")),
        intern=TRUE
        )
      },
    
    struc2str = function(corpus, sAttribute, struc){
      system(
        sprintf(
          self$as.cmd(.cqi_perl[["struc2str"]]), corpus, sAttribute, paste(as.character(struc), collapse=",")),
        intern=TRUE
        )
      },
    
    regex2id= function(corpus, pAttribute, regex){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["regex2id"]]), corpus, pAttribute, regex),
          intern=TRUE
        )
      )
    },
    
    str2id = function(corpus, pAttribute, str){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["str2id"]]), corpus, pAttribute, paste('"', str, '"', sep="", collapse=",")),
          intern=TRUE
        )
      )
    },
    
    id2freq = function(corpus, pAttribute, id){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["id2freq"]]), corpus, pAttribute, paste(id, collapse=",")),
          intern=TRUE
        )
      )
    },
    
    id2cpos = function(corpus, pAttribute, id){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["id2cpos"]]), corpus, pAttribute, paste(id, collapse=",")),
          intern=TRUE
          )
        )
      },
    
    cpos2lbound = function(corpus, sAttribute, cpos){
      message("cpos2lbound not implemented in the Perl-CL-Package")
      },
    
    cpos2rbound = function(corpus, sAttribute, cpos){
      message("cpos2rbound not implemented in the Perl-CL-Package")
      },
    
    query = function(){
      message("query not yet implemented")
      },
    
    dump_subcorpus = function(){
      message("dump_subcorpus not yet implemented")
      }
  )
)
