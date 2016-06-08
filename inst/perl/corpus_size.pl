paste(c(
  'use CWB::CL;',
  '$C = new CWB::CL::Corpus "PLPRBT";',
  '$Word = $C->attribute("word", "p");',
  '$corpus_size = $Word->max_cpos;',
  'print $corpus_size;'
), collapse=" ")