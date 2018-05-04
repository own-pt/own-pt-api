How to compute votes
====================

Please backup both SOLR and Allegro Graph before performing these
actions. Especially if this is your first time.

1. Run accept-reject.js

   This will select all suggestions with the appropriate scores for
   acceptance or rejection and mark them as such.

2. Run commit.js

   This will effectively modify the WN index with all accepted
   suggestions (ie, adding/removing words, glosses and examples.)

3. Over in WORDNET-EDITOR, execute the method (PROCESS-SUGGESTIONS).

   $ /usr/local/agraph/bin/agraph-backup backup wn30 <filename>
   $ mlisp
   cl-user: (ql:quickload :wordnet-editor)
   cl-user: (in-package :wordnet)
   wordnet: (open-triple-store "wn30")
   wordnet: (process-suggestions)
   (this will take approximatelly 8 hours to finalize)
   wordnet: (commit-triple-store)
   
   This will synchronize the SOLR and AGraph databases.

   Now you can export the triples, as follows:
   wordnet: (export-all)

   This will generate N-triples files in /tmp.  You can then take the
   new triples and release a new version of Openwordnet-PT with it.
   Notice that the whole vote processing system only affects the
   OWN-PT.NT file, so there's no need to update the other files
   generated.
   
