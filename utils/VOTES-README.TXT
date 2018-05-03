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
   (this will take a couple of hours or days)
   wordnet: (commit-triple-store)
   
   This will synchronize the SOLR and AGraph databases.

   Now you can export the triples, as follows:
   
