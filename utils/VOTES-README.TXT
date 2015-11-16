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

   This will synchronize the SOLR and AGraph databases.

