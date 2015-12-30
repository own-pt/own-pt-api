List of files in this directory.  Files with [oudated] in them were
written for the Cloudant version of the databases and haven't been
updated to Solr.

Files used in the regular operation of the site
===============================================

accept-reject.js: scans the list of suggestions and accepts or rejects
those that satisfy the voting criteria;

commit.js: commit all accepted suggestions to the wordnet database.

Files used in the batch processing of submissions
=================================================

import-alberto.js, import-disqus.js, import-hugo-definicoes.js,
import-hugo-palavras.js: [oudated] various one-off script files used
to import suggestions and comments from various sources.

import-watson-translation.js: import watson translation of gloses.

translate-glosses.lisp: translate gloses

Files used in backup/restore/other maintenance tasks
====================================================

export-to-couchdb.js
export-to-solr.js
restore-solr.js
update-suggestions.js

Other files (test scripts, etc.)
================================

tally_votes_file.js: [outdated] compute votes for all suggestions
(uses Cloudant dump file)

check-wn.js: [outdated] reads the original synset from Wordnet and
check if they are present in the database.

clean-wn.js, clean-st.js, clean-activities.js: [oudated] delete all
documents from those databases that are not design documents.

dump-st.js: [outdated] extract the contents of the st database into a
CSV file.

list-audit.js: [outdated] list all audit logs for a particular id

update-user.js: [outdated] fix the user of suggestions.

find-duplicate-suggestions.js: finds synsets with duplicated suggestions.

polysemy-case-sensitive.js: extract and count polysemic words that
start with lower and upper case.

simple-stats-polysemy.js, simple-stats.js: generate the stats shown in
WNSTATS.
