Example of how to trigger a replication/backup:

localhost:8983/solr/wn/replication?command=backup

This will generate a directory called "snapshot-XXXXX" inside the
collections `data' directory.

To restore, simply replace the `data/index' directory with the
contents of a snapshot.

///

To have a completely external copy of the contents of a collection,
you can download it like so:

curl "http://localhost:8983/solr/wn/select?q=*%3A*&wt=json&indent=true&start=0&rows=2000000" 

