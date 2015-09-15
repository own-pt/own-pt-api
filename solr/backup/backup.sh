#!/bin/bash

SUF=`date +%Y-%m-%d-%H%M%S`

curl -s -S "http://localhost:8983/solr/wn/select?q=*%3A*&wt=json&indent=true&start=0&rows=2000000" > wn-${SUF}.json
curl -s -S "http://localhost:8983/solr/st/select?q=*%3A*&wt=json&indent=true&start=0&rows=2000000" > st-${SUF}.json
curl -s -S "http://localhost:8983/solr/audit/select?q=*%3A*&wt=json&indent=true&start=0&rows=2000000" > audit-${SUF}.json
curl -s -S "http://localhost:8983/solr/suggestions/select?q=*%3A*&wt=json&indent=true&start=0&rows=2000000" > suggestions-${SUF}.json
curl -s -S "http://localhost:8983/solr/votes/select?q=*%3A*&wt=json&indent=true&start=0&rows=2000000" > votes-${SUF}.json

gzip -9 wn-${SUF}.json
gzip -9 st-${SUF}.json
gzip -9 audit-${SUF}.json
gzip -9 suggestions-${SUF}.json
gzip -9 votes-${SUF}.json
