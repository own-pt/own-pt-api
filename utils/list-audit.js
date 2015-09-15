var fs = require('fs');
var _ = require('underscore');
var credentials = require('../credentials.js');
var dbCredentials = credentials.getDatabase();

var cloudant = require('cloudant')(dbCredentials.url);
var wn = cloudant.use(dbCredentials.dbName);
var wnaudit= cloudant.use(dbCredentials.auditDbName);
var wnchanges = cloudant.use(dbCredentials.changesDbName);

var id = process.argv[2];

if (id)
{
  console.log(id, dbCredentials.auditDbName);

  wnaudit.search('doc_id', 'doc_id',
               { q: 'doc_id:'+id,
                 include_docs: true,
               },
                 function(err, body)
                 {
                   console.log(body.total_rows);
                   
                   if (!err)
                   {
                     body.rows.forEach(
                       function(r)
                       {
                         console.log(r.doc.doc_id, r.doc.action, r.doc.field, r.doc.value,
                                     r.doc.user, r.doc.provenance);
                       });
                   }
                 });
} else { console.log('missing id'); }
                 
