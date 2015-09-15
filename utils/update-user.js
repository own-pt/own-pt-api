var fs = require('fs');
var _ = require('underscore');
var credentials = require('../credentials.js');
var dbCredentials = credentials.getDatabase();

var cloudant = require('cloudant')(dbCredentials.url);
var wn = cloudant.use(dbCredentials.dbName);
var wnaudit= cloudant.use(dbCredentials.auditDbName);
var wnchanges = cloudant.use(dbCredentials.changesDbName);

function update(docs)
{
  wnchanges.bulk({docs: docs}, null,
                 function(err, data)
                 {
                   if (err)
                   {
                     console.log(err);
                   } else {
                     console.log('updated ok');
                   }
                 });
}

function search(bookmark)
{
  var docs = [];

  var params = { q: 'user:"(maclaudia.freitas)" or user:"claufreitas"',
                 include_docs: true,
                 limit: 200  };

  if (bookmark) params.bookmark = bookmark;
  
  wnchanges.search('byDoc', 'byDoc', params,
                   function(err, doc)
                   {
                     if (err)
                     {
                       console.log(err);
                     }
                     else
                     {
                       console.log(doc.rows.length);
                       
                       if (bookmark !== doc.bookmark)
                       {
                         search(doc.bookmark);
                       }

                       doc.rows.forEach(
                         function(elt)
                         {
                           var d = elt.doc;
                           d.user = 'claudiafreitas';
                           docs.push(d);
                         });
                       
                     }

                     console.log('update ', docs.length, ' docs');
                     update(docs);
                   });
}

search(null);
