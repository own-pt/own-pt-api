var credentials = require('../credentials.js');
var dbCredentials = credentials.getDatabase();
var cloudant = require('cloudant')(dbCredentials.url);
var wnaudit = cloudant.use(dbCredentials.auditDbName);
var wnchanges = cloudant.use(dbCredentials.changesDbName);

if (typeof String.prototype.startsWith != 'function') {
  // see below for better implementation!
  String.prototype.startsWith = function (str){
    return this.indexOf(str) === 0;
  };
}

docs = [];

wnchanges.list(
  function(err, body)
  {
    body.rows.forEach(
      function(doc)
      {
        if (!doc.id.startsWith('_'))
        {
          var id = doc.id;
          var re = doc.value.rev;

          docs.push({_id: id, _rev: re, _deleted: true});
        }
      });

    wnchanges.bulk({docs: docs}, null,
                   function(err, body)
                   {
                     console.log(err);
                     console.log(body);
                   });

  });

docs = [];

wnaudit.list(
  function(err, body)
  {
    body.rows.forEach(
      function(doc)
      {
        if (!doc.id.startsWith('_'))
        {
          var id = doc.id;
          var re = doc.value.rev;
          
          docs.push({_id: id, _rev: re, _deleted: true});
        }
      });

    wnaudit.bulk({docs: docs}, null,
            function(err, body)
            {
              console.log(err);
              console.log(body);
            });

  });

