var credentials = require('../credentials.js');
var dbCredentials = credentials.getDatabase();
var cloudant = require('cloudant')(dbCredentials.url);
var wn = cloudant.use(dbCredentials.dbName);

if (typeof String.prototype.startsWith != 'function') {
  // see below for better implementation!
  String.prototype.startsWith = function (str){
    return this.indexOf(str) === 0;
  };
}

docs = [];

wn.list(
  function(err, body)
  {
    console.log('err ', err);
    
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

    console.log('docs ', docs.length);

    wn.bulk({docs: docs}, null,
            function(err, body)
            {
              console.log(err);
              console.log(body);
            });
  });

