var credentials = require('../credentials.js');
var workflow = require('../workflow.js');

var dbCredentials = credentials.getDatabase();

var db = dbCredentials.dbName;

var updates = [];

var update = {id:'7d5cb8cbc0b8af89f864b5e26b3a673d',
              nomlex_verb: {set:'invocar'},
              doc_id:{set:'nm-pt:nomlex-invocar-invocação'}};

db.update([update],
          function(err,body)
          {
            db.commit();
          });
