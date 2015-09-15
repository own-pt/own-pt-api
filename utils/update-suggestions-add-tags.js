var credentials = require('../credentials.js');
var workflow = require('../workflow.js');

var dbCredentials = credentials.getDatabase();

var dbchanges = dbCredentials.changesDbName;
var dbvotes = dbCredentials.votesDbName;

var updates = [];

function read_dbs()
{
  var updates = [];

  dbchanges.search(
    dbchanges.createQuery().q('*').rows('2000000'),
    function(err,body)
    {
      wnsuggestions = body.response.docs;
      console.log(wnsuggestions.length, 'suggestions loaded.');
      wnsuggestions.forEach(
        function(elt)
        {
          var t = workflow.getTags(elt.params);
          if (t.length > 0){
            var update = {id: elt.id,
                          tags: {set: t}};
            updates.push(update);
          }
        });

      dbchanges.update(
        updates,
        function(err, body)
        {
          dbchanges.commit();
          console.log('done.', body, err);
        });
    });
}

read_dbs();
