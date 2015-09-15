var fs = require('fs');
var async = require('async');
var solr = require('solr-client');

function restore(db, filename)
{
  var solrdb = solr.createClient({core:db});
  console.log('reading . . .');
  var file = JSON.parse(fs.readFileSync(filename, 'utf8'));
  solrdb.deleteAll(
    function(err, body)
    {
      var docs = [];
      file.response.docs.forEach(
        function(d)
        {
          delete d._version_;
          docs.push(d);
        });
      
      console.log('adding . . .', err);
      solrdb.add(
        docs,
        function(err, body)
        {
          console.log('commit . . .', err);
          solrdb.commit(
            function(err, body)
            {
              console.log('optimizing . . .', err);
              solrdb.optimize(
                function(err, body)
                {
                  console.log('done: ', err);
                });
            });
        });
    });
}

if (process.argv.length == 4)
{
  restore(process.argv[2], process.argv[3]);
}
