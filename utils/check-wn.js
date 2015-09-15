var fs = require('fs');

var credentials = require('../credentials.js');
var dbCredentials = credentials.getDatabase();
var cloudant = require('cloudant')(dbCredentials.url);
var wn = cloudant.use(dbCredentials.dbName);

if (process.argv.length <= 2)
{
  console.log("Missing dir.");
  process.exit(1);
}

function processData(dir, dataFile, suffix)
{
  var content = fs.readFileSync(dir+dataFile, 'utf8');

  content.split('\n').forEach(
    function(l)
    {
      if (l[0] !== ' ')
      {
        var id = l.split(' ')[0]+'-'+suffix;

        wn.search('txt','id',
                  { q: '"'+id+'"' },
                  function(err, doc)
                  {
                    if (err)
                    {
                      console.log('error ' + id);
                      console.log(err);
                    }
                    else {
                      if (doc.total_rows !== 1)
                      {
                        console.log('problem with ' + id);
                        console.log(doc);
                      }
                    }
                  });
      }
    });
}

var wn_dir = process.argv[2];

processData(wn_dir, 'data.adj', 'a');
processData(wn_dir, 'data.adv', 'r');
processData(wn_dir, 'data.noun', 'n');
processData(wn_dir, 'data.verb', 'v');
