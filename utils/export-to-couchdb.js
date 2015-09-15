var fs = require('fs');
var async = require('async');
var nano = require('nano')('http://localhost:5984');

var local_wn =  nano.use('wn');
var local_wnaudit = nano.use('wnaudit');
var local_wnchanges = nano.use('wnproposedchanges');
var local_wnvotes = nano.use('wnvotes');

function recreate(dbname, callback)
{
  nano.db.destroy(
    dbname,
    function(err,body) {
      nano.db.create(dbname, callback);
    });
}

/*recreate('wn');
recreate('wnaudit');
recreate('wnproposedchanges');
recreate('wnvotes');
*/
function copy(file, dbout)
{
  console.log('reading, parsing');
  var file = JSON.parse(fs.readFileSync(file, 'utf8'));
  console.log('done');

  async.eachLimit(
    file.rows, 20,
    function(item, callback)
    {
      dbout.insert(item.doc,
                   function(e,b) { callback(e); });
    },
    function (err)
    {
      console.log('done');
    });
}

copy('wnvotes_dev.json', local_wnvotes);
copy('wn_dev.json', local_wn);
copy('wnaudit_dev.json', local_wnaudit);
copy('wnproposedchanges_dev.json', local_wnchanges);

