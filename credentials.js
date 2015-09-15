var fs = require('fs');
var url = require('url');
var solr = require('solr-client')

exports.getDatabase = function()
{
  var dbCredentials = {
    dbName : solr.createClient({core:'wn'}),
    auditDbName : solr.createClient({core:'audit'}),
    changesDbName : solr.createClient({core:'suggestions'}),
    votesDbName : solr.createClient({core:'votes'}),
    stDbName : solr.createClient({core:'st'}),
  };
  
  return dbCredentials;
}
