var fs = require('fs');
var parse = require('csv-parse');

var fs = require('fs');
var path = require('path');
var _ = require('underscore');
var credentials = require('../credentials.js');
var dbCredentials = credentials.getDatabase();

var cloudant = require('cloudant')(dbCredentials.url);
var wn = cloudant.use(dbCredentials.dbName);
var wnaudit= cloudant.use(dbCredentials.auditDbName);
var wnchanges = cloudant.use(dbCredentials.changesDbName);

var line_regex = /^(........-.) : \[.+\] : (.+)$/;
var word_regex = /^(.+)(\w*\.+)$/;

var audits = [];
var suggestions = [];

function processBulkData()
{
  wnchanges.bulk({docs: suggestions}, null,
               function(err, data)
               {
                 if (err)
                 {
                   console.log(err);
                 } else {
                   console.log('suggestions ok');
                 }
               });

  wnaudit.bulk({docs: audits}, null,
               function(err, data)
               {
                 if (err)
                 {
                   console.log(err);
                 } else {
                   console.log('audits ok');
                 }
               });
}

function registerAudit(db, action, id, field, value, user, provenance)
{
  var audit = {}
  audit.date = Date.now();
  audit.db = db;
  audit.action = action;
  audit.doc_id = id;
  audit.field = field;
  audit.value = value;
  audit.user = user;
  audit.provenance = provenance;
  audits.push(audit);
}

function addSuggestion(id, w, file)
{
  var action = {};
  action.date = Date.now();
  action.doc_id = id;
  action.doc_type = 'synset';
  action.type = 'suggestion';
  action.action = 'add-gloss-pt';

  var processed_w = w.trim().replace(word_regex, '$1').trim();
  
  action.params = processed_w;
  action.user = '(system)';
  action.status = 'new'; // new accepted not-accepted committed
  action.provenance = path.basename(file);

  registerAudit('wnproposedchanges', 'add-suggestion', action.doc_id,
                action.type, action.action+'('+action.params+')',
                action.user, action.provenance);

  suggestions.push(action);
}

function processCSV(file)
{
  var parser = parse({delimiter: '\t', relax: true},
                     function(err, data){
                       data.forEach(
                         function(d)
                         {
                           // por-30-80000976-n
                           // 01234567890123456
                           var tmp = d[0].trim();
                           var id = tmp.substring(7, tmp.length);
                           var glos = d[6].trim();
                           if (id[0] !== '8')
                             if (id.length > 0 && glos.length > 0)
                           {
                             addSuggestion(id,glos,file);
                           }
                         }
                       );
                       
                       console.log('inserting bulk data');
                       processBulkData();
                     });
  
  fs.createReadStream(file).pipe(parser);
}


var file = process.argv[2];

processCSV(file);
