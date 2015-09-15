
// 00013887-a : [bastante;lauto;abundante;copioso;farto;basto;] : abundante, em grande quantidade.

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
  wnaudit.insert(audit);
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

  wnchanges.insert(action);
  console.log(id,processed_w);
}

function processLine(l, file)
{
  var result = l.match(line_regex);
  if (result) {
    var id = result[1];
    var def = result[2];
    addSuggestion(id,def,file);
  }
}

function processFile(file)
{
  var contents = fs.readFileSync(file, 'utf8');
  contents.split('\n').forEach(function(l) { processLine(l, file); });
}

if (process.argv.length <= 2)
{
  console.log("Missing file name.");
  process.exit(1);
}

var file = process.argv[2];

processFile(file);
