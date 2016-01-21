// 08773880-n | a city in northwestern Germany and an important Baltic port; a leading member of the Hanseatic League | Uma cidade no noroeste da Alemanha e um porto báltico importante; um membro líder da Liga Hanseática  | g 

var fs = require('fs');
var path = require('path');
var _ = require('underscore');
var credentials = require('../credentials.js');
var dbCredentials = credentials.getDatabase();
var uuid = require('node-uuid');

var wn = dbCredentials.dbName;
var wnchanges = dbCredentials.changesDbName;
var audit = require('../audit.js');

var line_regex = /^(.+)\|(.+)\|(.+)\|(.+)$/;

var count = 0;

function addSuggestion(id, w, file, g_or_e)
{
  if (count++ % 10000 == 0)
  {
    console.log(count, 'suggestions added.');
  }

  var action = {};
  action.id = uuid.v4();
  action.date = Date.now();
  action.doc_id = id;
  action.doc_type = 'synset';
  action.type = 'suggestion';

  if (g_or_e === 'g')
  {
    action.action = 'add-gloss-pt';
  }
  else 
  if (g_or_e === 'e')
  {
    action.action = 'add-example-pt';
  } else {
    action.action = 'unknown';
  }

  action.sum_votes = 0;
  action.vote_score = 0;

  action.params = w;
  action.user = '(system)';
  action.status = 'new'; // new accepted not-accepted committed
  action.provenance = path.basename(file);

  audit.registerAudit('suggestions', 'add-suggestion', action.doc_id,
                action.type, action.action+'('+action.params+')',
                action.user, action.provenance);

  wnchanges.add(action, 
                function(err,body)
                {
                  if (err) console.log(err);
                  wnchanges.softCommit();
                });
}

function processLine(l, file)
{
  var result = l.match(line_regex);
  if (result)
  {
    var id = result[1].trim();
    var def = result[3].trim();
    var g_or_e = result[4].trim();
    addSuggestion(id,def,file,g_or_e);
  }
}

function processFile(file)
{
  console.log('reading file...');
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
