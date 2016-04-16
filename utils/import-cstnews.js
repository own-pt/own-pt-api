var async = require('async');
var uuid = require('node-uuid');
var fs = require('fs');
var parse = require('csv-parse');

var fs = require('fs');
var path = require('path');
var _ = require('underscore');

var credentials = require('../credentials.js');
var dbCredentials = credentials.getDatabase();

var wn = dbCredentials.dbName;
var wnchanges = dbCredentials.changesDbName;
var wnaudit = dbCredentials.auditDbName;

var audits = [];
var suggestions = [];

var word_regex = /^(.+)(\w*\.+)$/;

function escapeSpecialChars(s)
{
  return s.replace(/([\+\-!\(\)\{\}\[\]\^"~\*\?:\\])/g, function(match)
    {
      return '\\' + match;
    })
    .replace(/&&/g, '\\&\\&')
    .replace(/\|\|/g, '\\|\\|');
}


function processBulkData()
{
  console.log(suggestions.length)
  console.log(audits.length)

  wnchanges.add(suggestions, null,
               function(err, data)
               {
		 wnchanges.softCommit(
                   function(err, body)
                   {
                     if (err)
                     {
                       console.log(err);
                     } else {
                       console.log('suggestions ok');
                     }
                   });
               });

  wnaudit.add(audits, null,
               function(err, data)
               {
		 wnaudit.softCommit(
                   function(err, body)
                   {
                     if (err)
                     {
                       console.log(err);
                     } else {
                       console.log('suggestions ok');
                     }
                   });
               });
}

function registerAudit(db, action, id, field, value, user, provenance)
{
  var audit = {}
  audit.id = uuid.v4();
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

function wordAlreadySuggested(id,w,callback)
{
    var query = wnchanges.createQuery()
      .q(
      {
        type: 'suggestion',
        doc_id: '"' + escapeSpecialChars(id) + '"'
      })
      .sort(
      {
        'date': 'desc'
      });

    wnchanges.search(
      query,
      function(err, body)
      {
        var contains = false;
	body.response.docs.forEach(
	  function(d)
	  {
	    if (d.action == 'add-word-pt' && d.params == w) { 
	      //console.log(w,'already added.'); 
	      contains = true; 
	    }
	  });

	callback(contains);
      });
}

function addSuggestion(id, w, file, callback)
{
  wordAlreadySuggested(id,w,
    function(alreadyAdded)
    {
      if (!alreadyAdded) 
      {
	var action = {};
	
	action.id = uuid.v4();
	action.date = Date.now();
	action.doc_id = id;
	action.doc_type = 'synset';
	action.type = 'suggestion';
	action.action = 'add-word-pt';
	action.sum_votes = 0;
        action.vote_score = 0;

	var processed_w = w.trim().replace(word_regex, '$1').trim();
	
	action.params = processed_w;
	action.user = '(system)';
	action.status = 'new';
	action.provenance = path.basename(file);
	
	registerAudit('wnproposedchanges', 'add-suggestion', action.doc_id,
                      action.type, action.action+'('+action.params+')',
                      action.user, action.provenance);

	suggestions.push(action);
      }

      callback(null);
    });
}

function processCSV(file)
{
  var parser = parse({delimiter: ',', relax: true},
                     function(err, data){
		       async.each(data,
				  function(d,callback)
				  {
				    var id = d[0].trim();
				    var w = d[1].trim();
				    addSuggestion(id,w,file,callback);
				  },
				  function(err)
				  {
				    console.log('inserting bulk data');
				    processBulkData();
				  }
				 );
                       
                     });
  
  fs.createReadStream(file).pipe(parser);
}


var file = process.argv[2];

processCSV(file);
