var fs = require('fs');
var async = require('async');
var solr = require('solr-client');

var local_wn = solr.createClient({core:'wn'});
var local_wnaudit = solr.createClient({core:'audit'});
var local_wnchanges = solr.createClient({core:'suggestions'});
var local_wnvotes = solr.createClient({core:'votes'});
var local_st = solr.createClient({core:'st'});

/*
recreate('wn');
recreate('wnaudit');
recreate('wnproposedchanges');
recreate('wnvotes');
*/

if (typeof String.prototype.startsWith != 'function') {
  // see below for better implementation!
  String.prototype.startsWith = function (str){
    return this.indexOf(str) === 0;
  };
}

function copywn(file, dbout)
{
  dbout.autoCommit = true;
  
  console.log('reading, parsing', file);
  var file = JSON.parse(fs.readFileSync(file, 'utf8'));
  console.log('done');

  var docs = [];
  
  for (r in file.rows)
  {
    var item = file.rows[r];
    var doc = item.doc;
    doc.doc_id = doc.id;
    doc.id = doc._id;
    delete doc._id;
    delete doc._rev;

    doc.gloss_en = [ doc.gloss_en ];
    
    if (!doc.id.startsWith('_'))
    {
      docs.push(doc);
    }
  }

  dbout.add(docs,
            function(e,b)
            {
              dbout.commit(function(e,b) { dbout.optimize(); });
              console.log('err', e);
            });

}

function copy(file, dbout)
{
  dbout.autoCommit = true;
  
  console.log('reading, parsing', file);
  var file = JSON.parse(fs.readFileSync(file, 'utf8'));
  console.log('done');

  var docs = [];
  
  for (r in file.rows)
  {
    var item = file.rows[r];
    var doc = item.doc;
    doc.id = doc._id;
    delete doc._id;
    delete doc._rev;
    
    if (!doc.id.startsWith('_'))
    {
      docs.push(doc);
    }
  }

  dbout.add(docs,
            function(e,b)
            {
              dbout.commit(function(e,b) { dbout.optimize(); });
              console.log('err', e);
            });

}

function copychanges(file, dbout)
{
  dbout.autoCommit = true;
  
  console.log('reading, parsing', file);
  var file = JSON.parse(fs.readFileSync(file, 'utf8'));
  console.log('done');

  var docs = [];
  
  for (r in file.rows)
  {
    var item = file.rows[r];
    var doc = item.doc;
    doc.id = doc._id;
    delete doc._id;
    delete doc._rev;

    doc.negative_votes = [];
    doc.positive_votes = [];
    doc.negative_voters = [];
    doc.positive_voters = [];
    doc.all_voters = [];
    doc.sum_votes = 0;
    doc.vote_score = 0;
    doc.tags=[];
    doc.mentions=[];
    if (!doc.id.startsWith('_'))
    {
      docs.push(doc);
    }
  }

  dbout.add(docs,
            function(e,b)
            {
              dbout.commit(function(e,b) { dbout.optimize(); });
              console.log('err', e);
            });

}

copywn('bkp/wn.json', local_wn);
copy('bkp/wnvotes.json', local_wnvotes);
copy('bkp/wnaudit.json', local_wnaudit);
copychanges('bkp/wnproposedchanges.json', local_wnchanges);
copy('bkp/st.json', local_st);
