var fs = require('fs');
var xml = require('xml2json');
var url = require('url');
var qs = require('querystring');
var Entities = require('html-entities').AllHtmlEntities;
var entities = new Entities();
var htmlToText = require('html-to-text');

var credentials = require('../credentials.js');
var dbCredentials = credentials.getDatabase();

var cloudant = require('cloudant')(dbCredentials.url);
var wn = cloudant.use(dbCredentials.dbName);
var wnaudit= cloudant.use(dbCredentials.auditDbName);
var wnchanges = cloudant.use(dbCredentials.changesDbName);

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

function addComment(doc_id, msg, createdAt, author)
{
  var comment = {};
  comment.date = createdAt;
  comment.doc_id = doc_id;
  if (doc_id[0] == 'n')
  {
    comment.doc_type = 'nomlex';
  }
  else
  {
    comment.doc_type = 'synset';
  }

  comment.type = 'comment';
  comment.action = 'comment';
  comment.user = author;
  comment.params = msg;
  comment.status = 'new';
  comment.provenance = 'disqus';

  registerAudit('wnproposedchanges', 'add-suggestion', comment.doc_id,
                comment.type, comment.action+'('+comment.params+')',
                comment.user, comment.provenance);

  wnchanges.insert(comment,
                   null,
                   function(err)
                   {
                     if (err)
                     {
                       console.log(err);
                     }
                   });
}

function convertToJson(file)
{
  fs.readFile(file, 'utf8',
              function(err, data)
              {
                if (!err)
                {
                  var json = xml.toJson(data);
                  console.log(JSON.parse(json));
                }
                else
                  console.log("?");
              });
}

var threads = {};

function getGitHubName(author)
{
  var github = {}
  github['arademaker@gmail.com']='arademaker';
  github['elianemart@gmail.com']='(elianemart)';
  github['icerqueira@gmail.com']='(icerqueira)';
  github['livyreal@gmail.com']='livyreal';
  github['maclaudia.freitas@gmail.com']='(maclaudia.freitas)';
  github['valeria.depaiva@gmail.com']='vcvpaiva';
  
  return github[author.email];
}

function processJson(json)
{
  var obj = JSON.parse(json);
  var i = 0;
  console.log(obj.disqus.post.length);
  
  obj.disqus.thread.forEach(
    function(elt)
    {
      var l = url.parse(elt.link);
      threads[elt['dsq:id']] = qs.decode(l.query).id;
    });
  
  obj.disqus.post.forEach(
    function(elt)
    {
      if (!elt.isDeleted)
      {
        var dsq_id = elt.thread['dsq:id'];
        var doc_id = threads[dsq_id];
        var msg = htmlToText.fromString(entities.decode(entities.decode(elt.message)));
        var createdAt = Date.parse(elt.createdAt);
        var author = getGitHubName(elt.author);
        addComment(doc_id, msg, createdAt, author);
        console.log(doc_id, author);
      }
    });
}

function processXml(file)
{
  fs.readFile(file, 'utf8',
              function(err, data)
              {
                if (!err)
                {
                  var json = xml.toJson(data);
                  processJson(json);
                }
                else
                  console.log("?");
              });
}

var file = process.argv[2];

processXml(file);

// processXml('openwn-pt-2015-03-29T17-32-44.793617-all.xml');
// convertToJson('openwn-pt-2015-03-29T17-32-44.793617-all.xml');

console.log('done');
