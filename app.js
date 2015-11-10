var async = require('async');
var fs = require('fs');
var url = require('url');

var credentials = require('./credentials.js');
var solr2cloudant = require('./solr2cloudant.js');

var dbCredentials = credentials.getDatabase();

// var waCredentials = credentials.getWatson();

var wn = dbCredentials.dbName;
var wnchanges = dbCredentials.changesDbName;

var _ = require('underscore');

var wordnet = require('./wordnet.js');
var workflow = require('./workflow.js');
var sense = require('./sense.js');
var express = require('express')
var app = express();
var api_key = process.env.API_KEY;

var DEBUG = false;

if (typeof String.prototype.startsWith != 'function') {
  // see below for better implementation!
  String.prototype.startsWith = function (str){
    return this.indexOf(str) === 0;
  };
}

function escapeSpecialChars(s){
  return s.replace(/([\+\-!\(\)\{\}\[\]\^"~\*\?:\\])/g, function(match) {
    return '\\' + match;
  })
  .replace(/&&/g, '\\&\\&')
  .replace(/\|\|/g, '\\|\\|');
}


function fixCounts(counts)
{
  var new_counts = {};

  for(p in counts)
  {
    new_counts[p] = [];
    for (var i = 0; i < counts[p].length; i+= 2)
    {
      new_counts[p].push({name: counts[p][i], count: counts[p][i+1]});
    }
  }

  return new_counts;
}

function addExpandedInfo(s, item, fieldNameExpanded, callback)
{
  workflow.getDocument(
    item,
    function(err, elt)
    {
      if (elt != null)
      {
        s[fieldNameExpanded].push(
          { 'name': wordnet.getWord(elt), 'id': item });
      }
      
      callback(null);
    });
}

function processSynset(s, fieldName, callback)
{
  if (s && s.hasOwnProperty(fieldName))
  {
    // normalize values that came from Cloudant as single values
    // into an array with a single value.
    if (s[fieldName].constructor !== Array)
    {
      s[fieldName] = [s[fieldName]];
    }
    
    var fieldNameExpanded = fieldName + 'Expanded';
    s[fieldNameExpanded] = [];
    
    async.each(s[fieldName],
               function(item, callback)
               {
                 addExpandedInfo(s, item, fieldNameExpanded, callback);
               },
               function(err)
               {
                 callback(null);
               });
  }
  else
  {
    callback(null);
  }
}

function makeNomlexSearch(term)
{
  return '(nomlex_noun:"'+term+'" nomlex_verb:"'+term+'" nomlex_plural:"'+term+'")';
}

function makeNomlexSearchMultipleTerms(terms)
{
  return wn.createQuery().q(terms.map(makeNomlexSearch).join(' OR '));
}

function makeSynsetSearch(nl, property)
{
  return '(word_pt:'+nl[property]+')';
}

function simpleSummaryOfSynset(s)
{
  return { words: wordnet.getWordsPt(s),
           doc_id: s.doc_id,
           id: s.id}
}

function complexSummaryOfSynset(s)
{
  return { words: wordnet.getWords(s, 'array'),
           gloss: wordnet.getGlosses(s),
           doc_id: s.doc_id,
           id: s.id}
}

function searchSynsets(query, drilldown, getSynsetInfo, callback)
{
  var query = wn.createQuery().q(query);

  var fqs = solr2cloudant.GetFQArray(drilldown);
          
  if (fqs)
  {
    fqs.forEach(
      function(elt)
      {
        query = query.matchFilter(elt.field, elt.value);
      });
  }

  wn.search(query,
            function(err, doc)
            {
              var synsets=[];
              if (!err)
              {
                if (doc.response.numFound > 0) {
                  synsets =
                    doc.response.docs.map(
                      function(elt)
                      {
                        return getSynsetInfo(elt);
                      });
                }
              }

              callback(err, synsets);
            });
}

function addRelatedSynsetsForProperty(s, property, callback)
{
  var query = makeSynsetSearch(s, property);

  searchSynsets(
    query,
    null,
    simpleSummaryOfSynset,
    function(err, synsets)
    {
      if(!err)
        s['relatedSynsets_'+property] = synsets;
      callback(null);
    });
}

function addRelatedSynsets(s, callback)
{
  if (s)
  {
    var properties = ['nomlex_noun', 'nomlex_verb'];

    async.each(properties,
               function(item, callback)
               {
                 addRelatedSynsetsForProperty(s, item, callback);
               },
               function(err)
               {
                 callback(s);
               });
  }
  else
  {
    s = { error: 'error' };
    callback(s);
  }
}

function addRelatedNomlexes(s, callback)
{
  if (s && s.word_pt && s.word_pt.length > 0)
  {
    var query = makeNomlexSearchMultipleTerms(s.word_pt);
    wn.search(query,
              function(err, doc)
              {
                if (!err)
                {
                  if (doc.response.numFound > 0)
                  {
                    var ids = doc.response.docs.map(
                      function(elt) { return elt.doc_id; });

                    s.relatedNomlexes = ids;
                  }
                }
                
                callback(s);
              });
  }
  else
  {
    if (!s)
    {
      s = { error: 'error' };
    }
    callback(s);
  }
}

function fetchSynset(id, callback)
{
  var fields = wordnet.getPredicates();

  workflow.getDocument(
    id,
    function(errDoc, s)
    {
      async.each(fields,
                 function(item, callback)
                 {
                   processSynset(s, item, callback);
                 },
                 function(err)
                 {
                   wordnet.normalizeFields(s);
                   addRelatedNomlexes(s, callback);
                 });
    });
}

function fetchNomlex(id, callback)
{
  workflow.getDocument(
    id,
    function(err, s)
    {
      addRelatedSynsets(s, callback);
    });
}

app.use(express.responseTime());

app.get('/predicates',
        function(req, res)
        {
          res.json(wordnet.getPredicates());
        });

app.get('/',
        function(req,res)
        {
          res.json({version: '43-solr',
                    has_key: api_key != null,
                    date: new Date() });
        });

app.get('/get-suggestions/:id',
        function(req, res)
        {
          console.time("get-suggestions");
          var query = wnchanges.createQuery()
              .q({type: 'suggestion', doc_id: req.params.id})
              .sort({'date':'desc'});
          wnchanges.search(
            query,
            function(err, solr_body)
            {
              var body = solr2cloudant.convertSearchResults(solr_body);
              
              async.each(
                body.rows,
                function(item, callback)
                {
                  console.time("get-votes");
                  workflow.getVotes(
                    item.doc.id,
                    function(err, votes)
                    {
                      console.timeEnd("get-votes");
                      if (!err)
                      {
                        item.doc.votes = votes;
                      }

                      callback(null);
                    });
                },
                function(err)
                {
                  console.timeEnd("get-suggestions");
                  res.json(body);
                }
              );
            });
        });

app.get('/get-comments/:id',
        function(req, res)
        {
          console.time("get-comments");
          var query = wnchanges.createQuery()
              .q({type: 'comment', doc_id: req.params.id})
              .sort({'date': 'desc'});
          
          wnchanges.search(
            query,
            function(err, solr)
            {
              var doc = solr2cloudant.convertSearchResults(solr);
              console.timeEnd("get-comments");
              if (err) res.json(err);
              else res.json(doc);
            });
        });

app.get('/search-documents',
        function(req, res)
        {
          console.time("search-documents");
          var query = wn.createQuery();
          query = query.q(req.param('q'));
          query = query.facet(
            {on:true,
             field: ['wn30_lexicographerFile','rdf_type','wn30_frame',
                     'word_count_pt', 'word_count_en'] ,
             mincount: 1});

          if (req.param('fl'))
          {
            query = query.fl(req.param('fl'));
          }

          if (req.param('limit'))
          {
            query = query.rows(req.param('limit'));
          }

          if (req.param('start'))
          {
            query = query.start(req.param('start'));
          }

          var fqs = solr2cloudant.GetFQArray(req.param('drilldown'));
          
          if (fqs)
          {
            fqs.forEach(
              function(elt)
              {
                query = query.matchFilter(elt.field, '"'+escapeSpecialChars(elt.value)+'"');
              });
          }

          wn.search(
            query,
            function(err, doc)
            {
              if (err)
              {
                res.json(err);
              }
              else
              {
                doc = solr2cloudant.convertSearchResults(doc);
                
                doc.counts = fixCounts(doc.counts);

                console.timeEnd("search-documents");
                
                res.json(doc);
              }
            });
        });

app.get('/search-activities',
        function(req, res)
        {
          console.time("search-activities");

          var query = wn.createQuery();

          query = query.q(req.param('q'));
          query = query.facet(
            {on:true,
             field: ['type','action','status','doc_type','user','provenance','tags','sum_votes','vote_score'],
             mincount: 1});

          if (req.param('limit'))
          {
            query = query.rows(req.param('limit'));
          } else {
            query = query.rows(25);
          }

          if (req.param('fl'))
          {
            query = query.fl(req.param('fl'));
          }
          
          if (req.param('start'))
          {
            query = query.start(req.param('start'));
          }
          
          var fqs = solr2cloudant.GetFQArray(req.param('drilldown'));
          
          if (fqs)
          {
            fqs.forEach(
              function(elt)
              {
                query = query.matchFilter(elt.field, escapeSpecialChars(elt.value));
              });
          }

          if (req.param('sf'))
          {
            var sf = req.param('sf');
            var order = req.param('so');
            if (!order) { order = 'desc'; }
            var sort_info = {};
            sort_info[sf] = order;
            query = query.sort(sort_info);
          }
          else {
            query = query.sort({'date':'desc'});
          }
          
          wnchanges.search(
            query,
            function(err, solr_doc)
            {
              if (err)
              {
                res.json(err);
              }
              else
              {
                var doc = solr2cloudant.convertSearchResults(solr_doc);

                doc.counts = fixCounts(doc.counts);

                console.timeEnd("search-activities");
                
                res.json(doc);
              }
            });
        });

app.get('/accept-suggestion/:id',
        function(req,res)
        {
          if (req.param('key') != api_key)
          {
            console.log('unauthorized');
            
            res.json({ 'status': 'unauthorized'});
          }
          else
          {
            console.time("accept-suggestion");
            workflow.acceptSuggestion(
              req.params.id,
              function(err)
              {
                console.timeEnd("accept-suggestion");
                
                if (!err)
                {
                  res.json({'status' : 'suggestion-accepted'});
                } else
                {
                  res.json({'status' : 'error', 'error': err});
                }
              });
          }
        });

app.get('/reject-suggestion/:id',
        function(req,res)
        {
          if (req.param('key') != api_key)
          {
            res.json({ 'status': 'unauthorized'});
          }
          else
          {
            console.time("reject-suggestion");
            
            workflow.rejectSuggestion(
              req.params.id,
              function(err)
              {
                console.timeEnd("reject-suggestion");
                
                if (!err)
                {
                  res.json({'status' : 'suggestion-not-accepted'});
                } else
                {
                  res.json({'status' : 'error', 'error': err});
                }
              });
          }
        });

app.get('/delete-suggestion/:id',
        function(req, res)
        {
          if (req.param('key') != api_key)
          {
            res.json({ 'status': 'unauthorized'});
          }
          else
          {
            console.time("delete-suggestion");
            
            workflow.deleteSuggestion(
              req.params.id,
              function(err)
              {
                console.timeEnd("delete-suggestion");
                
                if (!err)
                {
                  res.json({'status' : 'suggestion-removed'});
                } else
                {
                  res.json({'status' : 'error', 'error': err});
                }
              });
          }
        });

app.get('/delete-comment/:id',
        function(req, res)
        {
          if (req.param('key') != api_key)
          {
            res.json({ 'status': 'unauthorized'});
          }
          else
          {
            console.time("delete-comment");
            workflow.deleteComment(
              req.params.id,
              function(err)
              {
                console.timeEnd("delete-comment");
                if(!err)
                {
                  res.json({'status' : 'comment-removed'});
                }else
                {
                  res.json({'status' : 'error', 'error': err});
                }
              });
          }
        });

app.get('/add-suggestion/:id',
        function(req, res)
        {
          if (req.param('key') != api_key)
          {
            res.json({ 'status': 'unauthorized'});
          }
          else
          {
            console.time("add-suggestion");
            
            workflow.addSuggestion(
              Date.now(),
              req.params.id,
              req.param('doc_type').trim(),
              req.param('suggestion_type').trim(),
              req.param('params').trim(),
              req.param('user').trim(),
              'web',
              true,
              function(err, body)
              {
                console.timeEnd("add-suggestion");
                
                res.json({'status' : 'suggestion-added'});
              });
          }
        });

app.get('/add-comment/:id',
        function(req, res)
        {
          if (req.param('key') != api_key)
          {
            res.json({ 'status': 'unauthorized'});
          }
          else
          {
            console.time("add-comment");
            
            workflow.addComment(
              Date.now(),
              req.params.id,
              req.param('doc_type').trim(),
              req.param('user').trim(),
              req.param('text'), 'web');

            console.timeEnd("add-comment");
            
            res.json({'status' : 'comment-added'});
          }
        });

app.get('/synset/:id',
        function(req, res)
        {
          console.time("synset");
          
          fetchSynset(req.params.id,
                      function(s)
                      {
                        console.timeEnd("synset");
                        res.json(s);
                      });
        });

app.get('/nomlex/:id',
        function(req, res)
        {
          var id = req.params.id;
          fetchNomlex(id,
                      function(s)
                      {
                        res.json(s);
                      });
        });

app.get('/add-vote/:id',
        function(req, res)
        {
          if (req.param('key') != api_key)
          {
            res.json({ 'status': 'unauthorized'});
          }
          else
          {
            console.time("add-vote");
            
            var suggestion_id = req.params.id;
            var user = req.param('user');
            var value = req.param('value');
            workflow.addVote(
              suggestion_id, user, value,
              function(err, data)
              {
                console.timeEnd("add-vote");

                if (err)
                {
                  res.json({'status': 'error', 'error': err});
                }
                else
                {
                  res.json({'status': 'vote-added', 'id': data.id});
                }
              });
          }
        });

app.get('/delete-vote/:id',
        function(req, res)
        {
          if (req.param('key') != api_key)
          {
            res.json({ 'status': 'unauthorized'});
          }
          else
          {
            console.time("delete-vote");
            
            var vote_id = req.params.id;
            workflow.deleteVote(
              vote_id,
              function(err, body)
              {
                console.timeEnd("delete-vote");
                
                if (err)
                {
                  res.json({'status': 'error', 'error': err});
                }
                else
                {
                  res.json({'status': 'vote-deleted', 'id': vote_id});
                }
              });
          }
        });

app.get('/pointers',
        function(req, res)
        {
            var synset = req.param('synset');
            var word = req.param('word');
            
            workflow.getPointers(synset,word,
                                 function(err, pointers)
                                 {
                                     res.json(pointers);
                                 });
        });

app.get('/statistics',
        function(req, res)
        {
          var fields = ['wn30_lexicographerFile','rdf_type'];
          wn.search(
            wn.createQuery().q('-rdf_type:Nominalization')
              .facet({on:true,
                      field: fields,
                      mincount: 1}),
            function(err, body)
            {
              var facets = body.facet_counts.facet_fields;
              var stats = {};
              var queries = [];
              
              fields.forEach(
                function(f)
                {
                  stats[f] = {};
                  for(i = 0; i < facets[f].length; i += 2)
                  {
                    var fvalue = facets[f][i];
                    var fcount = facets[f][i+1];
                    stats[f][fvalue] = {total: fcount};
                    queries.push(
                      {facet: f,
                       facet_value: fvalue,
                       q: wn.createQuery().q('*:*').rows(0).
                       matchFilter(f, fvalue).
                       matchFilter('word_count_pt', '0') });
                  }
                });
              
              async.each(
                queries,
                function(item, callback)
                {
                  wn.search(
                    item.q,
                    function(err, body)
                    {
                      stats[item.facet][item.facet_value].total_pt =
                        stats[item.facet][item.facet_value].total - body.response.numFound;
                      callback();
                    });
                },
                function()
                {
                  var stats_table = [];
                  for(facet in stats)
                  {
                    var facet_stats = [];
                    for (value in stats[facet])
                    {
                      facet_stats.push({value: value, total: stats[facet][value].total, total_pt: stats[facet][value].total_pt});
                    }
                    
                    stats_table.push(
                      { facet: facet, stats: facet_stats });
                  }
                  
                  res.json(stats_table);
                });
            });
        });

app.get('/get-sense-tagging-suggestion',
        function(req,res)
        {
          var userid = req.param('userid');
          var file = req.param('file');
          var text = req.param('text');
          var word = req.param('word');
          sense.getSuggestion(
            file,text,word,userid,
            function(err,body)
            {
              if (!err)
                res.json({'selection': body.response.docs[0] });
              else
                res.json({'error': err});
            });
        });        

app.get('/sense-tagging-process-suggestion',
        function(req,res)
        {
          var userid = req.param('userid');
          var file = req.param('file');
          var text = req.param('text');
          var word = req.param('word');
          var selection = req.param('selection');
          var comment = req.param('comment');
          var suggestion = {
            userid: userid,
            file: file,
            text: text,
            word: word,
            selection: selection,
            comment: comment,
            suggestion: suggestion };
          sense.addSuggestion(
            suggestion,
            function(err,body)
            {
              res.json({'status':'ok'});
            });
        });

app.get('/sense-tagging',
        function(req,res)
        {
          var file = req.param('file');
          var bosque = JSON.parse(fs.readFileSync(file, 'utf8'));
          res.json(bosque);
        });

app.get('/sense-tagging-detail',
        function(req,res)
        {
          var file=req.param('file');
          var ntext=req.param('text');
          var nword=req.param('word');
          var bosque = JSON.parse(fs.readFileSync(file, 'utf8'));
          var text = bosque[ntext];
          var word = bosque[ntext].words[nword];

          var result = {}
          result.text = text.text;
          result.word = word;
          
          var query = ('word_pt:"'+word.lemma+'"');
                    
          searchSynsets(
            query,
            JSON.stringify(['rdf_type','NounSynset']),
            complexSummaryOfSynset,
            function(e, synsets)
            {
              if (!e)
                result.lemma_synsets = synsets;

              res.json(result);
            });

        });

app.get('/list-synsets/:type',
        function(req,res)
        {
            console.log('type');
            var query = wn.createQuery()
                .q('*:*')
                .matchFilter("rdf_type", req.params.type)
                .fl(["word_en","word_pt","doc_id"])
                .rows(1000000);
            wn.search(query,
                      function(err, doc)
                      {
                          if (!err)
                          {
                              res.json(doc.response.docs);
                          } else 
                          {
                              res.json(err);
                          }
                      });

        });

app.get('/list-suggestions/:type',
        function(req,res)
        {
            var query = wnchanges.createQuery()
                .q('*:*')
                .matchFilter("type", "suggestion")
                .matchFilter("doc_type", "synset")
                .matchFilter("action", req.params.type)
                .fl(["doc_id","params"])
                .rows(1000000);
            wnchanges.search(query,
                      function(err, doc)
                      {
                          if (!err)
                          {
                              res.json(doc.response.docs);
                          } else 
                          {
                              res.json(err);
                          }
                      });

        });

var host = (process.env.VCAP_APP_HOST || 'localhost');
// The port on the DEA for communication with the application:
var port = (process.env.VCAP_APP_PORT || 3000);
// Start server
app.listen(port, host);
// app.listen(3000);

