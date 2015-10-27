var credentials = require('./credentials.js');
var dbCredentials = credentials.getDatabase();
var wn = dbCredentials.dbName;
var pointers = dbCredentials.pointersName;
var wnchanges = dbCredentials.changesDbName;
var wnvotes = dbCredentials.votesDbName;
var uuid = require('node-uuid');
var audit = require('./audit.js');

var DEBUG = false;

function escapeSpecialChars(s){
  return s.replace(/([\+\-!\(\)\{\}\[\]\^"~\*\?:\\])/g, function(match) {
    return '\\' + match;
  })
  .replace(/&&/g, '\\&\\&')
  .replace(/\|\|/g, '\\|\\|');
}

exports.getPointers = function(synset, word, callback)
{
    localGetPointers (synset, word, callback);
}

exports.getDocument = function(id, callback)
{
  localGetDocument(id, callback);
}

function localGetPointers (synset, word, callback)
{
    var params = {};

    if (synset) 
    {
        params.source_synset = escapeSpecialChars("https://w3id.org/own-pt/wn30-en/instances/synset-" + synset);
    }

    if (word)
    {
        params.source_word = word;
    }

    var query = pointers.createQuery().q(params);

    pointers.search(query,
                    function (err, result)
                    {
                        callback(null, result.response.docs);
                    });
}

function localGetDocument(id, callback)
{
  var query = wn.createQuery().q({doc_id:escapeSpecialChars(id)});
  
  wn.search(query,
            function(err, doc)
            {
              if (!err)
              {
                if (DEBUG && doc.respose.numFound > 1)
                {
                  console.log('WARNING: more than one document found!', id);
                }

                if (DEBUG && doc.response.numFound == 0)
                {
                  console.log('WARNING: no document found!', id);
                }

                if (doc.response.numFound >= 1)
                {
                  callback(null, doc.response.docs[0]);
                }
                else
                {
                  callback({'error' : 'document-not-found'}, null);
                }
              }
              else
              {
                callback({'error': err}, null);
              }
            });
}

exports.acceptSuggestion = function(id, callback)
{
  wnchanges.realTimeGet(
    id,
    function(err, body)
    {
      var doc = body.response.docs[0];

      if (!err)
      {
        if (doc.status === 'new')
        {
          var update = {id: doc.id,
                        status: {set: 'accepted'}};
          
          wnchanges.update(
            [update],
            function(err, body) {
              wnchanges.softCommit(
                function(err,body)
                {
                  if (callback) callback(err, body);
                });
            });
          
          audit.registerAudit('wnproposedchanges', 'accept-suggestion',
                              doc.doc_id,
                              doc.type,
                              doc.action
                              +'('+doc.params+')',
                              doc.user, 'web');
        } else
        {
          if (callback) callback({'error' : 'suggestion is not new'});
        }
      }
      else
      {
        if (callback) callback(err);
      }
    });
}

exports.rejectSuggestion = function(id, callback)
{
  wnchanges.realTimeGet(
    id,
    function(err, body)
    {
      var doc = body.response.docs[0];
      if (!err)
      {
        if (doc.status === 'new')
        {
          var update = [{id: doc.id,
                         status: {set: 'not-accepted'}}];
          
          wnchanges.update(
            update,
            function(err, body) {
              wnchanges.softCommit(
                function(err,body)
                {
                  if (callback) callback(err, body);
                });
            });

          audit.registerAudit('wnproposedchanges', 'reject-suggestion',
                              doc.doc_id,
                              doc.type,
                              doc.action
                              +'('+doc.params+')',
                              doc.user, 'web');
        }
        else
        {
          if (callback) callback({'error' : 'suggestion is not new'});
        }
      }
      else {
        if (callback) callback(err, body);
      }
    });
}

exports.commitSuggestion = function(id, callback)
{
  wnchanges.realTimeGet(
    id,
    function(err, body)
    {
      var doc = body.response.docs[0];
      if (!err)
      {
        var update = [{id: doc.id,
                       status: {set: 'committed'}}];

        wnchanges.update(
          update,
          function(err, body) {
            wnchanges.softCommit();
          });
        
        audit.registerAudit('wnproposedchanges', 'commit-suggestion',
                            doc.doc_id,
                            doc.type,
                            doc.action
                            +'('+doc.params+')',
                            doc.user, 'web');
      }
      
      if (callback)
        callback(err);
    });
}

function deleteVotesForSuggestion(suggestion_id, callback)
{
  var docs = [];
  var query = 'suggestion_id:'+escapeSpecialChars(suggestion_id);

  wnvotes.deleteByQuery(
    query,
    function(err, body)
    {
      wnvotes.softCommit();
      if (callback) callback(err, body);
    });
}

exports.deleteSuggestion = function(id, callback)
{
  wnchanges.realTimeGet(
    id,
    function(err, body)
    {
      if (!err)
      {
        var doc = body.response.docs[0];
        
        wnchanges.deleteByID(doc.id,
                             function(err,body)
                             { wnchanges.softCommit(); });
        
        audit.registerAudit('wnproposedchanges', 'delete-suggestion',
                            doc.doc_id,
                            doc.type,
                            doc.action
                            +'('+doc.params+')',
                            doc.user, 'web');
        
        deleteVotesForSuggestion(
          id,
          function(e, b)
          {
            callback(e);
          });
      }
      else
      {
        callback(err);
      }
    });
}

exports.deleteComment = function(id, callback)
{
  wnchanges.realTimeGet(
    id,
    function(err, body)
    {
      var doc = body.response.docs[0];
      
      if (!err)
      {
        wnchanges.deleteByID(doc.id,
                             function(err,body)
                             {
                               wnchanges.softCommit();
                             });
        audit.registerAudit('wnproposedchanges', 'delete-comment',
                            doc.doc_id,
                            doc.type,
                            doc.action
                            +'('+doc.params+')',
                            doc.user, 'web');
      }
      
      callback(err);
    });
}

exports.addSuggestion = function(date, doc_id, doc_type, suggestion_type, params, user, provenance, vote, callback)
{
  var action = {};
  action.id = uuid.v4();
  action.date = date;
  action.doc_id = doc_id;
  action.doc_type = doc_type;
  action.type = 'suggestion';
  action.action = suggestion_type;
  action.params = params;
  action.user = user;
  action.status = 'new'; // new accepted not-accepted committed
  action.provenance = provenance;

  audit.registerAudit('wnproposedchanges', 'add-suggestion', action.doc_id,
                      action.type, action.action+'('+action.params+')',
                      action.user, action.provenance);

  wnchanges.add(
    action,
    function(err,body)
    {
      wnchanges.softCommit(
        function(err, body)
        {
          if (vote)
          {
            localAddVote(
              action.id, action.user, 1,
              function(e,b)
              {
                callback(e,b);
              });
          }
          else
          {
            callback(err,body);
          }
        });
    });
}

exports.addComment = function(date, doc_id, doc_type, user, params, provenance)
{
  var comment = {};
  comment.date = date;
  comment.doc_id = doc_id;
  comment.doc_type = doc_type;
  comment.type = 'comment';
  comment.action = 'comment';
  comment.user = user;
  comment.params = params;
  comment.status = 'new';
  comment.provenance = provenance;
  comment.tags = localGetTags(params);
  comment.id = uuid.v4();
  
  wnchanges.add(
    comment,
    function(err, body)
    {
      wnchanges.softCommit();
    });
  
  audit.registerAudit('wnproposedchanges', 'add-suggestion', comment.doc_id,
                      comment.type, comment.action+'('+comment.params+')',
                      comment.user, comment.provenance);
}

function addEntryToField(synsetid, id, field, countField, entry, auditAction, callback)
{
  var update = { id: id };
  update[field] = { add: entry };
  if (countField)
  {
    update[countField] = { inc: 1 };
  }
  
  wn.update([update],
            function(err, body)
            {
              if (!err)
              {
                wn.commit(
                  function(err, body)
                  {
                    audit.registerAudit('wn', auditAction, synsetid,
                                    field, entry, '<system>', 'web');
                    callback(null);
                  });
              }
              else
              {
                callback({'error': 'update-error', 'err': err});
              }
            });
}

function removeEntryFromField(synsetid, id, field, countField, entry, auditAction, callback)
{
  var update = { id: id };
  update[field] = { remove: entry };
  if (countField)
  {
    update[countField] = { inc: -1 };
  }
      
  wn.update([update], 
            function(err, body)
            {
              if (!err)
              {
                wn.commit(
                  function(err, body)
                  {
                    audit.registerAudit(
                      'wn', auditAction,
                      synsetid, field, entry, '<system>', 'web');
                    callback(null);
                  });
              }
              else
              {
                callback({'error': 'update-error', 'err': err});
              }
              
            });
}
  
exports.addGlossToSynset = function(synsetid, gloss, callback)
{
  localGetDocument(
    synsetid,
    function(err, doc)
    {
      if (!err)
        addEntryToField(synsetid, doc.id, 'gloss_pt',
                        null, gloss, 'add-gloss', callback);
    });
}

exports.removeGlossFromSynset = function(synsetid, gloss, callback)
{
  localGetDocument(
    synsetid,
    function(err, doc)
    {
      if (!err)
        removeEntryFromField(synsetid, doc.id, 'gloss_pt',
                             null, gloss, 'remove-gloss', callback);
    }); 
}

exports.addExampleToSynset = function(synsetid, example, callback)
{
  localGetDocument(
    synsetid,
    function(err, doc)
    {
      if (!err)
        addEntryToField(synsetid, doc.id, 'example_pt',
                        null, example, 'add-example', callback);
    });
}

exports.removeExampleFromSynset = function(synsetid, example, callback)
{
  localGetDocument(
    synsetid,
    function(err, doc)
    {
      if (!err)
        removeEntryFromField(synsetid, doc.id, 'example_pt',
                             null, example, 'remove-example', callback);
    });
}

exports.addWordToSynset = function(synsetid, word, callback)
{
  localGetDocument(
    synsetid,
    function(err, doc)
    {
      if (!err)
        addEntryToField(synsetid, doc.id, 'word_pt',
                        'word_count_pt', word, 'add-word', callback);
    });
}

exports.removeWordFromSynset = function(synsetid, word, callback)
{
  localGetDocument(
    synsetid,
    function(err, doc)
    {
      if (!err)
        removeEntryFromField(synsetid, doc.id, 'word_pt',
                             'word_count_pt', word, 'remove-word', callback);
    });
}

exports.getVotes = function(suggestion_id, callback)
{
  var query = wnvotes.createQuery().q({suggestion_id: escapeSpecialChars(suggestion_id)});
  wnvotes.search(
    query,
    function(err, body)
    {
      if (err)
      {
        if (callback) callback({'error':err}, null);
      }
      else
      {
        var positive = 0;
        var negative = 0;
        var total = 0;
        var positive_votes = [];
        var negative_votes = [];

        body.response.docs.forEach(
          function(r)
          {
            var value = r.value;
            var user = r.user;
            var vid = r.id;
            if (value > 0)
            {
              positive += value;
              positive_votes.push({user:user, value:value, id:vid});
            }

            if (value < 0)
            {
              negative += value;
              negative_votes.push({user:user, value:value, id:vid});
            }

            total = positive+negative;
          });

        if (callback)
        {
          callback(
            null,
            { positive: positive,
              negative: negative,
              total: total,
              positive_votes: positive_votes,
              negative_votes: negative_votes});
        }
      }
    });
}

exports.addVote = function(suggestion_id, user, value, callback)
{
  localAddVote(suggestion_id, user, value, callback);
}

function localAddVote(suggestion_id, user, value, callback)
{
  var query = wnvotes.createQuery()
      .q("user:"+user+" AND suggestion_id:"+escapeSpecialChars(suggestion_id));
  
  wnvotes.search(
    query,
    function(err, body)
    {
      if (err || body.response.numFound > 0)
      {
        if (err)
        {
          if (callback) callback({'error': err});
        }
        else {
          if (body.response.numFound > 0)
          {
            if (callback) callback({'error': 'user-already-voted'});
          }
        }
      }
      else {
        wnchanges.realTimeGet(
          suggestion_id,
          function(err, body)
          {
            if (err)
            {
              if (callback) callback({'error': err});
            }
            else
            {
              var suggestion = body.response.docs[0];
              
              var vote = {};

              vote.id = uuid.v4();
              vote.date = Date.now();
              vote.suggestion_id = suggestion.id;
              vote.user = user;
              vote.value = parseInt(value);
              
              audit.registerAudit('wnvotes', 'add-vote',
                                  vote.suggestion_id, 'vote',
                                  vote.value,
                                  vote.user, 'web');
              
              wnvotes.add(
                vote,
                function (err, body)
                {
                  wnvotes.softCommit(
                    function(err, body)
                    {
                      processVotes(
                        suggestion.id,
                        function(err, body)
                        {
                          if (callback) callback(err, body);
                        });
                    });
                });
            }
          });
      }
    });
}

exports.deleteVote = function(vote_id, callback)
{
  wnvotes.realTimeGet(
    vote_id,
    function(err, body)
    {
      if (err)
      {
          if (callback) callback({'error': err});
      }
      else
      {
        var doc = body.response.docs[0];
        
        var suggestion_id = doc.suggestion_id;
        var value = doc.value;
        var user = doc.user;

        audit.registerAudit('wnvotes', 'remove-vote',
                            suggestion_id, 'vote', value, user, 'web');

        wnvotes.deleteByID(
          doc.id,
          function(e, b)
          {
            wnvotes.softCommit(
              function(e,b)
              {
                processVotes(suggestion_id,
                             function(e,b)
                             {
                               if (callback) callback(e, b);
                             });
              });
          });
      }
    });
}

function tally(suggestion_id, votes)
{
  var updates = [];
  var update = {};
  if (votes[suggestion_id])
  {
    update = {id : suggestion_id,
              negative_votes: {set: votes[suggestion_id].negative_votes},
              positive_votes: {set: votes[suggestion_id].positive_votes},
              negative_voters: {set: votes[suggestion_id].negative_voters},
              positive_voters: {set: votes[suggestion_id].positive_voters},
              all_voters: {set: votes[suggestion_id].all_voters},
              sum_votes: {set: votes[suggestion_id].sum_votes},
              vote_score: {set: votes[suggestion_id].vote_score}};
    updates.push(update);
  }

  return updates;
}

exports.tabulateVotes = function(votes)
{
  return localTabulateVotes(votes);
}

function localTabulateVotes(wnvotes)
{
  var votes={};
  wnvotes.forEach(
    function(elt)
    {
      var sid = elt.suggestion_id;
      var val = elt.value;
      var user = elt.user;
      
      if (!votes[sid])
      {
        votes[sid] = { positive_votes: [],
                       negative_votes: [],
                       positive_voters: [],
                       negative_voters: [],
                       all_voters: [],
                       sum_votes: 0,
                       sum_positive_votes: 0,
                       sum_negative_votes: 0,
                       vote_score: 0 };
      }
      
      if (val > 0)
      {
        votes[sid].all_voters.push(user);
        votes[sid].positive_votes.push(val);
        votes[sid].positive_voters.push(user);
        votes[sid].vote_score += Math.abs(val);
        votes[sid].sum_votes += val;
        votes[sid].sum_positive_votes += val;
      }
      
      if (val < 0)
      {
        votes[sid].all_voters.push(user);
        votes[sid].negative_votes.push(val);
        votes[sid].negative_voters.push(user);
        votes[sid].vote_score += Math.abs(val);
        votes[sid].sum_votes += val;
        votes[sid].sum_negative_votes += val;
      }
    });
  
  return votes;
}

function processVotes(id, callback)
{
  wnvotes.search(
    wnvotes.createQuery().q({suggestion_id:escapeSpecialChars(id)}).rows('2000000'),
    function(err, body)
    {
      var tallied_votes = localTabulateVotes(body.response.docs);
      var updates = tally(id, tallied_votes);

      wnchanges.update(
        updates,
        function(err, body)
        {
          if (!err)
            wnchanges.softCommit(
              function(err,body)
              {
                if (callback) callback(err, body);
              });
        });
    });
}

function localGetTags(str)
{
  var tags = [];
  str.split(/[\s\.\,\"\'\:\;]+/).forEach(
    function(elt)
    {
      if (elt.length > 1)
      {
        if (elt[0] == '@')
          tags.push('AT' + elt.substring(1));
        if (elt[0] == '#')
          tags.push('HASH' + elt.substring(1));
      }
    });
  return tags;
}

exports.getTags = function(str)
{
  return localGetTags(str);
}
