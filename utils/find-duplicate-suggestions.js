var async = require('async');
var credentials = require('../credentials.js');
var workflow = require('../workflow.js');

var dbCredentials = credentials.getDatabase();

var wnchanges = dbCredentials.changesDbName;

var _ = require('underscore');

wnchanges.search(
  wnchanges.createQuery().q('*').rows('2000000'),
  function(err,body)
  {
    var bysynset = _.groupBy(body.response.docs,
                          function(elt) { return elt.doc_id });

    for (synsetid in bysynset)
    {
      var suggestions = bysynset[synsetid];
      var words = [];
      suggestions.forEach(
        function(suggestion)
        {
          if (suggestion.type === 'suggestion')
          {
            var word = suggestion.params.trim();
            if (_.contains(words, word))
            {
              console.log(synsetid);
            } else {
              words.push(word);
            }
          }
        });
    }
  });

