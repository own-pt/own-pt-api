var async = require('async');
var credentials = require('../credentials.js');
var workflow = require('../workflow.js');

var dbCredentials = credentials.getDatabase();

var wn = dbCredentials.dbName;

var types = ['NounSynset','AdjectiveSynset','VerbSynset','AdverbSynset'];

types.forEach(
  function(type)
  {
    var words = {};
    wn.search(
      wn.createQuery().q('*:*').matchFilter('rdf_type',type).rows(2000000),
      function(err, body)
      {
        var words = {};
        body.response.docs.forEach(
          function(d)
          {
            if (d.word_en)
              d.word_en.forEach(
                function(word)
                {
                  var w = word.toLowerCase();
                  if (words[w]) { words[w]++; } else { words[w] = 1; }
                });
          });

        var mono = 0;
        var poly = 0;
        for(w in words)
        {
          if (words[w] == 1) mono++; else poly++;
        }
        console.log(type, 'mono: ' + mono, 'poly: '+ poly);
      });
  });
