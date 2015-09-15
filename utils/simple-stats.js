var async = require('async');
var credentials = require('../credentials.js');
var workflow = require('../workflow.js');

var dbCredentials = credentials.getDatabase();

var wn = dbCredentials.dbName;

function stats(ignoreCase)
{
  var types = ['NounSynset','AdjectiveSynset','VerbSynset','AdverbSynset'];
  var total = 0;
  async.each(
    types,
    function(type, callback)
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
                  function(w)
                  {
                    if (ignoreCase) w = w.toLowerCase();
                    words[w]=true;
                  });
            });
          var num = Object.keys(words).length;
          total += num;
          console.log(type, num);
          callback()
        });
    },
    function(err)
    {
      console.log('total',total);
    });
}

stats(true);
