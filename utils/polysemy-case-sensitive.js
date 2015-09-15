var fs = require('fs');
var async = require('async');
var credentials = require('../credentials.js');
var workflow = require('../workflow.js');

var dbCredentials = credentials.getDatabase();

var wn = dbCredentials.dbName;

var types = ['NounSynset','AdjectiveSynset','VerbSynset','AdverbSynset'];

// note: english-centric and ignores symbols!
function beginsWithLowerCase(w)
{
  return w.match(/^[a-z]/);
}

function beginsWithUpperCase(w)
{
  return w.match(/^[A-Z]/);
}

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
                  var w = word;
                  if (words[w]) { words[w]++; } else { words[w] = 1; }
                });
          });

        var mono_lower = 0;
        var mono_upper = 0;
        var poly_lower = 0;
        var poly_upper = 0;
        var mono = [];
        var poly = [];
        for(w in words)
        {
          if (words[w] == 1) mono.push(w); else poly.push(w);
          
          if (beginsWithLowerCase(w))
          {
            if (words[w] == 1) mono_lower++; else poly_lower++;
          }

          if (beginsWithUpperCase(w))
          {
            if (words[w] == 1) mono_upper++; else poly_upper++;
          }
        }
        
        fs.writeFile(type+'.mono.txt', mono.join('\n'));
        fs.writeFile(type+'.poly.txt', poly.join('\n'));
        
        console.log('##### ', type);
        console.log('mono lowercase: ' + mono_lower,
                    'mono uppercase: ' + mono_upper);
        console.log('poly lowercase: ' + poly_lower,
                    'poly uppercase: ' + poly_upper);
      });
  });
