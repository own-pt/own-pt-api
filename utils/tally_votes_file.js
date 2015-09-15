// synset, action, positive_votes, negative_votes

// alternative to tally_votes that fetches all the votes into memory first
// this works while the voting database is small

var fs = require('fs');

function tally(suggestion_id, synset, action, user, provenance, status)
{
  var pos = 0;
  var neg = 0;

  if (votes[suggestion_id])
  {
    pos = votes[suggestion_id].pos;
    neg = votes[suggestion_id].neg;
  }
  
  var result = [synset, action, user, provenance, status, pos, neg];
  console.log(result.join(','));
}

function search()
{
  var wnchanges = JSON.parse(fs.readFileSync('/tmp/wnproposedchanges.json', 'utf8'));

  wnchanges.rows.forEach(
    function(elt)
    {
      if (elt.doc.type === 'suggestion')
        tally(elt.doc._id, elt.doc.doc_id, elt.doc.action, elt.doc.user, elt.doc.provenance, elt.doc.status);
    });
}

var votes = {};

var wnvotes = JSON.parse(fs.readFileSync('/tmp/wnvotes.json', 'utf8'));

wnvotes.rows.forEach(
      function(elt)
      {
        var sid = elt.doc.suggestion_id;
        var val = elt.doc.value;

        if (!votes[sid])
        {
          votes[sid] = { pos:0, neg: 0};
        }
        
        if (val > 0)
        {
          votes[sid].pos += val;
        }

        if (val < 0)
        {
          votes[sid].neg += val;
        }
      });

search();
