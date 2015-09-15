var async = require('async');
var credentials = require('../credentials.js');
var workflow = require('../workflow.js');

var dbCredentials = credentials.getDatabase();

var wnchanges = dbCredentials.changesDbName;
var wnvotes = dbCredentials.votesDbName;

function isAccepted(vote)
{
  return vote.sum_votes >= 3;
}

function isRejected(vote)
{
  return false;
  //return vote.sum_votes <= -1;
}

function accept(suggestion_id)
{
  workflow.acceptSuggestion(suggestion_id,
                            function(err,body)
                            {
                              console.log(err);
                            });
}

function reject(suggestion_id)
{
  workflow.rejectSuggestion(suggestion_id,
                            function(err,body)
                            {
                              console.log(err);
                            });
}

wnvotes.search(
  wnvotes.createQuery().q('*:*').rows('2000000'),
  function(err, body)
  {
    var votes = body.response.docs;
    var tallied_votes = workflow.tabulateVotes(votes);
    for(v in tallied_votes)
    {
      if (isAccepted(tallied_votes[v]))
      {
        accept(v);
      } else
      if (isRejected(tallied_votes[v]))
      {
        reject(v);
      }
    }
  });


 
