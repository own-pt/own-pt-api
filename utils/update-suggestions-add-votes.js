var credentials = require('../credentials.js');

var dbCredentials = credentials.getDatabase();

var dbchanges = dbCredentials.changesDbName;
var dbvotes = dbCredentials.votesDbName;

var updates = [];

function tally(suggestion_id, votes)
{
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
}

function compute_votes(wnvotes)
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
                       vote_score: 0 };
      }
      
      if (val > 0)
      {
        votes[sid].all_voters.push(user);
        votes[sid].positive_votes.push(val);
        votes[sid].positive_voters.push(user);
        votes[sid].vote_score += Math.abs(val);
        votes[sid].sum_votes += val;
      }
      
      if (val < 0)
      {
        votes[sid].all_voters.push(user);
        votes[sid].negative_votes.push(val);
        votes[sid].negative_voters.push(user);
        votes[sid].vote_score += Math.abs(val);
        votes[sid].sum_votes += val;
      }
    });
  
  return votes;
}

function read_dbs()
{
  
  dbchanges.search(
    dbchanges.createQuery().q('*').rows('2000000'),
    function(err,body)
    {
      wnsuggestions = body.response.docs;
      console.log(wnsuggestions.length, 'suggestions loaded.');
      dbvotes.search(
        dbvotes.createQuery().q('*').rows('2000000'),
        function(err,body)
        {
          wnvotes = body.response.docs;
          console.log(wnvotes.length, 'votes loaded.');
          var tallied_votes = compute_votes(wnvotes);
          wnsuggestions.forEach(
            function(s)
            {
              tally(s.id, tallied_votes);
            });
          console.log('applying', updates.length,'updates');
          dbchanges.update(
            updates,
            function(err, body)
            {
              dbchanges.commit();
              console.log('done.', body, err);
            });
        });
    });
}

read_dbs();
