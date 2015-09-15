var credentials = require('./credentials.js');
var dbCredentials = credentials.getDatabase();
var st = dbCredentials.stDbName;

function getid(file,text,word,userid)
{
  return [file,text,word,userid].join('-');
}

exports.getSuggestion = function(file,text,word,userid,callback)
{
  st.realTimeGet(
    getid(file,text,word,userid),
    function(err, body)
    {
      callback(err, body);
    });
}

function localAddSuggestion(suggestion, callback)
{
  st.add(suggestion,
         function(err, body)
         {
           if (err) { 
             if (callback) callback(err, body);
           } else {
             st.softCommit(
               function(err, body)
               {
                 if (callback) callback(err, body);
               });
           }
         });
}

function localUpdateSuggestion(suggestion, callback)
{
  var update = {id  : suggestion.id,
                selection : {set: suggestion.selection },
                comment: {set: suggestion.comment} };
  st.update([update],
            function(err, body)
            {
              if (err)
              {
                if (callback) callback(err, body);
              }
              else
              {
                st.softCommit(
                  function(err, body)
                  {
                    if (callback) callback(err, body);
                  });
              }
            });
}

exports.addSuggestion = function(suggestion, callback)
{
  var id = getid(suggestion.file,suggestion.text,suggestion.word,suggestion.userid);

  suggestion['id'] = id;
  st.realTimeGet(
    id,
    function(err, body)
    {
      if (body.response.numFound == 0)
      {
        localAddSuggestion(suggestion, callback);        
      }
      else
      {
        localUpdateSuggestion(suggestion, callback);
      }
    });
}
     
     
     
