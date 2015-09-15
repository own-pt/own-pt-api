var async = require('async');
var credentials = require('../credentials.js');
var workflow = require('../workflow.js');

var dbCredentials = credentials.getDatabase();

var wnchanges = dbCredentials.changesDbName;

function processField(doc, method)
{
  method(
    doc.doc_id, doc.params.trim(),
    function(err, body)
    {
      if (!err)
      {
        workflow.commitSuggestion(
          doc.id,
          function(err,body)
          {
            console.log('committed', err);
          });
      }
      else
      {
        console.log('error', err);
      }
    });
}

function processAddWordPt(doc)
{
  console.log('add-word-pt', doc.params);
  processField(doc, workflow.addWordToSynset);
}

function processRemoveWordPt(doc)
{
  console.log('remove-word-pt', doc.params);
  processField(doc, workflow.removeWordFromSynset);
}

function processAddExamplePt(doc)
{
  console.log('add-example-pt', doc.params);
  processField(doc, workflow.addExampleToSynset);
}

function processRemoveExamplePt(doc)
{
  console.log('remove-example-pt', doc.params);
  processField(doc, workflow.removeExampleFromSynset);
}

function processAddGlossPt(doc)
{
  console.log('add-gloss-pt', doc.params);
  processField(doc, workflow.addGlossToSynset);
}

function processRemoveGlossPt(doc)
{
  console.log('remove-gloss-pt', doc.params);
  processField(doc, workflow.removeGlossFromSynset);
}

var processor = { 'add-word-pt': processAddWordPt,
                  'remove-word-pt': processRemoveWordPt,
                  'add-gloss-pt': processAddGlossPt,
                  'remove-gloss-pt': processRemoveGlossPt,
                  'add-example-pt': processAddExamplePt,
                  'remove-example-pt': processRemoveExamplePt };

function search()
{
  var query = wnchanges.createQuery().q('*:*').rows(2000000).matchFilter('status','accepted');
  
  wnchanges.search(
    query,
    function(err, body)
    {
      if (!err)
      {
        body.response.docs.forEach(
          function(elt)
          {
            var processorFunction = processor[elt.action];
            if (processorFunction)
            {
              processorFunction(elt);
            } else {
              console.log('unknown action', elt.action);
            }
          });
      }
      else
      {
        console.log(err);
      }
    });
}             

search();
