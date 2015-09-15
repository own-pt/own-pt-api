var credentials = require('../credentials.js');
var dbCredentials = credentials.getDatabase();
var cloudant = require('cloudant')(dbCredentials.url);
var st = cloudant.use(dbCredentials.stDbName);


/*
  "_id": "bosque.json-0-12-fcbr",
  "_rev": "1-7c784b60dface2da1fddb08859be5c85",
  "userid": "fcbr",
  "file": "bosque.json",
  "text": "0",
  "word": "12",
  "selection": "00044150-n",
  "comment": ""
*/

console.log(['doc_id','userid','file','text','word','selection','comment'].join(','));

st.list({ include_docs: true },
        function (err, body)
        {
          body.rows.forEach(
            function(elt)
            {

              console.log([elt.doc._id,elt.doc.userid,elt.doc.file,elt.doc.text,elt.doc.word,elt.doc.selection,elt.doc.comment].join(','));
            });
        });
