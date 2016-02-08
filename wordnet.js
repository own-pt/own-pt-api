var url = require('url');
var fs = require('fs');

function processPredicate(elt, predicates)
{
  var prefix = 'wn30_';
  var u = url.parse(elt.pred.value);
  var wn30 = '/wn30/schema/';

  if (u.pathname.lastIndexOf(wn30, 0) === 0)
  {
    predicates.push(prefix +
      u.pathname.substring(wn30.length, u.pathname.length));
  }
}

function getNormalizedField(fieldName, synset)
{
  if (synset && synset[fieldName] && synset[fieldName].constructor !== Array)
  {
    return [synset[fieldName]];
  }

  return synset[fieldName];
}

function normalizeField(fieldName, synset)
{
  if (synset && synset[fieldName] && synset[fieldName].constructor !== Array)
  {
    synset[fieldName] = [synset[fieldName]];
  }
}

function getGlossesPt(s)
{
  return getNormalizedField('gloss_pt', s);
}

function getGlossesEn(s)
{
  return getNormalizedField('gloss_en', s);
}

exports.normalizeFields = function(s)
{
  var fields = ['gloss_pt', 'example_pt'];

  fields.forEach(
    function(elt)
    {
      normalizeField(elt, s);
    });
}

exports.getWord = function(s)
{
  return s.word_en[0];
}

exports.getWordsPt = function(s)
{
  if (s.word_pt)
    return '(' + s.word_pt.join(', ') + ')';

  return '(empty)';
}

exports.getGlosses = function(s)
{
  var glosses_pt = getGlossesPt(s);
  var glosses_en = getGlossesEn(s);

  if (glosses_pt && glosses_pt.length > 0)
  {
    return glosses_pt.join('; ');
  }

  return glosses_en.join('; ');
}

exports.getWords = function(s, fmt)
{
  if (s && s.word_pt && s.word_pt.length > 0)
  {
    if (fmt === 'array')
    {
      return s.word_pt;
    }
    else
      return '(' + s.word_pt.join(', ') + ')';
  }

  if (s && s.word_en && s.word_en.length > 0)
  {
    if (fmt === 'array')
    {
      return s.word_en;
    }
    else
      return '(' + s.word_en.join(', ') + ')';
  }

  return '(unknown)';
}

exports.getPredicates = function()
{
  var obj = JSON.parse(fs.readFileSync('predicates.json', 'utf8'));

  pred = obj.results.bindings;

  var predicates = [];

  pred.forEach(function(elt)
  {
    processPredicate(elt, predicates);
  });

  return predicates;
}