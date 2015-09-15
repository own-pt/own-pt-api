exports.GetFQArray = function(drilldown)
{
  if (drilldown)
  {
    if (drilldown.constructor !== Array)
    {
      drilldown = [drilldown];
    }
    
    var qfs = [];
    
    drilldown.forEach(
      function(elt)
      {
        var drilldown_array = JSON.parse(elt);
        qfs.push({field:drilldown_array[0],value:drilldown_array[1]});
      });
    
    return qfs;
  }

  return null;
}
exports.convertSearchResults = function(cloudant)
{
  solr = cloudant;
  solr.total_rows = solr.response.numFound;
  var rows = [];
  for (d in solr.response.docs)
  {
    rows.push({doc: solr.response.docs[d]});
  }
  solr.rows = rows;

  if (solr.facet_counts)
  {
    solr.counts = solr.facet_counts.facet_fields;
    // delete solr.facet_counts;
  }
  
  delete solr.response;

  
  return solr;
}
