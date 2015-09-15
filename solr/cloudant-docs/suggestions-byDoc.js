function(doc){
    index("doc_id", doc.doc_id);
    index("doc_type", doc.doc_type,{facet:true});
    index("user", doc.user,{facet:true});
    index("type", doc.type, {facet:true});
    index("date", doc.date);
    index("params", doc.params);
    if (doc.action) index("action", doc.action,{facet:true});
    if (doc.status) index("status", doc.status,{facet:true});
    if (doc.provenance) index("provenance", doc.provenance,{facet:true});
}

