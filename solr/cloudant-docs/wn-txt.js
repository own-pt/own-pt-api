function(d){
    
    var text = '';

    if (d.word_pt) { text += d.word_pt.join(' ') + ' '; }
    if (d.word_en) { text += d.word_en.join(' ') + ' '; }
    if (d.id) { text += d.id + ' '; }
    if (d.gloss_en) { text += d.gloss_en + ' '; }
    if (d.gloss_pt) 
    { 
        if (Array.isArray(d.gloss_pt))
        {
            d.gloss_pt.forEach(function(elt,idx){text += elt + ' ';});
        }
        else
        {
        text += d.gloss_pt + ' '; 
        }
        
    }

    if (d.example_pt) 
    { 
        if (Array.isArray(d.example_pt))
        {
            d.example_pt.forEach(function(elt,idx){text += elt + ' ';});
        }
        else
        {
        text += d.example_pt + ' '; 
        }
        
    }

    if (d.nomlex_noun) { text += d.nomlex_noun + ' '; }
    if (d.nomlex_verb) { text += d.nomlex_verb + ' '; }
    if (d.nomlex_plural) { text += d.nomlex_plural; }

    index("default", text);
    index("id", d.id);
    
    if (d.nomlex_plural) {index("nomlex_plural", d.nomlex_plural);}
    if (d.nomlex_verb) { index("nomlex_verb", d.nomlex_verb); }
    if (d.nomlex_noun) { index("nomlex_noun", d.nomlex_noun); }

    if (d.word_pt)
    {
        if (Array.isArray(d.word_pt))
        {
            d.word_pt.forEach(function(elt, idx) 
            {
                index("word_pt", elt);
            });
        }
        else
        {
            index("word_pt", d.word_pt);
        }
    }
    
    if (d.word_en)
    {
        if (Array.isArray(d.word_en))
        {
            d.word_en.forEach(function(elt, idx) 
            {
                index("word_en", elt);
            });
        }
        else
        {
            index("word_en", d.word_pt);
        }
    }

    if (d.hasOwnProperty('word_count_en'))
    {
        index("word_count_en", d.word_count_en.toString(), { "facet": true });
    }

    if (d.hasOwnProperty('word_count_pt'))
    {
        index("word_count_pt", d.word_count_pt.toString(), { "facet": true });
    }
    
    if (d.rdf_type)
    {
        if (Array.isArray(d.rdf_type))
        {
            d.rdf_type.forEach(function(elt, idx) 
            {
                index("rdf_type", elt, {"facet": true});
                index("rdf_type1", elt, {"facet": true});
                index("rdf_type2", elt, {"facet": true});
                index("rdf_type3", elt, {"facet": true});
                index("rdf_type4", elt, {"facet": true});
                index("rdf_type5", elt, {"facet": true});
            });
        }
        else
        {
            index("rdf_type", d.rdf_type, { "facet": true });
            index("rdf_type1", d.rdf_type, { "facet": true });
            index("rdf_type2", d.rdf_type, { "facet": true });
            index("rdf_type3", d.rdf_type, { "facet": true });
            index("rdf_type4", d.rdf_type, { "facet": true });
            index("rdf_type5", d.rdf_type, { "facet": true });
        }
    }
    
    if (d.wn30_lexicographerFile) 
    {
        if (Array.isArray(d.wn30_lexicographerFile))
        {
            d.wn30_lexicographerFile.forEach(function(elt, idx)
            {
                index("wn30_lexicographerFile", elt, { "facet": true });
            });
        }
        else
        {
            index("wn30_lexicographerFile", d.wn30_lexicographerFile, { "facet": true });
        }
    }
}

