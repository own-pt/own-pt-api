var credentials = require('./credentials.js');
var dbCredentials = credentials.getDatabase();
var wnaudit= dbCredentials.auditDbName;
var uuid = require('node-uuid');

exports.registerAudit = function(db, action, id, field, value, user, provenance)
{
  var audit = {}
  audit.id = uuid.v4();
  audit.date = Date.now();
  audit.db = db;
  audit.action = action;
  audit.doc_id = id;
  audit.field = field;
  audit.value = value;
  audit.user = user;
  audit.provenance = provenance;
  wnaudit.add(audit,
              function(err, body)
              {
                wnaudit.softCommit();
              });
}
