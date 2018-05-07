"use strict";

var r = require("rethinkdb");

exports._connect = function(config) {
  return function(error, success) {
    return r.connect(config, function(err, connection) {
      if (err) {
        error(err);
      } else {
        success(connection);
      }
    });
    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
  };
};

exports._close = function(connection) {
  return function(error, success) {
    connection.close(function(err) {
      if (err) {
        error(err);
      } else {
        success();
      }
    });
    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
  };
};

exports._get = function(key) {
  return function(table) {
    return function(connection) {
      return function(error, success) {
        r
          .table(table)
          .get(key)
          .run(connection, function(err, value) {
            if (err) {
              error(err);
            } else {
              success(value);
            }
          });
      };
    };
  };
};

exports._getAll = function(keys) {
  return function(table) {
    return function(connection) {
      return function(error, success) {
        r
          .table(table)
          .getAll(keys)
          .run(connection, function(err, value) {
            if (err) {
              error(err);
            } else {
              success(value);
            }
          });
      };
    };
  };
};

exports._getAllFromIndex = function(keys) {
  return function(table) {
    return function(index) {
      return function(connection) {
        return function(error, success) {
          r
            .table(table)
            .getAll(keys, { index: index })
            .run(connection, function(err, value) {
              if (err) {
                error(err);
              } else {
                success(value);
              }
            });
        };
      };
    };
  };
};
