// module UrlShortener.Redis

var redis = require('redis');

function debugPrint(s) {
  console.log('(REDIS DEBUG): ' + s);
}

exports.selectDb =
  function(n) {
    var db = redis.createClient();
    db.select(n);
    return db;
  };

exports.nextAvailableKeyP =
  function(db) {
    return function(cb) {
      return function() {
        debugPrint('incr count');
        db.incr('count', function(err, val) {
          cb(val - 1)();
        });
      };
    };
  };

exports.insert =
  function(key) {
    return function(val) {
      return function(db) {
        return function(cb) {
          return function() {
            debugPrint('set ' + key + ' ' + val);
            db.set(key, val, 'nx', function(err, ok) {
              cb(err == null && ok != null)();
            });
          }
        };
      };
    };
  };

exports.lookupP =
  function(Nothing) {
    return function(Just) {
      return function(key) {
        return function(db) {
          return function(cb) {
            return function() {
              debugPrint('get ' + key);
              db.get(key, function(err, val) {
                cb(err == null && val != null ? Just(val) : Nothing)();
              });
            };
          };
        };
      };
    };
  };

exports.sampleLinks =
  function(n) {
    return function(db) {
      return function(cb) {
        return function() {
          debugPrint('scan 0 count ' + n);
          db.scan(0, 'count', n, function(err, response) {
            var shortNames = response[1];
            db.mget(shortNames, function(err, urls) {
              debugPrint('mget ' + shortNames.join(' '));
              var result = Object.create(null);
              for (var i = 0, l = shortNames.length; i < l; ++i) {
                result[shortNames[i]] = urls[i];
              }
              cb(result)();
            });
          });
        };
      };
    };
  };
