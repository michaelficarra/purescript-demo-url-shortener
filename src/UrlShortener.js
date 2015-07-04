// module UrlShortener

exports.port = process.env.PORT || 8080;

exports.linkColour =
  function(s) {
    return "\x1B[0;36m" + s + "\x1B[0m";
  };

exports.httpMethodColour =
  function(s) {
    return "\x1B[0;33m" + s + "\x1B[0m";
  };
