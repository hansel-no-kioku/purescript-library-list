
const execa = require('execa');

exports.execaImpl = function(command, args, done, fail) {
  execa(command, args)
    .then(function(result) { done(result.stdout)(); })
    .catch(function(error) { fail(error)(); });
};
