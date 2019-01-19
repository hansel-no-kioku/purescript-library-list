
exports._parseJSON = function(str, done, fail) {
  try {
    return done(JSON.parse(str));
  } catch(e) {
    return fail(e);
  }
};