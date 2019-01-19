
const semver = require('semver');

exports._valid = semver.valid

exports._eq = semver.eq
exports._gt = semver.gt

exports._validRange = semver.validRange
exports._satisfies = semver.satisfies
exports._maxSatisfying = semver.maxSatisfying
