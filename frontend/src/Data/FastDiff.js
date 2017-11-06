"use strict";

var diff = require('fast-diff');

function rawDiff(Tuple, a, b) {
  return diff(a, b).map(function(result) {
    return Tuple(op)(result[1]);
  });
}

module.exports = {
  rawDiff: rawDiff
}
