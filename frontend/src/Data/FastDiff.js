"use strict";

var diff = require('fast-diff')
var Tuple = require('Data.Tuple');
var FastDiff = require('Data.FastDiff');

var OPS = [FastDiff.Insert, FastDiff.Equal, FastDiff.Delete];

module.exports = {
  rawDiff: function rawDiff(a, b) {
    return diff(a, b).map(function(result) {
      var op = OPS[result[0]];
      return Tuple(op)(result[1]);
    });
  }
}
