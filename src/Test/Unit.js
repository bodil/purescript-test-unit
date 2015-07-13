/* global exports */
"use strict";

// module Test.Unit

exports.exit = function exit(rv) {
  return function() {
    try { process.exit(rv); } catch (e) {
      try { phantom.exit(rv); } catch (e) {}
    };
  };
};

exports.setTimeout = function(duration) {
  return function(cb) {
    return function () {
      setTimeout(function () {
        cb();
      }, duration);
      return {};
    };
  };
};
