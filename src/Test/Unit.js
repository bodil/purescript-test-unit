// module Test.Unit

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
