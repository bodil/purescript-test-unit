// module Test.Main

var ref = 0;

exports.resetRef = function(cb) {
  ref = 0;
  cb(ref);
};

exports.incRef = function(cb) {
  cb(++ref);
  if (ref > 1) {
    console.error("ERROR: A test ran twice and test-unit failed to catch it.");
    process.exit(1);
  }
};
