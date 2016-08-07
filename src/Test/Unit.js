// module Test.Unit

exports.memoise = function(aff) {
  var fresh = true;
  var listeners = [];
  var resultValue = undefined;
  var failed = false;
  var done = false;
  return function(success, failure) {
    if (done) {
      if (failed) {
        failure(resultValue);
      } else {
        success(resultValue);
      }
      return;
    }

    listeners.push({success: success, failure: failure});

    if (fresh) {
      fresh = false;
      aff(function(result) {
        done = true;
        resultValue = result;
        listeners.forEach(function(listener) {
          listener.success(result);
        });
        listeners = [];
      }, function(error) {
        done = true;
        failed = true;
        resultValue = error;
        listeners.forEach(function(listener) {
          listener.failure(error);
        });
        listeners = [];
      });
    }
  };
};
