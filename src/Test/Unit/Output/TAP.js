//module Test.Unit.Output.TAP

export var requested = (function() {
  try {
    if (process.argv.indexOf("--tap") >= 0
        || process.argv.indexOf("tap") >= 0) {
      return true;
    }
    return false;
  } catch (e) {
    return false;
  }
})();
