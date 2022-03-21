// module Test.Unit.Main

export function exit(rv) {
  return function () {
    try {
      process.exit(rv);
    } catch (e) {
      try {
        phantom.exit(rv);
      } catch (e) {}
    }
  };
}
