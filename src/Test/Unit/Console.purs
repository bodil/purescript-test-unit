module Test.Unit.Console where

import Control.Monad.Eff

foreign import data TestOutput :: !

foreign import hasStderr """
  var hasStderr;
  try { hasStderr = !!process.stderr; } catch (e) { hasStderr = false; }
""" :: Boolean

foreign import consoleLog """
  function consoleLog(s) {
    return function() {
      console.log(s);
    };
  }""" :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit

foreign import consoleError """
  function consoleError(s) {
    return function() {
      console.error(s);
    };
  }""" :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit

foreign import savePos """
  function savePos() {
    process.stderr.write("\x1b[s");
  }""" :: forall e. Eff (testOutput :: TestOutput | e) Unit

foreign import restorePos """
  function restorePos() {
    process.stderr.write("\x1b[u");
  }""" :: forall e. Eff (testOutput :: TestOutput | e) Unit

foreign import eraseLine """
  function eraseLine() {
    process.stderr.write("\x1b[K");
  }""" :: forall e. Eff (testOutput :: TestOutput | e) Unit

foreign import print """
  function print(s) {
    return function() {
      process.stderr.write("\x1b[33m" + s + "\x1b[0m");
    };
  }""" :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit

foreign import printLabel """
  function printLabel(s) {
    return function() {
      process.stderr.write("\x1b[33;1m" + s + "\x1b[0m");
    };
  }""" :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit

foreign import printFail """
  function printFail(s) {
    return function() {
      process.stderr.write("\x1b[31;1m" + s + "\x1b[0m");
    };
  }""" :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit

foreign import printPass """
  function printPass(s) {
    return function() {
      process.stderr.write("\x1b[32m" + s + "\x1b[0m");
    };
  }""" :: forall e. String -> Eff (testOutput :: TestOutput | e) Unit
