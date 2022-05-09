/* global exports */
"use strict";

// module Test.Unit.Console

var hasStderr;
try { hasStderr = !!process.stderr; } catch (e) { hasStderr = false; }
export { hasStderr };

export var hasColours = (function() {
  if (typeof process === "undefined") {
    return false;
  }
  if (process.stdout && !process.stdout.isTTY) {
    return false;
  }
  if (process.platform === "win32" || "COLORTERM" in process.env) {
    return true;
  }
  if (process.env.TERM === "dumb") {
    return false;
  }
  if (/^screen|^xterm|^vt100|color|ansi|cygwin|linux/i.test(process.env.TERM)) {
    return true;
  }
  return false;
})();

export function consoleLog(s) {
  return function() {
    console.log(s);
  };
};
export function consoleError(s) {
  return function() {
    console.error(s);
  };
};
export function savePos() {
  process.stderr.write("\x1b[s");
};
export function restorePos() {
  process.stderr.write("\x1b[u");
};
export function eraseLine() {
  process.stderr.write("\x1b[K");
};
export function print(s) {
  return function() {
    process.stderr.write("\x1b[33m" + s + "\x1b[0m");
  };
};
export function printLabel(s) {
  return function() {
    process.stderr.write("\x1b[33;1m" + s + "\x1b[0m");
  };
};
export function printFail(s) {
  return function() {
    process.stderr.write("\x1b[31;1m" + s + "\x1b[0m");
  };
};
export function printPass(s) {
  return function() {
    process.stderr.write("\x1b[32m" + s + "\x1b[0m");
  };
};
