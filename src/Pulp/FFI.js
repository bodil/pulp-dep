// module Pulp.FFI

"use strict";

exports["runNode'"] = function runNode$prime(error, success, fn) {
  return () => fn((err, val) => err ? error(err)() : success(val)());
};

exports["runPromise'"] = function runPromise$prime(error, success, p) {
  return () => p.then(val => success(val)(), err => error(err)());
};

exports.unsafeInspect = function unsafeInspect(obj) {
  return require('util').inspect(obj);
};
