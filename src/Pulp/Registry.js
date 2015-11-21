// module Pulp.Registry

const Promise = require("bluebird");
const path = require("path");
const mkdirp = Promise.promisify(require("mkdirp"));
const home = require("home-or-tmp");

const loadPaths = {
  npm: "jspm-npm",
  bower: "jspm-bower-endpoint"
};

exports["registry'"] = (name) => {
  const tmpDir = path.resolve(home, ".cache", "pulp", name);
  if (!loadPaths.hasOwnProperty(name)) {
    throw new Error(`Registry "${name}" is unknown.`);
  }
  const Reg = require(loadPaths[name]);
  return mkdirp(tmpDir).then(() => new Reg({name, tmpDir}, {
    log(level, msg) {
      console.log(level, msg);
    }
  }));
};

exports["lookup'"] = (reg) => (pkg) => {
  return reg.lookup(pkg);
};


exports["download'"] = (reg) => (pkg) => (ver) => (entry) => (dir) => {
  return mkdirp(dir).then(() => reg.download(pkg, ver, entry.hash, entry.meta, dir));
};
