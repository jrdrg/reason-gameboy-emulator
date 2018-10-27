// Generated by BUCKLESCRIPT VERSION 4.0.6, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Utils$GameboyEmulator = require("./Utils.bs.js");
var Emulator$GameboyEmulator = require("./Emulator.bs.js");

function loop(state) {
  var newState = Emulator$GameboyEmulator.frame(state);
  requestAnimationFrame((function () {
          return loop(newState);
        }));
  return /* () */0;
}

var foo = Utils$GameboyEmulator.Xhr[/* openUri */1](/* GET */3546230, "../roms/cpu_instrs/cpu_instrs.gb", new XMLHttpRequest()).then((function (bytes) {
          console.log(bytes);
          return Promise.resolve(bytes);
        })).then((function (bytes) {
        var canvas = Curry._1(Utils$GameboyEmulator.Canvas[/* getCanvas */0], "screen");
        return Promise.resolve(loop(Emulator$GameboyEmulator.load(bytes, canvas)));
      }));

exports.loop = loop;
exports.foo = foo;
/* foo Not a pure module */
