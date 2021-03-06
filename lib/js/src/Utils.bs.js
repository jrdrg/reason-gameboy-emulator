// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

var toArray = (function(response) { return new Uint8Array(response); });

function openUri(method__, uri, xhr) {
  return new Promise((function (resolve, param) {
                xhr.onload = (function (param) {
                    return resolve(Curry._1(toArray, xhr.response));
                  });
                xhr.responseType = "arraybuffer";
                xhr.open((function () {
                          switch (method__) {
                            case 3546230 :
                                return "GET";
                            case 891112544 :
                                return "POST";
                            
                          }
                        })(), uri, true);
                xhr.send();
                return /* () */0;
              }));
}

var Xhr = {
  toArray: toArray,
  openUri: openUri
};

var getContextFromId = (
      function(id) {
        const canvas = document.getElementById(id)
        return canvas.getContext("2d");
      }
  );

var Canvas = {
  getContextFromId: getContextFromId
};

var $$Window = { };

var $$Document = { };

exports.Xhr = Xhr;
exports.Canvas = Canvas;
exports.$$Window = $$Window;
exports.$$Document = $$Document;
/* toArray Not a pure module */
