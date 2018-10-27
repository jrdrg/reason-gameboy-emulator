// Generated by BUCKLESCRIPT VERSION 4.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");

var bios = /* array */[
  49,
  254,
  255,
  175,
  33,
  255,
  159,
  50,
  203,
  124,
  32,
  251,
  33,
  38,
  255,
  14,
  17,
  62,
  128,
  50,
  226,
  12,
  62,
  243,
  226,
  50,
  62,
  119,
  119,
  62,
  252,
  224,
  71,
  17,
  4,
  1,
  33,
  16,
  128,
  26,
  205,
  149,
  0,
  205,
  150,
  0,
  19,
  123,
  254,
  52,
  32,
  243,
  17,
  216,
  0,
  6,
  8,
  26,
  19,
  34,
  35,
  5,
  32,
  249,
  62,
  25,
  234,
  16,
  153,
  33,
  47,
  153,
  14,
  12,
  61,
  40,
  8,
  50,
  13,
  32,
  249,
  46,
  15,
  24,
  243,
  103,
  62,
  100,
  87,
  224,
  66,
  62,
  145,
  224,
  64,
  4,
  30,
  2,
  14,
  12,
  240,
  68,
  254,
  144,
  32,
  250,
  13,
  32,
  247,
  29,
  32,
  242,
  14,
  19,
  36,
  124,
  30,
  131,
  254,
  98,
  40,
  6,
  30,
  193,
  254,
  100,
  32,
  6,
  123,
  226,
  12,
  62,
  135,
  242,
  240,
  66,
  144,
  224,
  66,
  21,
  32,
  210,
  5,
  32,
  79,
  22,
  32,
  24,
  203,
  79,
  6,
  4,
  197,
  203,
  17,
  23,
  193,
  203,
  17,
  23,
  5,
  32,
  245,
  34,
  35,
  34,
  35,
  201,
  206,
  237,
  102,
  102,
  204,
  13,
  0,
  11,
  3,
  115,
  0,
  131,
  0,
  12,
  0,
  13,
  0,
  8,
  17,
  31,
  136,
  137,
  0,
  14,
  220,
  204,
  110,
  230,
  221,
  221,
  217,
  153,
  187,
  187,
  103,
  99,
  110,
  14,
  236,
  204,
  221,
  220,
  153,
  159,
  187,
  185,
  51,
  62,
  60,
  66,
  185,
  165,
  185,
  165,
  66,
  76,
  33,
  4,
  1,
  17,
  168,
  0,
  26,
  19,
  190,
  32,
  254,
  35,
  125,
  254,
  52,
  32,
  245,
  6,
  25,
  120,
  134,
  35,
  5,
  32,
  251,
  134,
  32,
  254,
  62,
  1,
  224,
  80
];

function load(bytes) {
  return /* record */[
          /* finishedBios */true,
          /* cartType */Caml_array.caml_array_get(bytes, 327),
          /* oam : array */[],
          /* rom */bytes,
          /* externalRam : array */[],
          /* videoRam : array */[],
          /* workRam : array */[]
        ];
}

function reset(mmu) {
  return /* record */[
          /* finishedBios */false,
          /* cartType */mmu[/* cartType */1],
          /* oam : array */[],
          /* rom */mmu[/* rom */3],
          /* externalRam */Caml_array.caml_make_vect(8192, 0),
          /* videoRam */Caml_array.caml_make_vect(8192, 0),
          /* workRam */Caml_array.caml_make_vect(8192, 0)
        ];
}

function read8(mmu, addr) {
  console.log(Curry._1(Printf.sprintf(/* Format */[
                /* Int */Block.__(4, [
                    /* Int_x */6,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* End_of_format */0
                  ]),
                "%x"
              ]), 57344));
  console.log(Curry._1(Printf.sprintf(/* Format */[
                /* String_literal */Block.__(11, [
                    ">> ",
                    /* Int */Block.__(4, [
                        /* Int_x */6,
                        /* No_padding */0,
                        /* No_precision */0,
                        /* End_of_format */0
                      ])
                  ]),
                ">> %x"
              ]), 3856));
  var match = addr & 61440;
  var exit = 0;
  if (match >= 28673) {
    if (match >= 45057) {
      if (match >= 53249) {
        if (match !== 57344) {
          if (match !== 61440) {
            return 0;
          } else {
            var match$1 = addr & 3840;
            if (match$1 !== 3584 && match$1 !== 3840) {
              return Caml_array.caml_array_get(mmu[/* workRam */6], addr & 8191);
            } else {
              return -1;
            }
          }
        } else {
          exit = 1;
        }
      } else if (match !== 49152 && match < 53248) {
        return 0;
      } else {
        exit = 1;
      }
    } else if (match >= 36865) {
      if (match !== 40960 && match < 45056) {
        return 0;
      } else {
        return -1;
      }
    } else if (match !== 32768 && match < 36864) {
      return 0;
    } else {
      return -1;
    }
  } else if (match >= 12289) {
    if (match >= 20481) {
      if (match !== 24576 && match < 28672) {
        return 0;
      } else {
        return -1;
      }
    } else if (match !== 16384 && match < 20480) {
      return 0;
    } else {
      return -1;
    }
  } else if (match >= 4097) {
    if (match !== 8192 && match < 12288) {
      return 0;
    } else {
      return Caml_array.caml_array_get(mmu[/* rom */3], addr);
    }
  } else if (match !== 0) {
    if (match >= 4096) {
      return Caml_array.caml_array_get(mmu[/* rom */3], addr);
    } else {
      return 0;
    }
  } else {
    var match$2 = mmu[/* finishedBios */0];
    var match$3 = addr <= 255;
    if (match$2 || !match$3) {
      return Caml_array.caml_array_get(mmu[/* rom */3], addr);
    } else {
      return Caml_array.caml_array_get(bios, addr);
    }
  }
  if (exit === 1) {
    return Caml_array.caml_array_get(mmu[/* workRam */6], addr & 8191);
  }
  
}

exports.bios = bios;
exports.load = load;
exports.reset = reset;
exports.read8 = read8;
/* No side effect */
