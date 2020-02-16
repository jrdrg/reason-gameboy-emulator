type gpuMode;

type t = {
  bgmap: int,
  bgtile: int,
  clock: int,
  line: int,
  mode: gpuMode,
  oam: array(int),
  tileset: array(array(array(int))),
  vram: array(int),
};

let make: unit => t;

let reset: unit => t;

let updateTile: (int, t) => unit;

let step: (int, Renderer.t, t) => t;

let read8: (int, t) => int;

let write8: (int, int, t) => t;