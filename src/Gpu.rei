type gpuMode;

type t = {
  mode: gpuMode,
  clock: int,
  line: int,
  vram: array(int),
  oam: array(int),
  tileset: array(array(array(int))),
};

let make: unit => t;

let reset: unit => t;

let updateTile: (int, t) => unit;

let step: (int, Renderer.t, t) => t;

let read8: t => int;

let write8: t => t;
