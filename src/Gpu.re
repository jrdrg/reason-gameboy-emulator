type gpuMode =
  | Hblank
  | Vblank
  | OamRead
  | VramRead;

type t = {
  mode: gpuMode,
  clock: int,
  line: int,
  vram: array(int),
  oam: array(int), /* 0xFE00 to 0xFE9F */
  tileset: array(array(array(int))),
};

/**
 * 384 8x8 tiles
 */
let initTileset = () => Array.make(512, Array.make(8, Array.make(8, 0)));

let make = () => {
  mode: Hblank,
  clock: 0,
  line: 0,
  vram: Array.make(8192, 0),
  oam: Array.make(160, 0),
  tileset: initTileset(),
};

let reset = () => {
  mode: Hblank,
  line: 0,
  clock: 0,
  vram: Array.make(8192, 0),
  oam: Array.make(160, 0),
  tileset: initTileset(),
};

/**
 * Gets a value written to the VRAM and updates the GPU tileset
 */
let updateTile = (addr: int, gpu: t) => {
  /*
   http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-Graphics

   Region	    Usage
   8000-87FF	Tile set #1: tiles 0-127
   8800-8FFF	Tile set #1: tiles 128-255
              Tile set #0: tiles -1 to -128
   9000-97FF	Tile set #0: tiles 0-127
   9800-9BFF	Tile map #0
   9C00-9FFF	Tile map #1
   */
  /* vram is 8192 bytes */
  let addr = addr land 0x1ffe; /* 8190 */
  /* 511 = 0b111111111 */
  let tile = addr lsr 4 land 511; /* 0x1ffe >> 4 = 511: Remove last 4 bits then get last 9 bits */
  let y = addr lsr 1 land 7; /* Remove the last bit then get the last 3 bits of the result */
  Belt.Range.forEach(
    0,
    7,
    x => {
      Js.log2("updateTile", x);
      /* Get bit index for this pixel */
      let sx = 1 lsl (7 - x);
      let b1 = gpu.vram[addr] land sx > 0 ? 1 : 0;
      let b2 = gpu.vram[addr + 1] land sx > 0 ? 2 : 0;
      gpu.tileset[tile][y][x] = b1 + b2;
      ();
    },
  );
  ();
};

let step = (mCycles: int, renderer: Renderer.t, gpu: t) => {
  let modeclock = gpu.clock + mCycles;
  let gpu = {...gpu, clock: modeclock};
  /*
   scanline (oam): mode 2, 20 machine cycles
   scanline (vram): mode 3, 43 machine cycles
   hblank: mode 0, 51 machine cycles
   vblank: mode 1, 1140 machine cycles - 10 lines
           1 line: 114 machine cycles
   */
  switch (gpu.mode) {
  /* hblank - after last one, render to canvas */
  | Hblank =>
    if (modeclock >= 51) {
      Js.log("hblank");
      let clock = 0;
      let line = gpu.line + 1;
      if (line == 143) {
        /* enter vblank */
        Js.log("Render to screen");
        let gpu' = {...gpu, clock, line, mode: Vblank};
        Renderer.renderToScreen(renderer);
        gpu';
      } else {
        {...gpu, clock, line, mode: OamRead};
      };
    } else {
      gpu;
    }
  /* vblank - 10 lines */
  | Vblank =>
    if (modeclock >= 114) {
      Js.log("vblank");
      let clock = 0;
      let line = gpu.line + 1;
      /* 10 lines after 143 */
      if (line > 153) {
        {...gpu, clock, line: 0, mode: OamRead};
      } else {
        {...gpu, clock, line};
      };
    } else {
      gpu;
    }
  /* oam read */
  | OamRead =>
    if (modeclock >= 20) {
      Js.log("oam read");
      {
        /* enter scanline mode */
        ...gpu,
        clock: 0,
        mode: VramRead,
      };
    } else {
      gpu;
    }
  /* vram read, scanline active */
  | VramRead =>
    if (modeclock >= 43) {
      Js.log("vram read");
      {
        /* enter hblank */
        ...gpu,
        clock: 0,
        mode: Hblank,
      }
      |> Renderer.renderScan(renderer);
    } else {
      gpu;
    }
  };
};

let read8 = _gpu => 0;

let write8 = gpu => gpu;
