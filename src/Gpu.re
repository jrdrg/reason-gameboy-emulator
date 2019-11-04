let screen_width = 160;
let screen_height = 144;

type gpuMode =
  | Hblank
  | Vblank
  | OamRead
  | VramRead;

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

/**
 * 384 8x8 tiles
 */
let initTileset = () => Array.make(512, Array.make(8, Array.make(8, 0)));

let make = () => {
  bgmap: 0,
  bgtile: 0,
  mode: Hblank,
  clock: 0,
  line: 0,
  vram: Array.make(8192, 0),
  oam: Array.make(160, 0),
  tileset: initTileset(),
};

let reset = () => {
  bgmap: 0,
  bgtile: 0,
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

let renderScan = (renderer: Renderer.t, gpu: t) => {
  open Belt;
  /* TODO: move this into GPU.t */
  let scrx = 0;
  let scry = 0;
  let palette = [|55, 192, 96, 0|];
  /* 255; break;
     case 1: GPU._palette.bg[i] = 192; break;
     case 2: GPU._palette.bg[i] = 96; break;
     case 3: GPU._palette.bg[i] = 0; b  */
  // let {screen} = renderer;
  // let screenData = screen |> Canvas.data;

  let mapoffs = gpu.bgmap == 1 ? 0x1C00 : 0x1800;

  // Which line of tiles to use in the map
  let mapoffs = mapoffs + (gpu.line + scry land 255) lsr 3;

  // Which tile to start with in the map line
  let lineoffs = scrx lsr 3;

  // Which line of pixels to use in the tiles
  let y = (gpu.line + scry) land 7;

  // Where in the tileline to start
  let x = scrx land 7;

  let tile = gpu.vram[mapoffs + lineoffs]->Option.getWithDefault(0);

  // If the tile data set in use is #1, the
  // indices are signed; calculate a real tile offset
  let tile = gpu.bgtile == 1 && tile < 128 ? tile + 256 : tile;

  // Where to render on the canvas
  let canvasoffs = gpu.line * screen_width * 4;

  let rec setPixels = (scrX: int, tile: int, xc: int, cOff: int, lOff: int) => {
    switch (scrX) {
    | 160 => ()
    | _ =>
      let (tile', xc', cOff', lOff') =
        gpu.tileset[tile]
        |> Js.Option.andThen((. t) => t[y])
        |> Js.Option.andThen((. row) => row[xc])
        |> Js.Option.andThen((. p) => palette[p])
        |> Js.Option.map((. color) => {
             Renderer.setPixel(renderer, ~pixel=cOff + 3, ~color);

             let tile' =
               xc == 8 ?
                 gpu.vram[mapoffs + lineoffs]->Option.getWithDefault(0) : tile;

             let (xc', lOff', tile') =
               xc == 8 ?
                 (
                   0,
                   (lOff + 1) land 0b11111, // 31
                   gpu.bgtile == 1 && tile' < 128 ? tile' + 256 : tile',
                 ) :
                 (xc + 1, lOff, tile);

             (tile', xc', cOff + 4, lOff');
           })
        |> Js.Option.getWithDefault((tile, xc, cOff, lOff));

      setPixels(scrX + 1, tile', xc', cOff', lOff');
    };
  };

  setPixels(0, tile, x, canvasoffs, lineoffs);
  gpu;
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
      let clock = 0;
      let line = gpu.line + 1;
      if (line == screen_height - 1) {
        /* enter vblank */
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
      let clock = 0;
      let line = gpu.line + 1;
      /* 10 lines after 143 */
      if (line > screen_height - 1 + 10) {
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
      {
        /* enter hblank */
        ...gpu,
        clock: 0,
        mode: Hblank,
      }
      |> renderScan(renderer);
    } else {
      gpu;
    }
  };
};

let read8 = _gpu => 0;

let write8 = gpu => gpu;
