/* http://gameboy.mongenel.com/dmg/opcodes.html */
/* http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf */

module Cpu = {
  type registers = {
    a: int,
    b: int,
    c: int,
    d: int,
    e: int,
    h: int,
    l: int,
    f: int,
    sp: int,
    pc: int,
  };
  type t = {
    clock: int,
    registers,
  };
  let make = () => {
    clock: 0,
    registers: {
      a: 0,
      b: 0,
      c: 0,
      d: 0,
      e: 0,
      h: 0,
      l: 0,
      f: 0,
      sp: 0,
      pc: 0,
    },
  };
};

module Gpu = {
  type t = {
    canvas: Utils.Canvas.ctx,
    vram: array(int),
    oam: array(int) /* 0xFE00 to 0xFE9F */
  };
  let make = canvas => {
    Js.log(canvas);
    {canvas, vram: Array.make(8192, 0), oam: Array.make(160, 0)};
  };

  let reset = () => {};
};

type state = {
  gpu: Gpu.t,
  cpu: Cpu.t,
  mmu: Mmu.t,
  rom: array(int),
};

let load = (bytes, canvas) => {
  Js.log2("Loaded, ROM length: ", Array.length(bytes));
  let mmu = Mmu.load(bytes);
  let b = Mmu.read8(mmu, 0x0001);
  Js.log2("Byte:", b);
  {rom: bytes, gpu: Gpu.make(canvas), cpu: Cpu.make(), mmu};
};

let reset = state => {...state, mmu: state.mmu |> Mmu.reset};

let frame = (s: state) => s;