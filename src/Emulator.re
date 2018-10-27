/* http://gameboy.mongenel.com/dmg/opcodes.html */
/* http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf */

module Z80 = {
  type t = {i: int};
  let make = () => {i: 0};
};

type state = {
  cpu: Z80.t,
  mmu: Mmu.t,
  rom: array(int),
};

let load = bytes => {
  Js.log2("Loaded, ROM length: ", Array.length(bytes));
  let mmu = Mmu.load(bytes);
  let b = Mmu.read8(Mmu.load(bytes), 0x0001);
  Js.log2("Byte:", b);
  {rom: bytes, cpu: Z80.make(), mmu};
};

let reset = state => {...state, mmu: state.mmu |> Mmu.reset};

let frame = (s: state) => s;