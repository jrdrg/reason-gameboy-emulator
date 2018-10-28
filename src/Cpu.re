/* http://gameboy.mongenel.com/dmg/opcodes.html */
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
  mCycles: int,
};

type t = {
  clock: int,
  registers,
};

exception UnhandledInstruction(string);

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
    /* machine cycles, 1 mC = 4 clock cycles */
    mCycles: 0,
  },
};

let programCount = cpu => cpu.registers.pc;

/* AF,BC,DE, & */
let rAf = ({registers: {a, f}}) => f + a lsl 8;

let rBc = ({registers: {b, c}}) => c + b lsl 8;

let rDe = ({registers: {d, e}}) => e + d lsl 8;

let rHl = ({registers: {h, l}}) => l + h lsl 8;

let setPc = (t, v) => {...t, pc: v};

let incSp = t => {...t, sp: t.sp + 1};

let decSp = t => {...t, sp: t.sp - 1};

let machineCycles = (cycles, registers) => {...registers, mCycles: cycles};

module Ops = {
  let nop = (cpu, mmu) => (
    {...cpu, registers: machineCycles(1, cpu.registers)},
    mmu,
  );

  let ld_bc_nn = (cpu, mmu) => {
    let pc = programCount(cpu);
    let c = mmu |> Mmu.read8(pc);
    let b = mmu |> Mmu.read8(pc + 1);
    (
      {
        ...cpu,
        registers: {
          ...cpu.registers,
          b,
          c,
          pc: pc + 2,
          mCycles: 3,
        },
      },
      mmu,
    );
  };

  let ld_bc_a = (cpu, mmu) => {
    let {a} = cpu.registers;
    ({
       ...cpu,
       registers: {
         ...cpu.registers,
         mCycles: 2,
       },
     }, mmu);
  };
  /* let inc_bc = cpu => {
       ...cpu,
       registers: {
         ...cpu.registers,
         c:
       },
     }; */
};

let exec = instruction =>
  switch (instruction) {
  | 0x00 => Ops.nop
  | 0x01 => Ops.ld_bc_nn
  | _ =>
    raise(
      UnhandledInstruction(
        Printf.sprintf("Unhandled instruction, %#2x", instruction),
      ),
    )
  };