/* http://gameboy.mongenel.com/dmg/opcodes.html */
module Flags = {
  let z = 7;
  let n = 6;
  let h = 5;
  let c = 4;
  type flag =
    | Z
    | N
    | H
    | C;
  let flagOffset = flag =>
    switch (flag) {
    | Z => z
    | N => n
    | H => h
    | C => c
    };
  let setFlag = (flag, value, flags) => {
    let bit = 1 lsl flagOffset(flag);
    switch (value) {
    | 0 => flags land lnot(bit)
    | _ => flags lor bit
    };
  };
};

type register8 =
  | A
  | B
  | C
  | D
  | E
  | F
  | H
  | L;

type register16 =
  | AF
  | BC
  | DE
  | HL;

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

let b2i = bool => bool ? 1 : 0;

let programCount = cpu => cpu.registers.pc;

let readRegister16 =
    (register: register16, {registers: {a, b, c, d, e, f, h, l}}) => {
  let (r1, r2) =
    switch (register) {
    | AF => (a, f)
    | BC => (b, c)
    | DE => (d, e)
    | HL => (h, l)
    };
  r2 + r1 lsl 8;
};

/* AF,BC,DE, & */
let rAf = readRegister16(AF);

let rBc = readRegister16(BC);

let rDe = readRegister16(DE);

let rHl = readRegister16(HL);

let incrementPc = (cycles, cpu) => {
  ...cpu,
  registers: {
    ...cpu.registers,
    pc: cpu.registers.pc + cycles,
  },
};

let incrementSp = t => {...t, sp: t.sp + 1};

let decrementSp = t => {...t, sp: t.sp - 1};

let setFlag = (flag, value, ~initialValue=?, cpu) => {
  ...cpu,
  registers: {
    ...cpu.registers,
    f:
      Flags.setFlag(
        flag,
        value,
        Belt.Option.getWithDefault(initialValue, cpu.registers.f),
      ),
  },
};

let getFlag = (flag, cpu) => {
  let bit = 1 lsl Flags.flagOffset(flag);
  cpu.registers.f land bit;
};

let setRegisters = (~a=?, ~b=?, ~c=?, ~d=?, ~e=?, ~h=?, ~l=?, cpu) => {
  ...cpu,
  registers: {
    ...cpu.registers,
    a: Belt.Option.getWithDefault(a, cpu.registers.a),
    b: Belt.Option.getWithDefault(b, cpu.registers.b),
    c: Belt.Option.getWithDefault(c, cpu.registers.c),
    d: Belt.Option.getWithDefault(d, cpu.registers.d),
    e: Belt.Option.getWithDefault(e, cpu.registers.e),
    h: Belt.Option.getWithDefault(h, cpu.registers.h),
    l: Belt.Option.getWithDefault(l, cpu.registers.l),
  },
};

let machineCycles = (cycles, cpu) => {
  ...cpu,
  registers: {
    ...cpu.registers,
    mCycles: cycles,
  },
};

module Ops = {
  let nop = (cpu, mmu) => (machineCycles(1, cpu), mmu);
  let ld_bc_nn = (cpu, mmu) => {
    let pc = programCount(cpu);
    let (c, m) = mmu |> Mmu.read8(pc);
    let (b, m1) = m |> Mmu.read8(pc + 1);
    (
      cpu |> setRegisters(~b, ~c) |> machineCycles(3) |> incrementPc(2),
      m1,
    );
  };
  let ld_m_bc_a = (cpu, mmu: Mmu.t) => {
    let {a} = cpu.registers;
    let mmuWrite = Mmu.write8(rBc(cpu), a, mmu);
    ({
       ...cpu,
       registers: {
         ...cpu.registers,
         mCycles: 2,
       },
     }, mmuWrite);
  };
  let inc_bc = (cpu, mmu) => {
    let c = (cpu.registers.c + 1) land 0xff; /* wrap after 255, i.e. 256 = 0 */
    let b =
      if (c === 0) {
        cpu.registers.b + 1;
      } else {
        cpu.registers.b;
      };
    (cpu |> setRegisters(~b, ~c) |> machineCycles(3), mmu);
  };
  let inc_b = (cpu, mmu) => {
    let b = (cpu.registers.b + 1) land 0xff;
    let f = cpu.registers.f land 0x10;
    /* let f = Flags.setFlag(Z, b === 0 ? 1 : 0, f); */
    (
      cpu |> setRegisters(~b) |> setFlag(Z, b2i(b === 0), ~initialValue=f),
      mmu,
    );
  };
  let dec_b = (cpu, mmu) => {
    let b = (cpu.registers.b - 1) land 0xff;
    let f = cpu.registers.f land 0x10;
    (
      cpu
      |> setRegisters(~b)
      |> setFlag(Z, b2i(b === 0), ~initialValue=f)
      |> setFlag(N, 1),
      mmu,
    );
  };
};

let exec = instruction =>
  switch (instruction) {
  | 0x00 => Ops.nop
  | 0x01 => Ops.ld_bc_nn
  | 0x02 => Ops.ld_m_bc_a
  | 0x03 => Ops.inc_bc
  | 0x04 => Ops.inc_b
  | 0x05 => Ops.dec_b
  | _ =>
    raise(
      UnhandledInstruction(
        Printf.sprintf("Unhandled instruction, %#2x", instruction),
      ),
    )
  };
