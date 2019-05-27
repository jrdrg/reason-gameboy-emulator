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
  let nop = (cpu, mmu: Mmu.t, _gpu) => (machineCycles(1, cpu), mmu);
  let ld_bc_nn = (cpu, mmu, _gpu) => {
    let pc = programCount(cpu);
    let (c, m) = mmu |> Mmu.read8(pc);
    let (b, m) = m |> Mmu.read8(pc + 1);
    (cpu |> setRegisters(~b, ~c) |> machineCycles(3) |> incrementPc(2), m);
  };
  let ld_m_bc_a = (cpu, mmu, gpu) => {
    let {a} = cpu.registers;
    let (mmuWrite, _gpu) = Mmu.write8(rBc(cpu), a, mmu, gpu);
    (cpu |> machineCycles(2), mmuWrite);
  };
  let inc_bc = (cpu, mmu, _gpu) => {
    let c = (cpu.registers.c + 1) land 0xff; /* wrap after 255, i.e. 256 = 0 */
    let b =
      if (c === 0) {
        cpu.registers.b + 1;
      } else {
        cpu.registers.b;
      };
    (cpu |> setRegisters(~b, ~c) |> machineCycles(2), mmu);
  };
  let inc_b = (cpu, mmu, _gpu) => {
    let b = (cpu.registers.b + 1) land 0xff;
    let f = cpu.registers.f land 0x10;
    /* let f = Flags.setFlag(Z, b === 0 ? 1 : 0, f); */
    (
      cpu
      |> setRegisters(~b)
      |> setFlag(Z, b2i(b === 0), ~initialValue=f)
      |> machineCycles(1),
      mmu,
    );
  };
  let dec_b = (cpu, mmu, _gpu) => {
    let b = (cpu.registers.b - 1) land 0xff;
    let f = cpu.registers.f land 0x10;
    (
      cpu
      |> setRegisters(~b)
      |> setFlag(Z, b2i(b === 0), ~initialValue=f)
      |> setFlag(N, 1)
      |> machineCycles(2),
      mmu,
    );
  };
  let ld_b_n = (cpu, mmu, _gpu) => {
    let pc = programCount(cpu);
    let (b, m) = mmu |> Mmu.read8(pc);
    (cpu |> setRegisters(~b) |> machineCycles(2) |> incrementPc(1), m);
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
  | 0x06 => Ops.ld_b_n
  | 0x07 => Ops.nop
  | 0x08 => Ops.nop
  | 0x09 => Ops.nop
  | 0x0A => Ops.nop
  | 0x0B => Ops.nop
  | 0x0C => Ops.nop
  | 0x0D => Ops.nop
  | 0x0E => Ops.nop
  | 0x0F => Ops.nop
  | 0x10 => Ops.nop
  | 0x11 => Ops.nop
  | 0x12 => Ops.nop
  | 0x13 => Ops.nop
  | 0x14 => Ops.nop
  | 0x15 => Ops.nop
  | 0x16 => Ops.nop
  | 0x17 => Ops.nop
  | 0x18 => Ops.nop
  | 0x19 => Ops.nop
  | 0x1A => Ops.nop
  | 0x1B => Ops.nop
  | 0x1C => Ops.nop
  | 0x1D => Ops.nop
  | 0x1E => Ops.nop
  | 0x1F => Ops.nop
  | 0x20 => Ops.nop
  | 0x21 => Ops.nop
  | 0x22 => Ops.nop
  | 0x23 => Ops.nop
  | 0x24 => Ops.nop
  | 0x25 => Ops.nop
  | 0x26 => Ops.nop
  | 0x27 => Ops.nop
  | 0x28 => Ops.nop
  | 0x29 => Ops.nop
  | 0x2A => Ops.nop
  | 0x2B => Ops.nop
  | 0x2C => Ops.nop
  | 0x2D => Ops.nop
  | 0x2E => Ops.nop
  | 0x2F => Ops.nop
  | 0x30 => Ops.nop
  | 0x31 => Ops.nop
  | 0x32 => Ops.nop
  | 0x33 => Ops.nop
  | 0x34 => Ops.nop
  | 0x35 => Ops.nop
  | 0x36 => Ops.nop
  | 0x37 => Ops.nop
  | 0x38 => Ops.nop
  | 0x39 => Ops.nop
  | 0x3A => Ops.nop
  | 0x3B => Ops.nop
  | 0x3C => Ops.nop
  | 0x3D => Ops.nop
  | 0x3E => Ops.nop
  | 0x3F => Ops.nop
  | 0x40 => Ops.nop
  | 0x41 => Ops.nop
  | 0x42 => Ops.nop
  | 0x43 => Ops.nop
  | 0x44 => Ops.nop
  | 0x45 => Ops.nop
  | 0x46 => Ops.nop
  | 0x47 => Ops.nop
  | 0x48 => Ops.nop
  | 0x49 => Ops.nop
  | 0x4A => Ops.nop
  | 0x4B => Ops.nop
  | 0x4C => Ops.nop
  | 0x4D => Ops.nop
  | 0x4E => Ops.nop
  | 0x4F => Ops.nop
  | 0x50 => Ops.nop
  | 0x51 => Ops.nop
  | 0x52 => Ops.nop
  | 0x53 => Ops.nop
  | 0x54 => Ops.nop
  | 0x55 => Ops.nop
  | 0x56 => Ops.nop
  | 0x57 => Ops.nop
  | 0x58 => Ops.nop
  | 0x59 => Ops.nop
  | 0x5A => Ops.nop
  | 0x5B => Ops.nop
  | 0x5C => Ops.nop
  | 0x5D => Ops.nop
  | 0x5E => Ops.nop
  | 0x5F => Ops.nop
  | 0x60 => Ops.nop
  | 0x61 => Ops.nop
  | 0x62 => Ops.nop
  | 0x63 => Ops.nop
  | 0x64 => Ops.nop
  | 0x65 => Ops.nop
  | 0x66 => Ops.nop
  | 0x67 => Ops.nop
  | 0x68 => Ops.nop
  | 0x69 => Ops.nop
  | 0x6A => Ops.nop
  | 0x6B => Ops.nop
  | 0x6C => Ops.nop
  | 0x6D => Ops.nop
  | 0x6E => Ops.nop
  | 0x6F => Ops.nop
  | 0x70 => Ops.nop
  | 0x71 => Ops.nop
  | 0x72 => Ops.nop
  | 0x73 => Ops.nop
  | 0x74 => Ops.nop
  | 0x75 => Ops.nop
  | 0x76 => Ops.nop
  | 0x77 => Ops.nop
  | 0x78 => Ops.nop
  | 0x79 => Ops.nop
  | 0x7A => Ops.nop
  | 0x7B => Ops.nop
  | 0x7C => Ops.nop
  | 0x7D => Ops.nop
  | 0x7E => Ops.nop
  | 0x7F => Ops.nop
  | 0x80 => Ops.nop
  | 0x81 => Ops.nop
  | 0x82 => Ops.nop
  | 0x83 => Ops.nop
  | 0x84 => Ops.nop
  | 0x85 => Ops.nop
  | 0x86 => Ops.nop
  | 0x87 => Ops.nop
  | 0x88 => Ops.nop
  | 0x89 => Ops.nop
  | 0x8A => Ops.nop
  | 0x8B => Ops.nop
  | 0x8C => Ops.nop
  | 0x8D => Ops.nop
  | 0x8E => Ops.nop
  | 0x8F => Ops.nop
  | 0x90 => Ops.nop
  | 0x91 => Ops.nop
  | 0x92 => Ops.nop
  | 0x93 => Ops.nop
  | 0x94 => Ops.nop
  | 0x95 => Ops.nop
  | 0x96 => Ops.nop
  | 0x97 => Ops.nop
  | 0x98 => Ops.nop
  | 0x99 => Ops.nop
  | 0x9A => Ops.nop
  | 0x9B => Ops.nop
  | 0x9C => Ops.nop
  | 0x9D => Ops.nop
  | 0x9E => Ops.nop
  | 0x9F => Ops.nop
  | 0xA0 => Ops.nop
  | 0xA1 => Ops.nop
  | 0xA2 => Ops.nop
  | 0xA3 => Ops.nop
  | 0xA4 => Ops.nop
  | 0xA5 => Ops.nop
  | 0xA6 => Ops.nop
  | 0xA7 => Ops.nop
  | 0xA8 => Ops.nop
  | 0xA9 => Ops.nop
  | 0xAA => Ops.nop
  | 0xAB => Ops.nop
  | 0xAC => Ops.nop
  | 0xAD => Ops.nop
  | 0xAE => Ops.nop
  | 0xAF => Ops.nop
  | 0xB0 => Ops.nop
  | 0xB1 => Ops.nop
  | 0xB2 => Ops.nop
  | 0xB3 => Ops.nop
  | 0xB4 => Ops.nop
  | 0xB5 => Ops.nop
  | 0xB6 => Ops.nop
  | 0xB7 => Ops.nop
  | 0xB8 => Ops.nop
  | 0xB9 => Ops.nop
  | 0xBA => Ops.nop
  | 0xBB => Ops.nop
  | 0xBC => Ops.nop
  | 0xBD => Ops.nop
  | 0xBE => Ops.nop
  | 0xBF => Ops.nop
  | 0xC0 => Ops.nop
  | 0xC1 => Ops.nop
  | 0xC2 => Ops.nop
  | 0xC3 => Ops.nop
  | 0xC4 => Ops.nop
  | 0xC5 => Ops.nop
  | 0xC6 => Ops.nop
  | 0xC7 => Ops.nop
  | 0xC8 => Ops.nop
  | 0xC9 => Ops.nop
  | 0xCA => Ops.nop
  | 0xCB => Ops.nop
  | 0xCC => Ops.nop
  | 0xCD => Ops.nop
  | 0xCE => Ops.nop
  | 0xCF => Ops.nop
  | 0xD0 => Ops.nop
  | 0xD1 => Ops.nop
  | 0xD2 => Ops.nop
  | 0xD3 => Ops.nop
  | 0xD4 => Ops.nop
  | 0xD5 => Ops.nop
  | 0xD6 => Ops.nop
  | 0xD7 => Ops.nop
  | 0xD8 => Ops.nop
  | 0xD9 => Ops.nop
  | 0xDA => Ops.nop
  | 0xDB => Ops.nop
  | 0xDC => Ops.nop
  | 0xDD => Ops.nop
  | 0xDE => Ops.nop
  | 0xDF => Ops.nop
  | 0xE0 => Ops.nop
  | 0xE1 => Ops.nop
  | 0xE2 => Ops.nop
  | 0xE3 => Ops.nop
  | 0xE4 => Ops.nop
  | 0xE5 => Ops.nop
  | 0xE6 => Ops.nop
  | 0xE7 => Ops.nop
  | 0xE8 => Ops.nop
  | 0xE9 => Ops.nop
  | 0xEA => Ops.nop
  | 0xEB => Ops.nop
  | 0xEC => Ops.nop
  | 0xED => Ops.nop
  | 0xEE => Ops.nop
  | 0xEF => Ops.nop
  | 0xF0 => Ops.nop
  | 0xF1 => Ops.nop
  | 0xF2 => Ops.nop
  | 0xF3 => Ops.nop
  | 0xF4 => Ops.nop
  | 0xF5 => Ops.nop
  | 0xF6 => Ops.nop
  | 0xF7 => Ops.nop
  | 0xF8 => Ops.nop
  | 0xF9 => Ops.nop
  | 0xFA => Ops.nop
  | 0xFB => Ops.nop
  | 0xFC => Ops.nop
  | 0xFD => Ops.nop
  | 0xFE => Ops.nop
  | 0xFF => Ops.nop
  | _ =>
    raise(
      UnhandledInstruction(
        Printf.sprintf("Unhandled instruction, %#2x", instruction),
      ),
    )
  };
