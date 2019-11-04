/* http://gameboy.mongenel.com/dmg/opcodes.html */
/* http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf */
module Flags = {
  let (z, n, h, c) = (7, 6, 5, 4);
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
  mutable a: int,
  mutable b: int,
  mutable c: int,
  mutable d: int,
  mutable e: int,
  mutable h: int,
  mutable l: int,
  mutable f: int,
  mutable sp: int,
  mutable pc: int,
  mutable mCycles: int,
};

type t = {
  mutable clock: int,
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

/**
 * Increment Program Counter
 */
let incrementPc = (cycles, cpu) => {
  cpu.registers.pc = cpu.registers.pc + cycles;
  cpu;
};

let incrementSp = (t: registers) => {
  t.sp = t.sp + 1;
  t;
};

let decrementSp = (t: registers) => {
  t.sp = t.sp - 1;
  t;
};

/**
 * Returns a byte with only the appropriate flag value set (or not)
 */
let getFlag = (flag, cpu) => {
  let bit = 1 lsl Flags.flagOffset(flag);
  cpu.registers.f land bit;
};

let setFlag = (flag, value, ~initialValue=?, cpu) => {
  open Belt;
  cpu.registers.f =
    Flags.setFlag(
      flag,
      value,
      Option.getWithDefault(initialValue, cpu.registers.f),
    );
  cpu;
};

let setRegisters = (~a=?, ~b=?, ~c=?, ~d=?, ~e=?, ~h=?, ~l=?, cpu) => {
  open Belt;
  let {registers} = cpu;
  registers.a = Option.getWithDefault(a, registers.a);
  registers.b = Option.getWithDefault(b, registers.b);
  registers.c = Option.getWithDefault(c, registers.c);
  registers.d = Option.getWithDefault(d, registers.d);
  registers.e = Option.getWithDefault(e, registers.e);
  registers.h = Option.getWithDefault(h, registers.h);
  registers.l = Option.getWithDefault(l, registers.l);
  cpu;
};

let machineCycles = (cycles: int, cpu: t) => {
  cpu.registers.mCycles = cycles;
  cpu;
};

let cycles = (cycles: int, cpu: t) =>
  /* 1 cycle = 4 machine cycles */
  cpu |> machineCycles(cycles / 4);

module Ops = {
  type state = {
    cpu: t,
    mmu: Mmu.t,
    gpu: Gpu.t,
  };
  type op = state => state;
  let newState = (~cpu=?, ~mmu=?, ~gpu=?, old: state) =>
    Belt.{
      cpu: Option.getWithDefault(cpu, old.cpu),
      mmu: Option.getWithDefault(mmu, old.mmu),
      gpu: Option.getWithDefault(gpu, old.gpu),
    };
  let nop: op = (s: state) => s |> newState(~cpu=machineCycles(1, s.cpu));
  /* 01 */
  let ld_bc_nn: op =
    ({mmu, gpu, cpu} as s) => {
      let pc = programCount(cpu);
      let (c, m) = Mmu.read8(pc, {gpu, mmu});
      let (b, m) = Mmu.read8(pc + 1, {gpu, mmu: m});
      s
      |> newState(
           ~cpu=
             cpu
             |> setRegisters(~b, ~c)
             |> machineCycles(3)
             |> incrementPc(2),
           ~mmu=m,
         );
    };
  /* 02 */
  let ld_m_bc_a: op =
    ({mmu, gpu, cpu} as s) => {
      let {a} = cpu.registers;
      let (mmuWrite, _gpu) = Mmu.write8(rBc(cpu), a, {mmu, gpu});
      s |> newState(~cpu=cpu |> machineCycles(2), ~mmu=mmuWrite);
    };
  /* 03 */
  let inc_bc: op =
    ({cpu} as s) => {
      let c = (cpu.registers.c + 1) land 0xff; /* wrap after 255, i.e. 256 = 0 */
      let b =
        if (c === 0) {
          cpu.registers.b + 1;
        } else {
          cpu.registers.b;
        };
      s |> newState(~cpu=cpu |> setRegisters(~b, ~c) |> machineCycles(2));
    };
  /* 04 */
  let inc_b: op =
    ({cpu} as s) => {
      let b = (cpu.registers.b + 1) land 0xff;
      let f = cpu.registers.f land 0x10;
      s
      |> newState(
           ~cpu=
             cpu
             |> setRegisters(~b)
             |> setFlag(Z, b2i(b === 0), ~initialValue=f)
             |> machineCycles(1),
         );
    };
  /* 05 */
  let dec_b: op =
    ({cpu} as s) => {
      let b = (cpu.registers.b - 1) land 0xff;
      let f = cpu.registers.f land 0x10;
      s
      |> newState(
           ~cpu=
             cpu
             |> setRegisters(~b)
             |> setFlag(Z, b2i(b === 0), ~initialValue=f)
             |> setFlag(N, 1)
             |> machineCycles(2),
         );
    };
  /* 06: LD B,d8 */
  let ld_b_n: op =
    ({mmu, gpu, cpu} as s) => {
      let pc = programCount(cpu);
      let (b, m) = Mmu.read8(pc, {gpu, mmu});
      s
      |> newState(
           ~cpu=
             cpu |> setRegisters(~b) |> machineCycles(2) |> incrementPc(1),
           ~mmu=m,
         );
    };
  /* 07: RLCA */
  let rlca: op =
    ({cpu} as s) => {
      let {a} = cpu.registers;
      let highBit = a land 0b10000000 > 0 ? 1 : 0;
      let a' = (a lsl 1 + highBit) land 255;
      s
      |> newState(
           ~cpu=
             cpu
             |> machineCycles(1)
             |> setRegisters(~a=a')
             |> setFlag(Flags.C, highBit),
         );
    };
  /* 08: LD (a16),SP */
  let ld_m_nn_sp: op =
    ({mmu, cpu, gpu} as s) => {
      let pc = programCount(cpu);
      let (addr, mmu) = Mmu.read16(pc, {mmu, gpu});
      let (mmu, gpu) = Mmu.write8(addr, cpu.registers.sp, {mmu, gpu});
      s |> newState(~cpu=cpu |> cycles(20) |> incrementPc(2), ~mmu, ~gpu);
    };
  /* 09: ADD HL,BC */
  let add_hl_bc: op =
    ({cpu} as s) => {
      let hl = rHl(cpu) + rBc(cpu);
      let h = hl lsr 8 land 255;
      let l = hl land 255;
      let carry = hl > 0xffff;
      s
      |> newState(
           ~cpu=
             cpu
             |> setRegisters(~h, ~l)
             |> setFlag(Flags.C, b2i(carry))
             |> machineCycles(2),
         );
    };
  /* 0A: LD A,(BC) */
  let ld_a_m_bc: op =
    ({cpu} as s) => {
      let a = rBc(cpu);
      s |> newState(~cpu=cpu |> setRegisters(~a) |> machineCycles(2));
    };
  /* 0F */
  let rrca: op =
    ({cpu} as s) => {
      s |> newState(~cpu=cpu |> machineCycles(1));
    };
  /* 17 */
  let rla: op =
    ({cpu} as s) => {
      s |> newState(~cpu=cpu |> machineCycles(1));
    };
  /* 1F */
  let rra: op =
    ({cpu} as s) => {
      s |> newState(~cpu=cpu |> machineCycles(1));
    };
};

let exec: int => Ops.op =
  instruction =>
    switch (instruction) {
    | 0x00 => Ops.nop
    | 0x01 => Ops.ld_bc_nn
    | 0x02 => Ops.ld_m_bc_a
    | 0x03 => Ops.inc_bc
    | 0x04 => Ops.inc_b
    | 0x05 => Ops.dec_b
    | 0x06 => Ops.ld_b_n
    | 0x07 => Ops.rlca
    | 0x08 => Ops.ld_m_nn_sp
    | 0x09 => Ops.add_hl_bc
    | 0x0A => Ops.ld_a_m_bc
    | 0x0B => Ops.nop
    | 0x0C => Ops.nop
    | 0x0D => Ops.nop
    | 0x0E => Ops.nop
    | 0x0F => Ops.rrca
    | 0x10 => Ops.nop
    | 0x11 => Ops.nop
    | 0x12 => Ops.nop
    | 0x13 => Ops.nop
    | 0x14 => Ops.nop
    | 0x15 => Ops.nop
    | 0x16 => Ops.nop
    | 0x17 => Ops.rla
    | 0x18 => Ops.nop
    | 0x19 => Ops.nop
    | 0x1A => Ops.nop
    | 0x1B => Ops.nop
    | 0x1C => Ops.nop
    | 0x1D => Ops.nop
    | 0x1E => Ops.nop
    | 0x1F => Ops.rra
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
