exception UnhandledInstruction(string);
exception AssertionException(string);

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
  let getFlag = (flag, flags) => {
    let offset = flagOffset(flag);
    let flag = flags land 1 lsl offset;
    flag lsr offset;
  };
  let isSet = (flag, flags) => {
    let offset = flagOffset(flag);
    flags land 1 lsl offset > 0;
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
  mutable halt: int,
  registers,
};

let make = () => {
  clock: 0,
  halt: 0,
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

let readAndIncrementPc = cpu => {
  let {pc} = cpu.registers;
  cpu.registers.pc = cpu.registers.pc + 1;
  (pc, cpu);
};

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

let checkRegister = register =>
  if (register > 0xff) {
    raise(AssertionException(string_of_int(register)));
  };

let setRegisters = (~a=?, ~b=?, ~c=?, ~d=?, ~e=?, ~h=?, ~l=?, ~f=?, cpu) => {
  open Belt;
  let {registers} = cpu;
  registers.a = Option.getWithDefault(a, registers.a);
  registers.b = Option.getWithDefault(b, registers.b);
  registers.c = Option.getWithDefault(c, registers.c);
  registers.d = Option.getWithDefault(d, registers.d);
  registers.e = Option.getWithDefault(e, registers.e);
  registers.h = Option.getWithDefault(h, registers.h);
  registers.l = Option.getWithDefault(l, registers.l);
  registers.f = Option.getWithDefault(f, registers.f);

  checkRegister(registers.a);
  checkRegister(registers.b);
  checkRegister(registers.c);
  checkRegister(registers.d);
  checkRegister(registers.e);
  checkRegister(registers.h);
  checkRegister(registers.l);

  cpu;
};

let writeRegister16 = (register: register16, value: int, cpu: t) => {
  if (value > 0xffff) {
    raise(AssertionException(Printf.sprintf("writeRegister16 %d", value)));
  };

  let r1 = value lsr 8 land 0xff;
  let r2 = value land 0xff;
  switch (register) {
  | AF =>
    let (a, f) = (r1, r2);
    cpu |> setRegisters(~a, ~f);
  | BC =>
    let (b, c) = (r1, r2);
    cpu |> setRegisters(~b, ~c);
  | DE =>
    let (d, e) = (r1, r2);
    cpu |> setRegisters(~d, ~e);
  | HL =>
    let (h, l) = (r1, r2);
    cpu |> setRegisters(~h, ~l);
  };
};

let wHl = writeRegister16(HL);

/**
 * Increment Program Counter
 */
let incrementPc = (cycles, cpu) => {
  cpu.registers.pc = cpu.registers.pc + cycles;
  cpu;
};

let setPc = (pc, cpu) => {
  cpu.registers.pc = pc;
  cpu;
};

let incrementSp = (~amount=1, cpu: t) => {
  cpu.registers.sp = (cpu.registers.sp + amount) land 0xffff;
  cpu;
};

let decrementSp = (~amount=1, cpu: t) => {
  cpu.registers.sp = (cpu.registers.sp - amount) land 0xffff;
  cpu;
};

let setSp = (sp: int, cpu: t) => {
  cpu.registers.sp = sp land 0xffff;
  cpu;
};

/**j
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

let toggleFlag = (flag, cpu) => {
  let flagValue =
    if (Flags.getFlag(flag, cpu.registers.f) === 0) {
      1;
    } else {
      0;
    };
  let f = Flags.setFlag(flag, flagValue, cpu.registers.f);
  cpu |> setRegisters(~f);
};

let machineCycles = (cycles: int, cpu: t) => {
  cpu.registers.mCycles = cycles;
  cpu;
};

let cycles = (cycles: int, cpu: t) =>
  /* 1 cycle = 4 machine cycles */
  cpu |> machineCycles(cycles / 4);

let incrementHl = (cpu: t): t => {
  let l = (cpu.registers.l + 1) land 0xff; /* wrap after 255, i.e. 256 = 0 */
  let h =
    if (l === 0) {
      (cpu.registers.h + 1) land 0xff;
    } else {
      cpu.registers.h;
    };
  cpu |> setRegisters(~h, ~l);
};

let decrementHl = (cpu: t): t => {
  let l = (cpu.registers.l - 1) land 0xff; /* wrap after 255, i.e. 256 = 0 */
  let h =
    if (l === 255) {
      (cpu.registers.h - 1) land 0xff;
    } else {
      cpu.registers.h;
    };
  cpu |> setRegisters(~h, ~l);
};

let signed = (value: int): int =>
  value > 127 ? - ((lnot(value) + 1) land 0xff) : value;
