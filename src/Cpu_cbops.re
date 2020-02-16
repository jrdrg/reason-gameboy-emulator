open Cpu;
open Cpu_types;

module Swap = {
  let swap = (cpu, value) => {
    let nibble1 = (value land 0xf0) lsr 4;
    let nibble2 = (value land 0x0f) lsl 4;
    let swapped = nibble1 + nibble2;
    (
      swapped,
      cpu
      |> setFlag(Z, swapped === 0 |> b2i)
      |> setFlag(N, 0)
      |> setFlag(H, 0)
      |> setFlag(C, 0),
    );
  };

  let swap_a: op =
    ({cpu} as s) => {
      let (a, cpu) = swap(cpu, cpu.registers.a);
      newState(~cpu=cpu |> wA(a) |> machineCycles(2), s);
    };
  let swap_b: op =
    ({cpu} as s) => {
      let (b, cpu) = swap(cpu, cpu.registers.b);
      newState(~cpu=cpu |> wB(b) |> machineCycles(2), s);
    };
  let swap_c: op =
    ({cpu} as s) => {
      let (c, cpu) = swap(cpu, cpu.registers.c);
      newState(~cpu=cpu |> wC(c) |> machineCycles(2), s);
    };
  let swap_d: op =
    ({cpu} as s) => {
      let (d, cpu) = swap(cpu, cpu.registers.d);
      newState(~cpu=cpu |> wD(d) |> machineCycles(2), s);
    };
};

module Bit = {
  let bit_ = (cpu, s, ~cycles=2, isSet: bool) => {
    newState(
      ~cpu=
        cpu
        |> setFlag(Z, b2i(isSet))
        |> setFlag(N, 0)
        |> setFlag(H, 1)
        |> machineCycles(cycles),
      s,
    );
  };

  module type BitOffset = {let offset: int;};

  module BitOps = (B: BitOffset) => {
    let bit_a = ({cpu} as s) => {
      bit_(cpu, s, cpu.registers.a land B.offset === 0);
    };
    let bit_b = ({cpu} as s) => {
      bit_(cpu, s, cpu.registers.b land B.offset === 0);
    };
    let bit_c = ({cpu} as s) => {
      bit_(cpu, s, cpu.registers.c land B.offset === 0);
    };
    let bit_d = ({cpu} as s) => {
      bit_(cpu, s, cpu.registers.d land B.offset === 0);
    };
    let bit_e = ({cpu} as s) => {
      bit_(cpu, s, cpu.registers.e land B.offset === 0);
    };
    let bit_h = ({cpu} as s) => {
      bit_(cpu, s, cpu.registers.h land B.offset === 0);
    };
    let bit_l = ({cpu} as s) => {
      bit_(cpu, s, cpu.registers.l land B.offset === 0);
    };
    let bit_m_hl = ({cpu, mmu, gpu} as s) => {
      let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
      newState(~mmu, bit_(~cycles=4, cpu, s, hl land B.offset === 0));
    };
  };
  // RLA:   function() { var ci=Z80._r.f&0x10?1:0; var co=Z80._r.a&0x80?0x10:0; Z80._r.a=(Z80._r.a<<1)+ci; Z80._r.a&=255;                             Z80._r.f=(Z80._r.f&0xEF)+co; Z80._r.m=1; },
  // RLr_a: function() { var ci=Z80._r.f&0x10?1:0; var co=Z80._r.a&0x80?0x10:0; Z80._r.a=(Z80._r.a<<1)+ci; Z80._r.a&=255; Z80._r.f=(Z80._r.a)?0:0x80; Z80._r.f=(Z80._r.f&0xEF)+co; Z80._r.m=2; },

  module Bit0 =
    BitOps({
      let offset = 0x01;
    });
  module Bit1 =
    BitOps({
      let offset = 0x02;
    });
  module Bit2 =
    BitOps({
      let offset = 0x04;
    });
  module Bit3 =
    BitOps({
      let offset = 0x08;
    });
  module Bit4 =
    BitOps({
      let offset = 0x10;
    });
  module Bit5 =
    BitOps({
      let offset = 0x20;
    });
  module Bit6 =
    BitOps({
      let offset = 0x40;
    });
  module Bit7 =
    BitOps({
      let offset = 0x80;
    });
};

module Rl = {
  let rl = (cpu, ~mCycles=2, register) => {
    let highBit = register land 0b10000000 > 0 ? 1 : 0;
    let carry = cpu |> getFlag(C) > 0 ? 1 : 0;
    let r = (register lsl 1 + carry) land 255;
    (
      r,
      cpu
      |> machineCycles(mCycles)
      |> setFlag(Flags.C, highBit)
      |> setFlag(Flags.Z, b2i(r === 0)),
    );
  };

  let rl_a: op =
    ({cpu} as s) => {
      let (a, cpu) = rl(cpu, cpu.registers.a);
      newState(~cpu=cpu |> wA(a), s);
    };
  let rl_b: op =
    ({cpu} as s) => {
      let (b, cpu) = rl(cpu, cpu.registers.b);
      newState(~cpu=cpu |> wB(b), s);
    };
  let rl_c: op =
    ({cpu} as s) => {
      let (c, cpu) = rl(cpu, cpu.registers.c);
      newState(~cpu=cpu |> wC(c), s);
    };
  let rl_d: op =
    ({cpu} as s) => {
      let (d, cpu) = rl(cpu, cpu.registers.d);
      newState(~cpu=cpu |> wD(d), s);
    };
  let rl_e: op =
    ({cpu} as s) => {
      let (e, cpu) = rl(cpu, cpu.registers.e);
      newState(~cpu=cpu |> wE(e), s);
    };
  let rl_h: op =
    ({cpu} as s) => {
      let (h, cpu) = rl(cpu, cpu.registers.h);
      newState(~cpu=cpu |> wH(h), s);
    };
  let rl_l: op =
    ({cpu} as s) => {
      let (l, cpu) = rl(cpu, cpu.registers.l);
      newState(~cpu=cpu |> wL(l), s);
    };
  let rl_m_hl: op =
    ({cpu, mmu, gpu} as s) => {
      let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
      let (hl, cpu) = rl(cpu, ~mCycles=4, hl);
      let (mmu, gpu) = Mmu.write8(rHl(cpu), hl, {mmu, gpu});
      newState(~cpu, ~mmu, ~gpu, s);
    };
};

module Res = {
  module type ResOffset = {let offset: int;};

  module ResOps = (R: ResOffset) => {
    let res_a: op =
      ({cpu} as s) => {
        let a = cpu.registers.a land R.offset;
        newState(~cpu=cpu |> wA(a) |> machineCycles(2), s);
      };
    let res_b: op =
      ({cpu} as s) => {
        let b = cpu.registers.b land R.offset;
        newState(~cpu=cpu |> wB(b) |> machineCycles(2), s);
      };
    let res_c: op =
      ({cpu} as s) => {
        let c = cpu.registers.c land R.offset;
        newState(~cpu=cpu |> wC(c) |> machineCycles(2), s);
      };
    let res_d: op =
      ({cpu} as s) => {
        let d = cpu.registers.d land R.offset;
        newState(~cpu=cpu |> wD(d) |> machineCycles(2), s);
      };
    let res_e: op =
      ({cpu} as s) => {
        let e = cpu.registers.e land R.offset;
        newState(~cpu=cpu |> wE(e) |> machineCycles(2), s);
      };
    let res_h: op =
      ({cpu} as s) => {
        let h = cpu.registers.h land R.offset;
        newState(~cpu=cpu |> wH(h) |> machineCycles(2), s);
      };
    let res_l: op =
      ({cpu} as s) => {
        let l = cpu.registers.l land R.offset;
        newState(~cpu=cpu |> wL(l) |> machineCycles(2), s);
      };
    // let res_m_hl: op =
    //   ({cpu, mmu, gpu} as s) => {
    //     let hl = Mmu.read8;
    //     let l = cpu.registers.l land R.offset;
    //     newState(~cpu=cpu |> wL(l) |> machineCycles(2), s);
    //   };
  };
};