open Cpu_types;

/* http://gameboy.mongenel.com/dmg/opcodes.html */
/* http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf */

module Ops = {
  open Cpu;

  let nop: op =
    (s: state) => {
      newState(~cpu=machineCycles(1, s.cpu), s);
    };

  let halt: op =
    ({cpu} as s: state) => {
      cpu.halt = 1;
      newState(~cpu=machineCycles(1, cpu), s);
    };

  module Interrupts = {
    let di: op =
      ({cpu} as s: state) => {
        cpu.ime = 0;
        newState(~cpu=machineCycles(1, cpu), s);
      };

    let ei: op =
      ({cpu} as s: state) => {
        cpu.ime = 1;
        newState(~cpu=machineCycles(1, cpu), s);
      };
  };

  module Load_nn_8 = {
    let load_nn_8 = (register, {cpu, mmu, gpu} as s) => {
      let (pc, cpu) = readAndIncrementPc(cpu);
      let (value, m) = Mmu.read8(pc, {gpu, mmu});
      let cpu =
        switch (register) {
        | A => raise(AssertionException("No ld_a_n instruction"))
        | F => raise(AssertionException("No ld_f_n instruction"))
        | B
        | C
        | D
        | E
        | H
        | L => cpu |> writeRegister8(register, value) |> machineCycles(2)
        };
      newState(~cpu, ~mmu=m, s);
    };
    /* 06: LD B,d8 */
    let ld_b_n: op = load_nn_8(B);
    let ld_c_n: op = load_nn_8(C);
    let ld_d_n: op = load_nn_8(D);
    let ld_e_n: op = load_nn_8(E);
    let ld_h_n: op = load_nn_8(H);
    let ld_l_n: op = load_nn_8(L);
  };

  module Load_r1_r2 = {
    let ld_b_b: op =
      ({cpu} as s) => {
        let {b} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~b), s);
      };
    let ld_b_c: op =
      ({cpu} as s) => {
        let {c} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~b=c), s);
      };
    let ld_b_d: op =
      ({cpu} as s) => {
        let {d} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~b=d), s);
      };
    let ld_b_e: op =
      ({cpu} as s) => {
        let {e} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~b=e), s);
      };
    let ld_b_h: op =
      ({cpu} as s) => {
        let {h} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~b=h), s);
      };
    let ld_b_l: op =
      ({cpu} as s) => {
        let {l} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~b=l), s);
      };
    let ld_b_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> setRegisters(~b=hl),
          ~mmu,
          s,
        );
      };
    let ld_c_b: op =
      ({cpu} as s) => {
        let {b} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~c=b), s);
      };
    let ld_c_c: op =
      ({cpu} as s) => {
        let {c} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~c), s);
      };
    let ld_c_d: op =
      ({cpu} as s) => {
        let {d} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~c=d), s);
      };
    let ld_c_e: op =
      ({cpu} as s) => {
        let {e} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~c=e), s);
      };
    let ld_c_h: op =
      ({cpu} as s) => {
        let {h} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~c=h), s);
      };
    let ld_c_l: op =
      ({cpu} as s) => {
        let {l} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~c=l), s);
      };
    let ld_c_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> setRegisters(~c=hl),
          ~mmu,
          s,
        );
      };
    let ld_d_b: op =
      ({cpu} as s) => {
        let {b} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~d=b), s);
      };
    let ld_d_c: op =
      ({cpu} as s) => {
        let {c} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~d=c), s);
      };
    let ld_d_d: op =
      ({cpu} as s) => {
        let {d} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~d), s);
      };
    let ld_d_e: op =
      ({cpu} as s) => {
        let {e} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~d=e), s);
      };
    let ld_d_h: op =
      ({cpu} as s) => {
        let {h} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~d=h), s);
      };
    let ld_d_l: op =
      ({cpu} as s) => {
        let {l} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~d=l), s);
      };
    let ld_d_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> setRegisters(~d=hl),
          ~mmu,
          s,
        );
      };
    let ld_e_b: op =
      ({cpu} as s) => {
        let {b} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~e=b), s);
      };
    let ld_e_c: op =
      ({cpu} as s) => {
        let {c} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~e=c), s);
      };
    let ld_e_d: op =
      ({cpu} as s) => {
        let {d} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~e=d), s);
      };
    let ld_e_e: op =
      ({cpu} as s) => {
        let {e} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~e), s);
      };
    let ld_e_h: op =
      ({cpu} as s) => {
        let {h} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~e=h), s);
      };
    let ld_e_l: op =
      ({cpu} as s) => {
        let {l} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~e=l), s);
      };
    let ld_e_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> setRegisters(~e=hl),
          ~mmu,
          s,
        );
      };
    let ld_h_b: op =
      ({cpu} as s) => {
        let {b} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~h=b), s);
      };
    let ld_h_c: op =
      ({cpu} as s) => {
        let {c} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~h=c), s);
      };
    let ld_h_d: op =
      ({cpu} as s) => {
        let {d} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~h=d), s);
      };
    let ld_h_e: op =
      ({cpu} as s) => {
        let {e} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~h=e), s);
      };
    let ld_h_h: op =
      ({cpu} as s) => {
        let {h} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~h), s);
      };
    let ld_h_l: op =
      ({cpu} as s) => {
        let {l} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~h=l), s);
      };
    let ld_h_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> setRegisters(~h=hl),
          ~mmu,
          s,
        );
      };
    let ld_l_b: op =
      ({cpu} as s) => {
        let {b} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~l=b), s);
      };
    let ld_l_c: op =
      ({cpu} as s) => {
        let {c} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~l=c), s);
      };
    let ld_l_d: op =
      ({cpu} as s) => {
        let {d} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~l=d), s);
      };
    let ld_l_e: op =
      ({cpu} as s) => {
        let {e} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~l=e), s);
      };
    let ld_l_h: op =
      ({cpu} as s) => {
        let {h} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~l=h), s);
      };
    let ld_l_l: op =
      ({cpu} as s) => {
        let {l} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~l), s);
      };
    let ld_l_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> setRegisters(~l=hl),
          ~mmu,
          s,
        );
      };
    let ld_m_hl_8 = (register: int, {cpu, mmu, gpu}) => {
      let (mmu, gpu) = Mmu.write8(rHl(cpu), register, {mmu, gpu});
      (cpu |> machineCycles(2), mmu, gpu);
    };
    let ld_m_hl_b: op =
      ({cpu} as s) => {
        let (cpu, mmu, gpu) = ld_m_hl_8(cpu.registers.b, s);
        newState(~cpu, ~mmu, ~gpu, s);
      };
    let ld_m_hl_c: op =
      ({cpu} as s) => {
        let (cpu, mmu, gpu) = ld_m_hl_8(cpu.registers.c, s);
        newState(~cpu, ~mmu, ~gpu, s);
      };
    let ld_m_hl_d: op =
      ({cpu} as s) => {
        let (cpu, mmu, gpu) = ld_m_hl_8(cpu.registers.d, s);
        newState(~cpu, ~mmu, ~gpu, s);
      };
    let ld_m_hl_e: op =
      ({cpu} as s) => {
        let (cpu, mmu, gpu) = ld_m_hl_8(cpu.registers.e, s);
        newState(~cpu, ~mmu, ~gpu, s);
      };
    let ld_m_hl_h: op =
      ({cpu} as s) => {
        let (cpu, mmu, gpu) = ld_m_hl_8(cpu.registers.h, s);
        newState(~cpu, ~mmu, ~gpu, s);
      };
    let ld_m_hl_l: op =
      ({cpu} as s) => {
        let (cpu, mmu, gpu) = ld_m_hl_8(cpu.registers.l, s);
        newState(~cpu, ~mmu, ~gpu, s);
      };
    let ld_m_hl_n: op =
      ({cpu, mmu, gpu} as s) => {
        let (n, mmu) = Mmu.read8(programCount(cpu), {mmu, gpu});
        let (mmu, gpu) = Mmu.write8(rHl(cpu), n, {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(3) |> incrementPc(1),
          ~mmu,
          ~gpu,
          s,
        );
      };
  };

  module Load_nn_16 = {
    let ld_n_nn = ({cpu, gpu, mmu}) => {
      let pc = programCount(cpu);
      let (b2, m) = Mmu.read8(pc, {gpu, mmu});
      let (b1, m) = Mmu.read8(pc + 1, {gpu, mmu: m});

      (b1, b2, m, cpu |> machineCycles(3) |> incrementPc(2));
    };
    /* 01 */
    let ld_bc_nn: op =
      s => {
        let (b, c, m, cpu) = ld_n_nn(s);
        newState(~cpu=cpu |> setRegisters(~b, ~c), ~mmu=m, s);
      };
    let ld_de_nn: op =
      s => {
        let (d, e, m, cpu) = ld_n_nn(s);
        newState(~cpu=cpu |> setRegisters(~d, ~e), ~mmu=m, s);
      };
    let ld_hl_nn: op =
      s => {
        let (h, l, m, cpu) = ld_n_nn(s);
        Printf.printf("LD HL nn: %x %x\n", h, l);
        newState(~cpu=cpu |> setRegisters(~h, ~l), ~mmu=m, s);
      };
    let ld_sp_nn: op =
      ({cpu, mmu, gpu} as s) => {
        let (sp, mmu) = Mmu.read16(programCount(cpu), {gpu, mmu});
        Printf.printf("LD SP nn: %x\n", sp);
        newState(
          ~cpu=cpu |> setSp(sp) |> machineCycles(3) |> incrementPc(2),
          ~mmu,
          s,
        );
      };
    let ld_sp_hl: op =
      ({cpu} as s) => {
        let hl = rHl(cpu);
        newState(~cpu=cpu |> setSp(hl) |> machineCycles(2), s);
      };
    let ld_hl_sp_n: op =
      ({cpu, mmu, gpu} as s) => {
        // TODO: set flags H and C
        let (n, mmu) = Mmu.read8(programCount(cpu), {mmu, gpu});
        let n = signed(n) + cpu.registers.sp;
        newState(
          ~cpu=
            cpu
            |> wHl(n)
            |> setFlag(Z, 0)
            |> setFlag(N, 0)
            |> machineCycles(3)
            |> incrementPc(1),
          ~mmu,
          s,
        );
      };
    /* 08: LD (a16),SP */
    let ld_m_nn_sp: op =
      ({mmu, cpu, gpu} as s) => {
        let pc = programCount(cpu);
        let (addr, mmu) = Mmu.read16(pc, {mmu, gpu});
        let (mmu, gpu) = Mmu.write16(addr, cpu.registers.sp, {mmu, gpu});
        newState(~cpu=cpu |> cycles(20) |> incrementPc(2), ~mmu, ~gpu, s);
      };
  };

  module Load_8_A = {
    /* 02 */
    let ld_m_bc_a: op =
      ({mmu, gpu, cpu} as s) => {
        let {a} = cpu.registers;
        let (mmuWrite, gpu) = Mmu.write8(rBc(cpu), a, {mmu, gpu});
        newState(~cpu=cpu |> machineCycles(2), ~mmu=mmuWrite, ~gpu, s);
      };
    let ld_m_de_a: op =
      ({mmu, gpu, cpu} as s) => {
        let {a} = cpu.registers;
        let (mmuWrite, gpu) = Mmu.write8(rDe(cpu), a, {mmu, gpu});
        newState(~cpu=cpu |> machineCycles(2), ~mmu=mmuWrite, ~gpu, s);
      };
    let ld_m_hl_a: op =
      ({mmu, gpu, cpu} as s) => {
        let {a} = cpu.registers;
        let (mmuWrite, gpu) = Mmu.write8(rHl(cpu), a, {mmu, gpu});
        newState(~cpu=cpu |> machineCycles(2), ~mmu=mmuWrite, ~gpu, s);
      };
    let ld_m_nn_a: op =
      ({mmu, gpu, cpu} as s) => {
        let {a} = cpu.registers;
        let (addr, mmu) = Mmu.read16(programCount(cpu), {mmu, gpu});
        let (mmuWrite, gpu) = Mmu.write8(addr, a, {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(4) |> incrementPc(2),
          ~mmu=mmuWrite,
          ~gpu,
          s,
        );
      };
    let ld_a_a: op =
      ({cpu} as s) => {
        let {a} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~a), s);
      };
    let ld_b_a: op =
      ({cpu} as s) => {
        let {a} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~b=a), s);
      };
    let ld_c_a: op =
      ({cpu} as s) => {
        let {a} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~c=a), s);
      };
    let ld_d_a: op =
      ({cpu} as s) => {
        let {a} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~d=a), s);
      };
    let ld_e_a: op =
      ({cpu} as s) => {
        let {a} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~e=a), s);
      };
    let ld_h_a: op =
      ({cpu} as s) => {
        let {a} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~h=a), s);
      };
    let ld_l_a: op =
      ({cpu} as s) => {
        let {a} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~l=a), s);
      };
    let ld_m_c_a: op =
      ({cpu, mmu, gpu} as s) => {
        let {a, c} = cpu.registers;
        let addr = 0xff00 + c;
        let (mmu, gpu) = Mmu.write8(addr, a, {mmu, gpu});
        newState(~cpu=cpu |> machineCycles(2), ~mmu, ~gpu, s);
      };
    let ld_hld_a: op =
      ({cpu, mmu, gpu} as s) => {
        let {a} = cpu.registers;
        let (mmu, gpu) = Mmu.write8(rHl(cpu), a, {mmu, gpu});
        newState(~cpu=cpu |> machineCycles(2) |> decrementHl, ~mmu, ~gpu, s);
      };
    let ld_hli_a: op =
      ({cpu, mmu, gpu} as s) => {
        let {a} = cpu.registers;
        let (mmu, gpu) = Mmu.write8(rHl(cpu), a, {mmu, gpu});
        newState(~cpu=cpu |> machineCycles(2) |> incrementHl, ~mmu, ~gpu, s);
      };
    let ldh_n_a: op =
      ({cpu, mmu, gpu} as s) => {
        let (immediate, mmu) = Mmu.read8(programCount(cpu), {mmu, gpu});
        let addr = 0xff00 + immediate;
        let (mmu, gpu) = Mmu.write8(addr, cpu.registers.a, {mmu, gpu});
        newState(
          ~cpu=cpu |> incrementPc(1) |> machineCycles(3),
          ~mmu,
          ~gpu,
          s,
        );
      };
  };

  module Load_A_8 = {
    let ld_a_b: op =
      ({cpu} as s) => {
        let {b} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~a=b), s);
      };
    let ld_a_c: op =
      ({cpu} as s) => {
        let {c} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~a=c), s);
      };
    let ld_a_d: op =
      ({cpu} as s) => {
        let {d} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~a=d), s);
      };
    let ld_a_e: op =
      ({cpu} as s) => {
        let {e} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~a=e), s);
      };
    let ld_a_h: op =
      ({cpu} as s) => {
        let {h} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~a=h), s);
      };
    let ld_a_l: op =
      ({cpu} as s) => {
        let {l} = cpu.registers;
        newState(~cpu=cpu |> machineCycles(1) |> setRegisters(~a=l), s);
      };
    /* 0A: LD A,(BC) */
    let ld_a_m_bc: op =
      ({cpu, mmu, gpu} as s) => {
        let (bc, mmu) = Mmu.read8(rBc(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> setRegisters(~a=bc),
          ~mmu,
          s,
        );
      };
    let ld_a_m_de: op =
      ({cpu, mmu, gpu} as s) => {
        let (de, mmu) = Mmu.read8(rDe(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> setRegisters(~a=de),
          ~mmu,
          s,
        );
      };
    let ld_a_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> setRegisters(~a=hl),
          ~mmu,
          s,
        );
      };
    let ld_a_m_nn: op =
      ({cpu, mmu, gpu} as s) => {
        let (addr, mmu) = Mmu.read16(programCount(cpu), {mmu, gpu});
        let (a, mmu) = Mmu.read8(addr, {mmu, gpu});
        newState(
          ~cpu=
            cpu |> machineCycles(4) |> incrementPc(2) |> setRegisters(~a),
          ~mmu,
          s,
        );
      };
    let ld_a_m_n: op =
      ({cpu, mmu, gpu} as s) => {
        let addr = programCount(cpu);
        let (a, mmu) = Mmu.read8(addr, {mmu, gpu});
        newState(
          ~cpu=
            cpu |> machineCycles(2) |> incrementPc(1) |> setRegisters(~a),
          ~mmu,
          s,
        );
      };
    let ld_a_m_c: op =
      ({cpu, mmu, gpu} as s) => {
        let {c} = cpu.registers;
        let addr = 0xff00 + c;
        let (mc, mmu) = Mmu.read8(addr, {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> setRegisters(~a=mc),
          ~mmu,
          s,
        );
      };
    let ld_a_m_hld: op =
      ({cpu, mmu, gpu} as s) => {
        let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> decrementHl |> setRegisters(~a=hl),
          ~mmu,
          s,
        );
      };
    let ld_a_m_hli: op =
      ({cpu, mmu, gpu} as s) => {
        let (hl, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> machineCycles(2) |> incrementHl |> setRegisters(~a=hl),
          ~mmu,
          s,
        );
      };
    let ldh_a_n: op =
      ({cpu, mmu, gpu} as s) => {
        let (immediate, mmu) = Mmu.read8(programCount(cpu), {mmu, gpu});
        let addr = 0xff00 + immediate;
        let (a, mmu) = Mmu.read8(addr, {mmu, gpu});
        newState(
          ~cpu=
            cpu |> setRegisters(~a) |> incrementPc(1) |> machineCycles(3),
          ~mmu,
          ~gpu,
          s,
        );
      };
  };

  module Increment16 = {
    /* Increment 16-bit value */
    /* 03 */
    let inc_bc: op =
      ({cpu} as s) => {
        let c = (cpu.registers.c + 1) land 0xff; /* wrap after 255, i.e. 256 = 0 */
        let b =
          if (c === 0) {
            (cpu.registers.b + 1) land 0xff;
          } else {
            cpu.registers.b;
          };
        newState(~cpu=cpu |> setRegisters(~b, ~c) |> machineCycles(2), s);
      };
    let inc_de: op =
      ({cpu} as s) => {
        let e = (cpu.registers.e + 1) land 0xff; /* wrap after 255, i.e. 256 = 0 */
        let d =
          if (e === 0) {
            (cpu.registers.d + 1) land 0xff;
          } else {
            cpu.registers.d;
          };
        newState(~cpu=cpu |> setRegisters(~d, ~e) |> machineCycles(2), s);
      };
    let inc_hl: op =
      ({cpu} as s) => {
        newState(~cpu=cpu |> incrementHl |> machineCycles(2), s);
      };
    let inc_sp: op =
      ({cpu} as s) => {
        newState(~cpu=cpu |> incrementSp |> machineCycles(2), s);
      };
  };

  /* Increment 8-bit value */
  module Increment8 = {
    let inc = (register: int, cpu) => {
      let r = (register + 1) land 0xff;
      let f = cpu.registers.f land 0x10;
      (
        r,
        cpu
        |> setFlag(Z, b2i(r === 0), ~initialValue=f)
        |> setFlag(N, 0)
        |> machineCycles(1),
      );
    };
    let inc_a: op =
      ({cpu} as s) => {
        let (a, cpu) = inc(cpu.registers.a, cpu);
        newState(~cpu=cpu |> setRegisters(~a), s);
      };
    /* 04 */
    let inc_b: op =
      ({cpu} as s) => {
        let (b, cpu) = inc(cpu.registers.b, cpu);
        newState(~cpu=cpu |> setRegisters(~b), s);
      };
    let inc_c: op =
      ({cpu} as s) => {
        let (c, cpu) = inc(cpu.registers.c, cpu);
        newState(~cpu=cpu |> setRegisters(~c), s);
      };
    let inc_d: op =
      ({cpu} as s) => {
        let (d, cpu) = inc(cpu.registers.d, cpu);
        newState(~cpu=cpu |> setRegisters(~d), s);
      };
    let inc_e: op =
      ({cpu} as s) => {
        let (e, cpu) = inc(cpu.registers.e, cpu);
        newState(~cpu=cpu |> setRegisters(~e), s);
      };
    let inc_h: op =
      ({cpu} as s) => {
        let (h, cpu) = inc(cpu.registers.h, cpu);
        newState(~cpu=cpu |> setRegisters(~h), s);
      };
    let inc_l: op =
      ({cpu} as s) => {
        let (l, cpu) = inc(cpu.registers.l, cpu);
        newState(~cpu=cpu |> setRegisters(~l), s);
      };
  };

  /* Decrement */
  module Decrement8 = {
    let dec = (register: int, ~cycles=1, cpu) => {
      let r = (register - 1) land 0xff;
      let f = cpu.registers.f land 0x10;
      (
        r,
        cpu
        |> setFlag(Z, b2i(r === 0), ~initialValue=f)
        |> setFlag(N, 1)
        |> machineCycles(cycles),
      );
    };
    let dec_a: op =
      ({cpu} as s) => {
        let (a, cpu) = dec(cpu.registers.a, cpu);
        newState(~cpu=cpu |> setRegisters(~a), s);
      };
    /* 05 */
    let dec_b: op =
      ({cpu} as s) => {
        let (b, cpu) = dec(cpu.registers.b, cpu);
        newState(~cpu=cpu |> setRegisters(~b), s);
      };
    let dec_c: op =
      ({cpu} as s) => {
        let (c, cpu) = dec(cpu.registers.c, cpu);
        newState(~cpu=cpu |> setRegisters(~c), s);
      };
    let dec_d: op =
      ({cpu} as s) => {
        let (d, cpu) = dec(cpu.registers.d, cpu);
        newState(~cpu=cpu |> setRegisters(~d), s);
      };
    let dec_e: op =
      ({cpu} as s) => {
        let (e, cpu) = dec(cpu.registers.e, cpu);
        newState(~cpu=cpu |> setRegisters(~e), s);
      };
    let dec_h: op =
      ({cpu} as s) => {
        let (h, cpu) = dec(cpu.registers.h, cpu);
        newState(~cpu=cpu |> setRegisters(~h), s);
      };
    let dec_l: op =
      ({cpu} as s) => {
        let (l, cpu) = dec(cpu.registers.l, cpu);
        newState(~cpu=cpu |> setRegisters(~l), s);
      };
    let dec_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let addr = rHl(cpu);
        let (hl, _) = Mmu.read8(addr, {mmu, gpu});
        let (hl', cpu) = dec(hl, ~cycles=3, cpu);
        let (mmu, gpu) = Mmu.write8(addr, hl', {mmu, gpu});
        newState(~cpu, ~mmu, ~gpu, s);
      };
  };

  module Decrement16 = {
    /* 0B: DEC (BC) */
    let dec_bc: op =
      ({cpu} as s) => {
        let c = (cpu.registers.c - 1) land 0xff; /* wrap after 255, i.e. 256 = 0 */
        let b =
          if (c === 255) {
            (cpu.registers.b - 1) land 0xff;
          } else {
            cpu.registers.b;
          };
        newState(~cpu=cpu |> setRegisters(~b, ~c) |> machineCycles(2), s);
      };
    /* 1B: DEC DE */
    let dec_de: op =
      ({cpu} as s) => {
        let e = (cpu.registers.e - 1) land 0xff; /* wrap after 255, i.e. 256 = 0 */
        let d =
          if (e === 255) {
            (cpu.registers.d - 1) land 0xff;
          } else {
            cpu.registers.d;
          };
        newState(~cpu=cpu |> setRegisters(~d, ~e) |> machineCycles(2), s);
      };
    let dec_hl: op =
      ({cpu} as s) => {
        let cpu = decrementHl(cpu);
        newState(~cpu=cpu |> machineCycles(2), s);
      };
    let dec_sp: op =
      ({cpu} as s) => {
        newState(~cpu=cpu |> decrementSp |> machineCycles(2), s);
      };
  };

  module Rotation = {
    let rlc = (cpu, ~mCycles=1, register) => {
      let highBit = register land 0b10000000 > 0 ? 1 : 0;
      let r = (register lsl 1 + highBit) land 255;
      (
        r,
        cpu
        |> machineCycles(mCycles)
        |> setFlag(Flags.C, highBit)
        |> setFlag(Z, b2i(r === 0)),
      );
    };
    let rl = (cpu, ~mCycles=1, register) => {
      let highBit = register land 0b10000000 > 0 ? 1 : 0;
      let carry = cpu |> getFlag(C) > 0 ? 1 : 0;
      let r = (register lsl 1 + carry) land 255;
      (r, cpu |> machineCycles(mCycles) |> setFlag(Flags.C, highBit));
    };
    /* 07: RLCA */
    let rlca: op =
      ({cpu} as s) => {
        let (a, cpu) = rlc(cpu, cpu.registers.a);
        newState(~cpu=cpu |> setRegisters(~a), s);
      };
    let rlcb: op =
      ({cpu} as s) => {
        let (b, cpu) = rlc(cpu, ~mCycles=2, cpu.registers.b);
        newState(~cpu=cpu |> setRegisters(~b), s);
      };
    let rlcc: op =
      ({cpu} as s) => {
        let (c, cpu) = rlc(cpu, ~mCycles=2, cpu.registers.c);
        newState(~cpu=cpu |> setRegisters(~c), s);
      };
    let rlcd: op =
      ({cpu} as s) => {
        let (d, cpu) = rlc(cpu, ~mCycles=2, cpu.registers.d);
        newState(~cpu=cpu |> setRegisters(~d), s);
      };
    let rlce: op =
      ({cpu} as s) => {
        let (e, cpu) = rlc(cpu, ~mCycles=2, cpu.registers.e);
        newState(~cpu=cpu |> setRegisters(~e), s);
      };
    let rlch: op =
      ({cpu} as s) => {
        let (h, cpu) = rlc(cpu, ~mCycles=2, cpu.registers.h);
        newState(~cpu=cpu |> setRegisters(~h), s);
      };
    let rlcl: op =
      ({cpu} as s) => {
        let (l, cpu) = rlc(cpu, ~mCycles=2, cpu.registers.l);
        newState(~cpu=cpu |> setRegisters(~l), s);
      };
    /* 0F */
    // let rrca: op =
    //   ({cpu} as s) => {
    //     newState(~cpu=cpu |> machineCycles(1), s);
    //   };
    /* 17 */
    let rla: op =
      ({cpu} as s) => {
        let (a, cpu) = rl(cpu, cpu.registers.a);
        newState(
          ~cpu=cpu |> wA(a) |> setFlag(Z, 0) |> machineCycles(1),
          s,
        );
      };
    // /* 1F */
    // let rra: op =
    //   ({cpu} as s) => {
    //     newState(~cpu=cpu |> machineCycles(1), s);
    //   };
  };

  module Or = {
    let or_ = (cpu, value) => {
      let {a} = cpu.registers;
      let a' = a lor value;
      let fZ = a' === 0 |> b2i;
      cpu
      |> setFlag(Z, fZ)
      |> setFlag(N, 0)
      |> setFlag(H, 0)
      |> setFlag(C, 0)
      |> setRegisters(~a=a' land 0xff);
    };
    let or_a: op =
      ({cpu} as s) => {
        let cpu = or_(cpu, cpu.registers.a);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let or_b: op =
      ({cpu} as s) => {
        let cpu = or_(cpu, cpu.registers.b);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let or_c: op =
      ({cpu} as s) => {
        let cpu = or_(cpu, cpu.registers.c);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let or_d: op =
      ({cpu} as s) => {
        let cpu = or_(cpu, cpu.registers.d);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let or_e: op =
      ({cpu} as s) => {
        let cpu = or_(cpu, cpu.registers.e);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let or_h: op =
      ({cpu} as s) => {
        let cpu = or_(cpu, cpu.registers.h);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let or_l: op =
      ({cpu} as s) => {
        let cpu = or_(cpu, cpu.registers.l);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let or_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (value, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        let cpu = or_(cpu, value);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
    let or_n: op =
      ({cpu, mmu, gpu} as s) => {
        let (pc, cpu) = readAndIncrementPc(cpu);
        let (value, mmu) = Mmu.read8(pc, {mmu, gpu});
        let cpu = or_(cpu, value);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
  };

  module Xor = {
    let xor_ = (cpu, value) => {
      let {a} = cpu.registers;
      let a' = a lxor value;
      let fZ = a' === 0 |> b2i;
      cpu
      |> setFlag(Z, fZ)
      |> setFlag(N, 0)
      |> setFlag(H, 0)
      |> setFlag(C, 0)
      |> setRegisters(~a=a' land 0xff);
    };
    let xor_a: op =
      ({cpu} as s) => {
        Printf.printf("XOR A\n");
        let cpu = xor_(cpu, cpu.registers.a);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let xor_b: op =
      ({cpu} as s) => {
        let cpu = xor_(cpu, cpu.registers.b);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let xor_c: op =
      ({cpu} as s) => {
        let cpu = xor_(cpu, cpu.registers.c);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let xor_d: op =
      ({cpu} as s) => {
        let cpu = xor_(cpu, cpu.registers.d);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let xor_e: op =
      ({cpu} as s) => {
        let cpu = xor_(cpu, cpu.registers.e);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let xor_h: op =
      ({cpu} as s) => {
        let cpu = xor_(cpu, cpu.registers.h);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let xor_l: op =
      ({cpu} as s) => {
        let cpu = xor_(cpu, cpu.registers.l);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let xor_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (value, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        let cpu = xor_(cpu, value);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
    let xor_n: op =
      ({cpu, mmu, gpu} as s) => {
        let (pc, cpu) = readAndIncrementPc(cpu);
        let (value, mmu) = Mmu.read8(pc, {mmu, gpu});
        let cpu = xor_(cpu, value);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
  };

  module Push = {
    let push = ({cpu, mmu, gpu}, register1, register2) => {
      let cpu = decrementSp(cpu);
      let (mmu, gpu) = Mmu.write8(cpu.registers.sp, register1, {mmu, gpu});
      let cpu = decrementSp(cpu);
      let (mmu, gpu) = Mmu.write8(cpu.registers.sp, register2, {mmu, gpu});

      (cpu |> machineCycles(4), mmu, gpu);
    };
    let push_af: op =
      ({cpu} as s) => {
        let (cpu, mmu, gpu) = push(s, cpu.registers.a, cpu.registers.f);
        newState(~cpu, ~mmu, ~gpu, s);
      };
    let push_bc: op =
      ({cpu} as s) => {
        let (cpu, mmu, gpu) = push(s, cpu.registers.b, cpu.registers.c);
        newState(~cpu, ~mmu, ~gpu, s);
      };
    let push_de: op =
      ({cpu} as s) => {
        let (cpu, mmu, gpu) = push(s, cpu.registers.d, cpu.registers.e);
        newState(~cpu, ~mmu, ~gpu, s);
      };
    let push_hl: op =
      ({cpu} as s) => {
        let (cpu, mmu, gpu) = push(s, cpu.registers.h, cpu.registers.l);
        newState(~cpu, ~mmu, ~gpu, s);
      };
  };

  module Pop = {
    let pop = ({cpu, mmu, gpu}) => {
      let (r2, mmu) = Mmu.read8(cpu.registers.sp, {mmu, gpu});
      let cpu = incrementSp(cpu);
      let (r1, mmu) = Mmu.read8(cpu.registers.sp, {mmu, gpu});
      let cpu = incrementSp(cpu);

      (cpu |> machineCycles(3), mmu, r1, r2);
    };
    let pop_af: op =
      s => {
        let (cpu, mmu, a, f) = pop(s);
        newState(~cpu=cpu |> setRegisters(~a, ~f), ~mmu, s);
      };
    let pop_bc: op =
      s => {
        let (cpu, mmu, b, c) = pop(s);
        newState(~cpu=cpu |> setRegisters(~b, ~c), ~mmu, s);
      };
    let pop_de: op =
      s => {
        let (cpu, mmu, d, e) = pop(s);
        newState(~cpu=cpu |> setRegisters(~d, ~e), ~mmu, s);
      };
    let pop_hl: op =
      s => {
        let (cpu, mmu, h, l) = pop(s);
        newState(~cpu=cpu |> setRegisters(~h, ~l), ~mmu, s);
      };
  };

  module Add = {
    let isCarry = value => {
      value > 0xffff;
    };
    let isHalfCarry = (a, b) => {
      // true if the half carry bit should be set
      // i.e. there was a carry on the lower 4 bits
      a land 0xf + b land 0xf > 0xf;
    };
    let add_a = (cpu, value) => {
      let {a} = cpu.registers;
      let a' = a + value;
      let fC = isCarry(a') |> b2i;
      let fH = isHalfCarry(a, value) |> b2i;
      let fZ = a' === 0 |> b2i;
      cpu
      |> setFlag(Z, fZ)
      |> setFlag(N, 0)
      |> setFlag(H, fH)
      |> setFlag(C, fC)
      |> setRegisters(~a=a' land 0xff);
    };
    let add_a_a: op =
      ({cpu} as s) => {
        let cpu = add_a(cpu, cpu.registers.a);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let add_a_b: op =
      ({cpu} as s) => {
        let cpu = add_a(cpu, cpu.registers.b);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let add_a_c: op =
      ({cpu} as s) => {
        let cpu = add_a(cpu, cpu.registers.c);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let add_a_d: op =
      ({cpu} as s) => {
        let cpu = add_a(cpu, cpu.registers.d);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let add_a_e: op =
      ({cpu} as s) => {
        let cpu = add_a(cpu, cpu.registers.e);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let add_a_h: op =
      ({cpu} as s) => {
        let cpu = add_a(cpu, cpu.registers.h);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let add_a_l: op =
      ({cpu} as s) => {
        let cpu = add_a(cpu, cpu.registers.l);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let add_a_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (value, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        let cpu = add_a(cpu, value);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
    let add_a_n: op =
      ({cpu, mmu, gpu} as s) => {
        let (value, mmu) = Mmu.read8(programCount(cpu), {mmu, gpu});
        let cpu = add_a(cpu, value);
        newState(~cpu=cpu |> machineCycles(2) |> incrementPc(1), ~mmu, s);
      };
    let adc_a_a: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = add_a(cpu, cpu.registers.a + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let adc_a_b: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = add_a(cpu, cpu.registers.b + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let adc_a_c: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = add_a(cpu, cpu.registers.c + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let adc_a_d: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = add_a(cpu, cpu.registers.d + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let adc_a_e: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = add_a(cpu, cpu.registers.e + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let adc_a_h: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = add_a(cpu, cpu.registers.h + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let adc_a_l: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = add_a(cpu, cpu.registers.l + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let adc_a_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let (value, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        let cpu = add_a(cpu, value + carry);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
    let adc_a_n: op =
      ({cpu, mmu, gpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let (value, mmu) = Mmu.read8(programCount(cpu), {mmu, gpu});
        let cpu = add_a(cpu, value + carry);
        newState(~cpu=cpu |> machineCycles(2) |> incrementPc(1), ~mmu, s);
      };
  };

  module Add_16 = {
    let isCarry = Add.isCarry;
    let isHalfCarry = Add.isHalfCarry;

    let add_hl = (cpu, value) => {
      let hl = rHl(cpu) + value;
      let h = hl lsr 8 land 255;
      let l = hl land 255;

      (
        cpu
        |> setFlag(N, 0)
        |> setFlag(H, isHalfCarry(rHl(cpu), value) |> b2i)
        |> setFlag(C, isCarry(hl) |> b2i)
        |> machineCycles(2),
        h,
        l,
      );
    };
    /* 09: ADD HL,BC */
    let add_hl_bc: op =
      ({cpu} as s) => {
        let (cpu, h, l) = add_hl(cpu, rBc(cpu));
        newState(~cpu=cpu |> setRegisters(~h, ~l), s);
      };
    let add_hl_de: op =
      ({cpu} as s) => {
        let (cpu, h, l) = add_hl(cpu, rDe(cpu));
        newState(~cpu=cpu |> setRegisters(~h, ~l), s);
      };
    let add_hl_hl: op =
      ({cpu} as s) => {
        let (cpu, h, l) = add_hl(cpu, rHl(cpu));
        newState(~cpu=cpu |> setRegisters(~h, ~l), s);
      };
    let add_hl_sp: op =
      ({cpu} as s) => {
        let (cpu, h, l) = add_hl(cpu, cpu.registers.sp);
        newState(~cpu=cpu |> setRegisters(~h, ~l), s);
      };
    let add_sp_n: op =
      ({cpu, mmu, gpu} as s) => {
        let (n, mmu) = Mmu.read8(programCount(cpu), {mmu, gpu});
        // TODO: set flags H and/or C?
        s
        |> newState(
             ~cpu=
               cpu
               |> setFlag(Z, 0)
               |> setFlag(N, 0)
               |> incrementSp(~amount=signed(n))
               |> incrementPc(1)
               |> machineCycles(4),
             ~mmu,
           );
      };
  };

  module Sub = {
    let isBorrow = (a, b) => {
      a < b;
    };
    let isHalfBorrow = (a, b) => {
      a land 0xf - b land 0xf > 0xf;
    };

    let sub = (cpu, value) => {
      let {a} = cpu.registers;
      let a' = a - value;
      let fC = isBorrow(a, value) |> b2i;
      let fH = isHalfBorrow(a, value) |> b2i;
      let fZ = a' === 0 |> b2i;
      cpu
      |> setFlag(Z, fZ)
      |> setFlag(N, 1)
      |> setFlag(H, fH)
      |> setFlag(C, fC)
      |> setRegisters(~a=a' land 0xff);
    };

    let sub_a: op =
      ({cpu} as s) => {
        let cpu = sub(cpu, cpu.registers.a);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sub_b: op =
      ({cpu} as s) => {
        let cpu = sub(cpu, cpu.registers.b);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sub_c: op =
      ({cpu} as s) => {
        let cpu = sub(cpu, cpu.registers.c);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sub_d: op =
      ({cpu} as s) => {
        let cpu = sub(cpu, cpu.registers.d);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sub_e: op =
      ({cpu} as s) => {
        let cpu = sub(cpu, cpu.registers.e);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sub_h: op =
      ({cpu} as s) => {
        let cpu = sub(cpu, cpu.registers.h);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sub_l: op =
      ({cpu} as s) => {
        let cpu = sub(cpu, cpu.registers.l);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sub_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (value, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        let cpu = sub(cpu, value);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
    let sub_n: op =
      ({cpu, mmu, gpu} as s) => {
        let (value, mmu) = Mmu.read8(programCount(cpu), {mmu, gpu});
        let cpu = sub(cpu, value);
        newState(~cpu=cpu |> machineCycles(2) |> incrementPc(1), ~mmu, s);
      };
    let sbc_a: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = sub(cpu, cpu.registers.a + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sbc_b: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = sub(cpu, cpu.registers.b + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sbc_c: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = sub(cpu, cpu.registers.c + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sbc_d: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = sub(cpu, cpu.registers.d + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sbc_e: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = sub(cpu, cpu.registers.e + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sbc_h: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = sub(cpu, cpu.registers.h + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sbc_l: op =
      ({cpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let cpu = sub(cpu, cpu.registers.l + carry);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let sbc_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let carry = Flags.getFlag(C, cpu.registers.f);
        let (value, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        let cpu = sub(cpu, value + carry);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
  };

  module And = {
    let and_ = (cpu, value) => {
      let {a} = cpu.registers;
      let a' = a land value;
      let fZ = a' === 0 |> b2i;
      cpu
      |> setFlag(Z, fZ)
      |> setFlag(N, 0)
      |> setFlag(H, 1)
      |> setFlag(C, 0)
      |> setRegisters(~a=a' land 0xff);
    };

    let and_a: op =
      ({cpu} as s) => {
        let cpu = and_(cpu, cpu.registers.a);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let and_b: op =
      ({cpu} as s) => {
        let cpu = and_(cpu, cpu.registers.b);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let and_c: op =
      ({cpu} as s) => {
        let cpu = and_(cpu, cpu.registers.c);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let and_d: op =
      ({cpu} as s) => {
        let cpu = and_(cpu, cpu.registers.d);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let and_e: op =
      ({cpu} as s) => {
        let cpu = and_(cpu, cpu.registers.e);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let and_h: op =
      ({cpu} as s) => {
        let cpu = and_(cpu, cpu.registers.h);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let and_l: op =
      ({cpu} as s) => {
        let cpu = and_(cpu, cpu.registers.l);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let and_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (value, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        let cpu = and_(cpu, value);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
    let and_n: op =
      ({cpu, mmu, gpu} as s) => {
        let (pc, cpu) = readAndIncrementPc(cpu);
        let (value, mmu) = Mmu.read8(pc, {mmu, gpu});
        let cpu = and_(cpu, value);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
  };

  module Cp = {
    let isBorrow = (a, b) => {
      a < b;
    };
    let isHalfBorrow = (a, b) => {
      a land 0xf - b land 0xf > 0xf;
    };

    let cp = (cpu, value) => {
      let {a} = cpu.registers;
      let a' = a - value;
      let fC = isBorrow(a, value) |> b2i;
      let fH = isHalfBorrow(a, value) |> b2i;
      let fZ = a' === 0 |> b2i;
      cpu
      |> setFlag(Z, fZ)
      |> setFlag(N, 1)
      |> setFlag(H, fH)
      |> setFlag(C, fC);
    };

    let cp_a: op =
      ({cpu} as s) => {
        let cpu = cp(cpu, cpu.registers.a);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let cp_b: op =
      ({cpu} as s) => {
        let cpu = cp(cpu, cpu.registers.b);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let cp_c: op =
      ({cpu} as s) => {
        let cpu = cp(cpu, cpu.registers.c);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let cp_d: op =
      ({cpu} as s) => {
        let cpu = cp(cpu, cpu.registers.d);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let cp_e: op =
      ({cpu} as s) => {
        let cpu = cp(cpu, cpu.registers.e);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let cp_h: op =
      ({cpu} as s) => {
        let cpu = cp(cpu, cpu.registers.h);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let cp_l: op =
      ({cpu} as s) => {
        let cpu = cp(cpu, cpu.registers.l);
        newState(~cpu=cpu |> machineCycles(1), s);
      };
    let cp_m_hl: op =
      ({cpu, mmu, gpu} as s) => {
        let (value, mmu) = Mmu.read8(rHl(cpu), {mmu, gpu});
        let cpu = cp(cpu, value);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };
    let cp_n: op =
      ({cpu, mmu, gpu} as s) => {
        let (value, mmu) = Mmu.read8(programCount(cpu), {mmu, gpu});
        let cpu = cp(cpu, value);
        newState(~cpu=cpu |> machineCycles(2) |> incrementPc(1), ~mmu, s);
      };
  };

  module Misc = {
    let daa: op =
      ({cpu} as s) => {
        // var a=Z80._r.a;
        // if((Z80._r.f&0x20)||((Z80._r.a&15)>9)) Z80._r.a+=6;
        // Z80._r.f&=0xEF;
        // if((Z80._r.f&0x20)||(a>0x99)) { Z80._r.a+=0x60; Z80._r.f|=0x10; }
        // Z80._r.m=1;
        Js.log("DAA");
        let {a, f} = cpu.registers;
        let a =
          if (f land 0x20 !== 0 || a land 0xf > 9) {
            a + 6;
          } else {
            a;
          };
        cpu.registers.f = cpu.registers.f land 0xEF;
        let (a, f) =
          if (cpu.registers.f land 0x20 !== 0 || a > 0x99) {
            (a + 0x60, cpu.registers.f lor 0x10);
          } else {
            (a, cpu.registers.f);
          };

        let a = a land 0xff;
        let cpu =
          cpu
          |> setRegisters(~a, ~f)
          |> setFlag(N, 0)
          |> setFlag(H, 0)
          |> setFlag(C, 0)
          |> machineCycles(1);

        newState(~cpu, s);
      };
    let cpl: op =
      ({cpu} as s) => {
        let a = cpu.registers.a lxor 0xff;
        let cpu =
          cpu
          |> setRegisters(~a)
          |> setFlag(N, 1)
          |> setFlag(H, 1)
          |> machineCycles(1);
        newState(~cpu, s);
      };
    let ccf: op =
      ({cpu} as s) => {
        s
        |> newState(
             ~cpu=
               cpu
               |> setFlag(N, 0)
               |> setFlag(H, 0)
               |> toggleFlag(C)
               |> machineCycles(1),
           );
      };
    let scf: op =
      ({cpu} as s) => {
        s
        |> newState(
             ~cpu=
               cpu
               |> setFlag(N, 0)
               |> setFlag(H, 0)
               |> setFlag(C, 1)
               |> machineCycles(1),
           );
      };
    let halt: op =
      ({cpu} as s) => {
        cpu.halt = 1;
        newState(~cpu, s);
      };
  };

  module Jump = {
    let jp_nn: op =
      ({cpu, mmu, gpu} as s) => {
        let (nn, mmu) = Mmu.read16(programCount(cpu), {mmu, gpu});
        cpu.registers.pc = nn;
        newState(~cpu=cpu |> machineCycles(3), ~mmu, s);
      };
    let jp_m_hl: op =
      ({cpu, mmu} as s) => {
        cpu.registers.pc = rHl(cpu);
        newState(~cpu=cpu |> machineCycles(4), ~mmu, s);
      };
    let jp_nz_nn: op =
      ({cpu, mmu, gpu} as s) => {
        let (pc, mmu, mCycles) =
          if (!Flags.isSet(Z, cpu.registers.f)) {
            let (pc', mmu) = Mmu.read16(programCount(cpu), {mmu, gpu});
            (pc', mmu, 4);
          } else {
            let pc' = programCount(cpu) + 2;
            (pc', mmu, 3);
          };
        cpu.registers.pc = pc;

        newState(~cpu=cpu |> machineCycles(mCycles), ~mmu, s);
      };
    let jp_z_nn: op =
      ({cpu, mmu, gpu} as s) => {
        let (pc, mmu, mCycles) =
          if (Flags.isSet(Z, cpu.registers.f)) {
            // if (cpu.registers.f land 0xb10000000 === 0b10000000) {
            let (pc', mmu) = Mmu.read16(programCount(cpu), {mmu, gpu});
            (pc', mmu, 4);
          } else {
            let pc' = programCount(cpu) + 2;
            (pc', mmu, 3);
          };
        cpu.registers.pc = pc;

        newState(~cpu=cpu |> machineCycles(mCycles), ~mmu, s);
      };
    let jp_nc_nn: op =
      ({cpu, mmu, gpu} as s) => {
        let (pc, mmu, mCycles) =
          if (!Flags.isSet(C, cpu.registers.f)) {
            // if (cpu.registers.f land 0xb00010000 === 0x00) {
            let (pc', mmu) = Mmu.read16(programCount(cpu), {mmu, gpu});
            (pc', mmu, 4);
          } else {
            let pc' = programCount(cpu) + 2;
            (pc', mmu, 3);
          };
        cpu.registers.pc = pc;

        newState(~cpu=cpu |> machineCycles(mCycles), ~mmu, s);
      };
    let jp_c_nn: op =
      ({cpu, mmu, gpu} as s) => {
        let (pc, mmu, mCycles) =
          if (Flags.isSet(C, cpu.registers.f)) {
            // if (cpu.registers.f land 0xb00010000 === 0b00010000) {
            let (pc', mmu) = Mmu.read16(programCount(cpu), {mmu, gpu});
            (pc', mmu, 4);
          } else {
            let pc' = programCount(cpu) + 2;
            (pc', mmu, 3);
          };
        cpu.registers.pc = pc;

        newState(~cpu=cpu |> machineCycles(mCycles), ~mmu, s);
      };
    let jr_n: op =
      ({cpu, mmu, gpu} as s) => {
        let (n, mmu) = Mmu.read8(programCount(cpu), {mmu, gpu});
        newState(
          ~cpu=cpu |> incrementPc(signed(n) + 1) |> machineCycles(2),
          ~mmu,
          s,
        );
      };
    let jr_nz_n: op =
      ({cpu, mmu, gpu} as s) => {
        let pc = programCount(cpu);
        let (n, mmu) = Mmu.read8(pc, {mmu, gpu});
        let (pc, mCycles) =
          if (!Flags.isSet(Z, cpu.registers.f)) {
            (pc + 1 + signed(n), 3);
          } else {
            (pc + 1, 2);
          };
        newState(
          ~cpu=cpu |> incrementPc(pc) |> machineCycles(mCycles),
          ~mmu,
          s,
        );
      };
    let jr_z_n: op =
      ({cpu, mmu, gpu} as s) => {
        let pc = programCount(cpu);
        let (n, mmu) = Mmu.read8(pc, {mmu, gpu});
        let (pc, mCycles) =
          if (Flags.isSet(Z, cpu.registers.f)) {
            (pc + 1 + signed(n), 3);
          } else {
            (pc + 1, 2);
          };
        s
        |> newState(
             ~cpu=cpu |> incrementPc(pc) |> machineCycles(mCycles),
             ~mmu,
           );
      };
    let jr_nc_n: op =
      ({cpu, mmu, gpu} as s) => {
        let pc = programCount(cpu);
        let (n, mmu) = Mmu.read8(pc, {mmu, gpu});
        let (pc, mCycles) =
          if (!Flags.isSet(C, cpu.registers.f)) {
            (pc + 1 + signed(n), 3);
          } else {
            (pc + 1, 2);
          };
        s
        |> newState(
             ~cpu=cpu |> incrementPc(pc) |> machineCycles(mCycles),
             ~mmu,
           );
      };
    let jr_c_n: op =
      ({cpu, mmu, gpu} as s) => {
        let pc = programCount(cpu);
        let (n, mmu) = Mmu.read8(pc, {mmu, gpu});
        let (pc, mCycles) =
          if (Flags.isSet(C, cpu.registers.f)) {
            (pc + 1 + signed(n), 3);
          } else {
            (pc + 1, 2);
          };
        s
        |> newState(
             ~cpu=cpu |> incrementPc(pc) |> machineCycles(mCycles),
             ~mmu,
           );
      };
  };

  module Call = {
    let call = ({cpu, mmu, gpu}) => {
      let cpu = cpu |> decrementSp(~amount=2);
      let (mmu, gpu) =
        Mmu.write16(cpu.registers.sp, programCount(cpu) + 2, {mmu, gpu});
      let (pc, mmu) = Mmu.read16(programCount(cpu), {mmu, gpu});
      (pc, mmu);
    };
    let call_nn: op =
      ({cpu} as s) => {
        let (pc, mmu) = call(s);
        let cpu = cpu |> setPc(pc) |> machineCycles(4);
        newState(~cpu, ~mmu, s);
      };
    let call_nz_nn: op =
      ({cpu, mmu} as s) => {
        let cpu = cpu |> machineCycles(3);
        let pc = programCount(cpu);
        let (pc, mmu, mCycles) =
          if (!Flags.isSet(Z, cpu.registers.f)) {
            let (pc, mmu) = call(s);
            (pc, mmu, 1);
          } else {
            (pc + 2, mmu, 0);
          };
        newState(~cpu=cpu |> setPc(pc) |> machineCycles(mCycles), ~mmu, s);
      };
  };

  module Ret = {
    let ret_ = (cpu, mmu, gpu) => {
      let (addr, mmu) = Mmu.read16(cpu.registers.sp, {mmu, gpu});
      let cpu = setPc(addr, cpu);
      cpu.registers.sp = cpu.registers.sp + 2;
      (cpu, mmu);
    };

    let ret: op =
      ({cpu, mmu, gpu} as s) => {
        let (cpu, mmu) = ret_(cpu, mmu, gpu);
        newState(~cpu=cpu |> machineCycles(2), ~mmu, s);
      };

    let ret_nz: op =
      ({cpu, mmu, gpu} as s) => {
        let (cpu, mmu) =
          if (!Flags.isSet(Z, cpu.registers.f)) {
            let (cpu, mmu) = ret_(cpu, mmu, gpu);
            (cpu |> machineCycles(3), mmu);
          } else {
            (cpu |> machineCycles(1), mmu);
          };
        newState(~cpu, ~mmu, s);
      };
    let ret_z: op =
      ({cpu, mmu, gpu} as s) => {
        let (cpu, mmu) =
          if (Flags.isSet(Z, cpu.registers.f)) {
            let (cpu, mmu) = ret_(cpu, mmu, gpu);
            (cpu |> machineCycles(3), mmu);
          } else {
            (cpu |> machineCycles(1), mmu);
          };
        newState(~cpu, ~mmu, s);
      };
    let ret_nc: op =
      ({cpu, mmu, gpu} as s) => {
        let (cpu, mmu) =
          if (!Flags.isSet(C, cpu.registers.f)) {
            let (cpu, mmu) = ret_(cpu, mmu, gpu);
            (cpu |> machineCycles(3), mmu);
          } else {
            (cpu |> machineCycles(1), mmu);
          };
        newState(~cpu, ~mmu, s);
      };
    let ret_c: op =
      ({cpu, mmu, gpu} as s) => {
        let (cpu, mmu) =
          if (Flags.isSet(C, cpu.registers.f)) {
            let (cpu, mmu) = ret_(cpu, mmu, gpu);
            (cpu |> machineCycles(3), mmu);
          } else {
            (cpu |> machineCycles(1), mmu);
          };
        newState(~cpu, ~mmu, s);
      };
  };

  module Rst = {};

  module CbOps = Cpu_cbops;
  module Swap = Cpu_cbops.Swap;

  module Cb = {
    let cb_exec: op =
      ({cpu, mmu, gpu}) => {
        let (pc, cpu) = readAndIncrementPc(cpu);
        let (nextInstr, mmu) = Mmu.read8(pc land 0xffff, {mmu, gpu});
        let cbop =
          switch (nextInstr) {
          | 0x10 => CbOps.Rl.rl_b
          | 0x11 => CbOps.Rl.rl_c
          | 0x12 => CbOps.Rl.rl_d
          | 0x13 => CbOps.Rl.rl_e
          | 0x14 => CbOps.Rl.rl_h
          | 0x15 => CbOps.Rl.rl_l
          | 0x16 => CbOps.Rl.rl_m_hl
          | 0x17 => CbOps.Rl.rl_a
          | 0x30 => Swap.swap_b
          | 0x37 => Swap.swap_a
          | 0x40 => CbOps.Bit.Bit0.bit_b
          | 0x41 => CbOps.Bit.Bit0.bit_c
          | 0x42 => CbOps.Bit.Bit0.bit_d
          | 0x43 => CbOps.Bit.Bit0.bit_e
          | 0x44 => CbOps.Bit.Bit0.bit_h
          | 0x45 => CbOps.Bit.Bit0.bit_l
          | 0x46 => CbOps.Bit.Bit0.bit_m_hl
          | 0x47 => CbOps.Bit.Bit0.bit_a
          | 0x48 => CbOps.Bit.Bit1.bit_b
          | 0x49 => CbOps.Bit.Bit1.bit_c
          | 0x4A => CbOps.Bit.Bit1.bit_d
          | 0x4B => CbOps.Bit.Bit1.bit_e
          | 0x4C => CbOps.Bit.Bit1.bit_h
          | 0x4D => CbOps.Bit.Bit1.bit_l
          | 0x4E => CbOps.Bit.Bit1.bit_m_hl
          | 0x4F => CbOps.Bit.Bit1.bit_a
          | 0x50 => CbOps.Bit.Bit2.bit_b
          | 0x51 => CbOps.Bit.Bit2.bit_c
          | 0x52 => CbOps.Bit.Bit2.bit_d
          | 0x53 => CbOps.Bit.Bit2.bit_e
          | 0x54 => CbOps.Bit.Bit2.bit_h
          | 0x55 => CbOps.Bit.Bit2.bit_l
          | 0x56 => CbOps.Bit.Bit2.bit_m_hl
          | 0x57 => CbOps.Bit.Bit2.bit_a
          | 0x58 => CbOps.Bit.Bit3.bit_b
          | 0x59 => CbOps.Bit.Bit3.bit_c
          | 0x5A => CbOps.Bit.Bit3.bit_d
          | 0x5B => CbOps.Bit.Bit3.bit_e
          | 0x5C => CbOps.Bit.Bit3.bit_h
          | 0x5D => CbOps.Bit.Bit3.bit_l
          | 0x5E => CbOps.Bit.Bit3.bit_m_hl
          | 0x5F => CbOps.Bit.Bit3.bit_a
          | 0x60 => CbOps.Bit.Bit4.bit_b
          | 0x61 => CbOps.Bit.Bit4.bit_c
          | 0x62 => CbOps.Bit.Bit4.bit_d
          | 0x63 => CbOps.Bit.Bit4.bit_e
          | 0x64 => CbOps.Bit.Bit4.bit_h
          | 0x65 => CbOps.Bit.Bit4.bit_l
          | 0x66 => CbOps.Bit.Bit4.bit_m_hl
          | 0x67 => CbOps.Bit.Bit4.bit_a
          | 0x68 => CbOps.Bit.Bit5.bit_b
          | 0x69 => CbOps.Bit.Bit5.bit_c
          | 0x6A => CbOps.Bit.Bit5.bit_d
          | 0x6B => CbOps.Bit.Bit5.bit_e
          | 0x6C => CbOps.Bit.Bit5.bit_h
          | 0x6D => CbOps.Bit.Bit5.bit_l
          | 0x6E => CbOps.Bit.Bit5.bit_m_hl
          | 0x6F => CbOps.Bit.Bit5.bit_a
          | 0x70 => CbOps.Bit.Bit6.bit_b
          | 0x71 => CbOps.Bit.Bit6.bit_c
          | 0x72 => CbOps.Bit.Bit6.bit_d
          | 0x73 => CbOps.Bit.Bit6.bit_e
          | 0x74 => CbOps.Bit.Bit6.bit_h
          | 0x75 => CbOps.Bit.Bit6.bit_l
          | 0x76 => CbOps.Bit.Bit6.bit_m_hl
          | 0x77 => CbOps.Bit.Bit6.bit_a
          | 0x78 => CbOps.Bit.Bit7.bit_b
          | 0x79 => CbOps.Bit.Bit7.bit_c
          | 0x7A => CbOps.Bit.Bit7.bit_d
          | 0x7B => CbOps.Bit.Bit7.bit_e
          | 0x7C => CbOps.Bit.Bit7.bit_h
          | 0x7D => CbOps.Bit.Bit7.bit_l
          | 0x7E => CbOps.Bit.Bit7.bit_m_hl
          | 0x7F => CbOps.Bit.Bit7.bit_a
          | _ =>
            raise(
              Cpu.UnhandledInstruction(
                Printf.sprintf("Unhandled instruction, CB %x", nextInstr),
              ),
            )
          };
        cbop({cpu, mmu, gpu});
      };
  };
};

let exec: int => op =
  instruction =>
    switch (instruction) {
    | 0x00 => Ops.nop
    | 0x01 => Ops.Load_nn_16.ld_bc_nn
    | 0x02 => Ops.Load_8_A.ld_m_bc_a
    | 0x03 => Ops.Increment16.inc_bc
    | 0x04 => Ops.Increment8.inc_b
    | 0x05 => Ops.Decrement8.dec_b
    | 0x06 => Ops.Load_nn_8.ld_b_n
    | 0x07 => Ops.Rotation.rlca
    | 0x08 => Ops.Load_nn_16.ld_m_nn_sp
    | 0x09 => Ops.Add_16.add_hl_bc
    | 0x0A => Ops.Load_A_8.ld_a_m_bc
    | 0x0B => Ops.Decrement16.dec_bc
    | 0x0C => Ops.Increment8.inc_c
    | 0x0D => Ops.Decrement8.dec_c
    | 0x0E => Ops.Load_nn_8.ld_c_n
    // | 0x0F => Ops.Rotation.rrca
    /* | 0x10 => (s) => {
         Js.log("STOP");
         s
       } */
    | 0x11 => Ops.Load_nn_16.ld_de_nn
    | 0x12 => Ops.Load_8_A.ld_m_de_a
    | 0x13 => Ops.Increment16.inc_de
    | 0x14 => Ops.Increment8.inc_d
    | 0x15 => Ops.Decrement8.dec_d
    | 0x16 => Ops.Load_nn_8.ld_d_n
    | 0x17 => Ops.Rotation.rla
    | 0x18 => Ops.Jump.jr_n
    | 0x19 => Ops.Add_16.add_hl_de
    | 0x1A => Ops.Load_A_8.ld_a_m_de
    | 0x1B => Ops.Decrement16.dec_de
    | 0x1C => Ops.Increment8.inc_e
    | 0x1D => Ops.Decrement8.dec_e
    | 0x1E => Ops.Load_nn_8.ld_e_n
    // | 0x1F => Ops.Rotation.rra
    | 0x20 => Ops.Jump.jr_nz_n
    | 0x21 => Ops.Load_nn_16.ld_hl_nn
    | 0x22 => Ops.Load_8_A.ld_hli_a
    | 0x23 => Ops.Increment16.inc_hl
    | 0x24 => Ops.Increment8.inc_h
    | 0x25 => Ops.Decrement8.dec_h
    | 0x26 => Ops.Load_nn_8.ld_h_n
    | 0x27 => Ops.Misc.daa
    | 0x28 => Ops.Jump.jr_z_n
    | 0x29 => Ops.Add_16.add_hl_hl
    | 0x2A => Ops.Load_A_8.ld_a_m_hli
    | 0x2B => Ops.Decrement16.dec_hl
    | 0x2C => Ops.Increment8.inc_l
    | 0x2D => Ops.Decrement8.dec_l
    | 0x2E => Ops.Load_nn_8.ld_l_n
    | 0x2F => Ops.Misc.cpl
    | 0x30 => Ops.Jump.jr_nc_n
    | 0x31 => Ops.Load_nn_16.ld_sp_nn
    | 0x32 => Ops.Load_8_A.ld_hld_a
    | 0x33 => Ops.Increment16.inc_sp
    // | 0x34 => Ops.nop
    | 0x35 => Ops.Decrement8.dec_m_hl
    | 0x36 => Ops.Load_r1_r2.ld_m_hl_n
    | 0x37 => Ops.Misc.scf
    | 0x38 => Ops.Jump.jr_c_n
    | 0x39 => Ops.Add_16.add_hl_sp
    | 0x3A => Ops.Load_A_8.ld_a_m_hld
    | 0x3B => Ops.Decrement16.dec_sp
    | 0x3C => Ops.Increment8.inc_a
    | 0x3D => Ops.Decrement8.dec_a
    | 0x3E => Ops.Load_A_8.ld_a_m_n
    | 0x3F => Ops.Misc.ccf
    | 0x40 => Ops.Load_r1_r2.ld_b_b
    | 0x41 => Ops.Load_r1_r2.ld_b_c
    | 0x42 => Ops.Load_r1_r2.ld_b_d
    | 0x43 => Ops.Load_r1_r2.ld_b_e
    | 0x44 => Ops.Load_r1_r2.ld_b_h
    | 0x45 => Ops.Load_r1_r2.ld_b_l
    | 0x46 => Ops.Load_r1_r2.ld_b_m_hl
    | 0x47 => Ops.Load_8_A.ld_b_a
    | 0x48 => Ops.Load_r1_r2.ld_c_b
    | 0x49 => Ops.Load_r1_r2.ld_c_c
    | 0x4A => Ops.Load_r1_r2.ld_c_d
    | 0x4B => Ops.Load_r1_r2.ld_c_e
    | 0x4C => Ops.Load_r1_r2.ld_c_h
    | 0x4D => Ops.Load_r1_r2.ld_c_l
    | 0x4E => Ops.Load_r1_r2.ld_c_m_hl
    | 0x4F => Ops.Load_8_A.ld_c_a
    | 0x50 => Ops.Load_r1_r2.ld_d_b
    | 0x51 => Ops.Load_r1_r2.ld_d_c
    | 0x52 => Ops.Load_r1_r2.ld_d_d
    | 0x53 => Ops.Load_r1_r2.ld_d_e
    | 0x54 => Ops.Load_r1_r2.ld_d_h
    | 0x55 => Ops.Load_r1_r2.ld_d_l
    | 0x56 => Ops.Load_r1_r2.ld_d_m_hl
    | 0x57 => Ops.Load_8_A.ld_d_a
    | 0x58 => Ops.Load_r1_r2.ld_e_b
    | 0x59 => Ops.Load_r1_r2.ld_e_c
    | 0x5A => Ops.Load_r1_r2.ld_e_d
    | 0x5B => Ops.Load_r1_r2.ld_e_e
    | 0x5C => Ops.Load_r1_r2.ld_e_h
    | 0x5D => Ops.Load_r1_r2.ld_e_l
    | 0x5E => Ops.Load_r1_r2.ld_e_m_hl
    | 0x5F => Ops.Load_8_A.ld_e_a
    | 0x60 => Ops.Load_r1_r2.ld_h_b
    | 0x61 => Ops.Load_r1_r2.ld_h_c
    | 0x62 => Ops.Load_r1_r2.ld_h_d
    | 0x63 => Ops.Load_r1_r2.ld_h_e
    | 0x64 => Ops.Load_r1_r2.ld_h_h
    | 0x65 => Ops.Load_r1_r2.ld_h_l
    | 0x66 => Ops.Load_r1_r2.ld_h_m_hl
    | 0x67 => Ops.Load_8_A.ld_h_a
    | 0x68 => Ops.Load_r1_r2.ld_l_b
    | 0x69 => Ops.Load_r1_r2.ld_l_c
    | 0x6A => Ops.Load_r1_r2.ld_l_d
    | 0x6B => Ops.Load_r1_r2.ld_l_e
    | 0x6C => Ops.Load_r1_r2.ld_l_h
    | 0x6D => Ops.Load_r1_r2.ld_l_l
    | 0x6E => Ops.Load_r1_r2.ld_l_m_hl
    | 0x6F => Ops.Load_8_A.ld_l_a
    | 0x70 => Ops.Load_r1_r2.ld_m_hl_b
    | 0x71 => Ops.Load_r1_r2.ld_m_hl_c
    | 0x72 => Ops.Load_r1_r2.ld_m_hl_d
    | 0x73 => Ops.Load_r1_r2.ld_m_hl_e
    | 0x74 => Ops.Load_r1_r2.ld_m_hl_h
    | 0x75 => Ops.Load_r1_r2.ld_m_hl_l
    | 0x76 => Ops.Misc.halt
    | 0x77 => Ops.Load_8_A.ld_m_hl_a
    | 0x78 => Ops.Load_A_8.ld_a_b
    | 0x79 => Ops.Load_A_8.ld_a_c
    | 0x7A => Ops.Load_A_8.ld_a_d
    | 0x7B => Ops.Load_A_8.ld_a_e
    | 0x7C => Ops.Load_A_8.ld_a_h
    | 0x7D => Ops.Load_A_8.ld_a_l
    | 0x7E => Ops.Load_A_8.ld_a_m_hl
    | 0x7F => Ops.Load_8_A.ld_a_a
    | 0x80 => Ops.Add.add_a_b
    | 0x81 => Ops.Add.add_a_c
    | 0x82 => Ops.Add.add_a_d
    | 0x83 => Ops.Add.add_a_e
    | 0x84 => Ops.Add.add_a_h
    | 0x85 => Ops.Add.add_a_l
    | 0x86 => Ops.Add.add_a_m_hl
    | 0x87 => Ops.Add.add_a_a
    | 0x88 => Ops.Add.adc_a_b
    | 0x89 => Ops.Add.adc_a_c
    | 0x8A => Ops.Add.adc_a_d
    | 0x8B => Ops.Add.adc_a_e
    | 0x8C => Ops.Add.adc_a_h
    | 0x8D => Ops.Add.adc_a_l
    | 0x8E => Ops.Add.adc_a_m_hl
    | 0x8F => Ops.Add.adc_a_a
    | 0x90 => Ops.Sub.sub_b
    | 0x91 => Ops.Sub.sub_c
    | 0x92 => Ops.Sub.sub_d
    | 0x93 => Ops.Sub.sub_e
    | 0x94 => Ops.Sub.sub_h
    | 0x95 => Ops.Sub.sub_l
    | 0x96 => Ops.Sub.sub_m_hl
    | 0x97 => Ops.Sub.sub_a
    | 0x98 => Ops.Sub.sbc_b
    | 0x99 => Ops.Sub.sbc_c
    | 0x9A => Ops.Sub.sbc_d
    | 0x9B => Ops.Sub.sbc_e
    | 0x9C => Ops.Sub.sbc_h
    | 0x9D => Ops.Sub.sbc_l
    | 0x9E => Ops.Sub.sbc_m_hl
    | 0x9F => Ops.Sub.sbc_a
    | 0xA0 => Ops.And.and_b
    | 0xA1 => Ops.And.and_c
    | 0xA2 => Ops.And.and_d
    | 0xA3 => Ops.And.and_e
    | 0xA4 => Ops.And.and_h
    | 0xA5 => Ops.And.and_l
    | 0xA6 => Ops.And.and_m_hl
    | 0xA7 => Ops.And.and_a
    | 0xA8 => Ops.Xor.xor_b
    | 0xA9 => Ops.Xor.xor_c
    | 0xAA => Ops.Xor.xor_d
    | 0xAB => Ops.Xor.xor_e
    | 0xAC => Ops.Xor.xor_h
    | 0xAD => Ops.Xor.xor_l
    | 0xAE => Ops.Xor.xor_m_hl
    | 0xAF => Ops.Xor.xor_a
    | 0xB0 => Ops.Or.or_b
    | 0xB1 => Ops.Or.or_c
    | 0xB2 => Ops.Or.or_d
    | 0xB3 => Ops.Or.or_e
    | 0xB4 => Ops.Or.or_h
    | 0xB5 => Ops.Or.or_l
    | 0xB6 => Ops.Or.or_m_hl
    | 0xB7 => Ops.Or.or_a
    | 0xB8 => Ops.Cp.cp_b
    | 0xB9 => Ops.Cp.cp_c
    | 0xBA => Ops.Cp.cp_d
    | 0xBB => Ops.Cp.cp_e
    | 0xBC => Ops.Cp.cp_h
    | 0xBD => Ops.Cp.cp_l
    | 0xBE => Ops.Cp.cp_m_hl
    | 0xBF => Ops.Cp.cp_a
    | 0xC0 => Ops.Ret.ret_nz
    | 0xC1 => Ops.Pop.pop_bc
    | 0xC2 => Ops.Jump.jp_nz_nn
    | 0xC3 => Ops.Jump.jp_nn
    | 0xC4 => Ops.Call.call_nz_nn
    | 0xC5 => Ops.Push.push_bc
    | 0xC6 => Ops.Add.add_a_n
    /* | 0xC7 => Ops.nop */
    | 0xC8 => Ops.Ret.ret_z
    | 0xC9 => Ops.Ret.ret
    | 0xCA => Ops.Jump.jp_z_nn
    | 0xCB => Ops.Cb.cb_exec
    /* | 0xCC => Ops.nop */
    | 0xCD => Ops.Call.call_nn
    | 0xCE => Ops.Add.adc_a_n
    /* | 0xCF => Ops.nop */
    | 0xD0 => Ops.Ret.ret_nc
    | 0xD1 => Ops.Pop.pop_de
    | 0xD2 => Ops.Jump.jp_nc_nn
    /* | 0xD3 => Ops.nop
       | 0xD4 => Ops.nop */
    | 0xD5 => Ops.Push.push_de
    | 0xD6 => Ops.Sub.sub_n
    /* | 0xD7 => Ops.nop */
    | 0xD8 => Ops.Ret.ret_c
    /* | 0xD9 => Ops.nop */
    | 0xDA => Ops.Jump.jp_c_nn
    /* | 0xDB => Ops.nop
       | 0xDC => Ops.nop
       | 0xDD => Ops.nop
       | 0xDE => Ops.nop
       | 0xDF => Ops.nop */
    | 0xE0 => Ops.Load_8_A.ldh_n_a
    | 0xE1 => Ops.Pop.pop_hl
    | 0xE2 => Ops.Load_8_A.ld_m_c_a
    /* | 0xE3 => Ops.nop
       | 0xE4 => Ops.nop */
    | 0xE5 => Ops.Push.push_hl
    | 0xE6 => Ops.And.and_n
    /* | 0xE7 => Ops.nop */
    | 0xE8 => Ops.Add_16.add_sp_n
    | 0xE9 => Ops.Jump.jp_m_hl
    | 0xEA => Ops.Load_8_A.ld_m_nn_a
    /* | 0xEB => Ops.nop
       | 0xEC => Ops.nop
       | 0xED => Ops.nop */
    | 0xEE => Ops.Xor.xor_n
    /* | 0xEF => Ops.nop */
    | 0xF0 => Ops.Load_A_8.ldh_a_n
    | 0xF1 => Ops.Pop.pop_af
    | 0xF2 => Ops.Load_A_8.ld_a_m_c
    | 0xF3 => Ops.Interrupts.di
    /* | 0xF4 => Ops.nop  */
    | 0xF5 => Ops.Push.push_af
    | 0xF6 => Ops.Or.or_n
    /* | 0xF7 => Ops.nop */
    | 0xF8 => Ops.Load_nn_16.ld_hl_sp_n
    | 0xF9 => Ops.Load_nn_16.ld_sp_hl
    | 0xFA => Ops.Load_A_8.ld_a_m_nn
    | 0xFB => Ops.Interrupts.ei
    /*   | 0xFC => Ops.nop
         | 0xFD => Ops.nop */
    | 0xFE => Ops.Cp.cp_n
    /* | 0xFF => Ops.nop */
    | _ =>
      raise(
        Cpu.UnhandledInstruction(
          Printf.sprintf("Unhandled instruction, %#2x", instruction),
        ),
      )
    };
