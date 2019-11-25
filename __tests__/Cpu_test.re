open Jest;

describe("Cpu", () => {
  open Expect;

  let initialValues = () => {
    (Mmu.load(Array.make(4096, 0)), Gpu.make());
  };

  describe("Signed", () => {
    testAll(
      "returns a positive number if it is < 127",
      [(1, 1), (65, 65), (127, 127), (0, 0)],
      ((num, expected)) => {
        let s = Cpu.signed(num);
        expect(s) |> toBe(expected);
      },
    );
    testAll(
      "returns a negative number if it is > 127",
      [(128, (-128)), (129, (-127))],
      ((num, expected)) => {
        let s = Cpu.signed(num);
        expect(s) |> toBe(expected);
      },
    );
  });

  describe("Ops", () => {
    describe("Increment", () => {
      let makeState = (~a=0, ~b=0, ~c=0, ~d=0, ~e=0, ()) => {
        let cpu = Cpu.make();
        let cpu = cpu |> Cpu.setRegisters(~a, ~b, ~c, ~d, ~e);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        (cpu, mmu, gpu);
      };
      describe("INC B", () => {
        test("increments register b", () => {
          let (cpu, mmu, gpu) = makeState(~b=9, ());
          let cpu1 = Cpu.Ops.Increment8.inc_b({cpu, mmu, gpu}).cpu;
          expect(cpu1.registers.b) |> toEqual(0x0a);
        });
        test("updates the carry flag", () => {
          let (cpu, mmu, gpu) = makeState(~b=255, ());
          let cpu1 = Cpu.Ops.Increment8.inc_b({cpu, mmu, gpu}).cpu;
          expect((cpu1.registers.b, cpu1.registers.f))
          |> toEqual((0x0, 0x0 lor 0x1 lsl 7));
        });
      });
      describe("INC C", () => {
        test("increments register c", () => {
          let (cpu, mmu, gpu) = makeState(~c=9, ());
          let cpu1 = Cpu.Ops.Increment8.inc_c({cpu, mmu, gpu}).cpu;
          expect(cpu1.registers.c) |> toEqual(0x0a);
        });
        test("updates the carry flag", () => {
          let (cpu, mmu, gpu) = makeState(~c=255, ());
          let cpu1 = Cpu.Ops.Increment8.inc_c({cpu, mmu, gpu}).cpu;
          expect((cpu1.registers.c, cpu1.registers.f))
          |> toEqual((0x0, 0x0 lor 0x1 lsl 7));
        });
      });
      test("INC BC", () => {
        let (cpu, mmu, gpu) = makeState(~c=255, ());
        let cpu1 = Cpu.Ops.Increment16.inc_bc({mmu, gpu, cpu}).cpu;
        expect((Cpu.rBc(cpu1), cpu1.registers.b, cpu1.registers.c))
        |> toEqual((256, 0x1, 0x0));
      });
      test("INC DE", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~e=255);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Increment16.inc_de({mmu, gpu, cpu}).cpu;
        expect((Cpu.rDe(cpu1), cpu1.registers.d, cpu1.registers.e))
        |> toEqual((256, 0x1, 0x0));
      });
      test("INC HL", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~l=255);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Increment16.inc_hl({mmu, gpu, cpu}).cpu;
        expect((Cpu.rHl(cpu1), cpu1.registers.h, cpu1.registers.l))
        |> toEqual((256, 0x1, 0x0));
      });
    });

    describe("Decrement", () => {
      test("DEC BC", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~b=1, ~c=0);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Decrement16.dec_bc({mmu, gpu, cpu}).cpu;
        expect((Cpu.rBc(cpu1), cpu1.registers.b, cpu1.registers.c))
        |> toEqual((255, 0x0, 0xff));
      });
      test("DEC DE", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~d=1, ~e=0);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Decrement16.dec_de({mmu, gpu, cpu}).cpu;
        expect((Cpu.rDe(cpu1), cpu1.registers.d, cpu1.registers.e))
        |> toEqual((255, 0x0, 0xff));
      });
      test("DEC HL", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~h=1, ~l=0);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Decrement16.dec_hl({mmu, gpu, cpu}).cpu;
        expect((Cpu.rHl(cpu1), cpu1.registers.h, cpu1.registers.l))
        |> toEqual((255, 0x0, 0xff));
      });
    });

    describe("Add", () => {
      test("updates the register", () => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~h=1, ~l=0, ~b=2, ~c=0xdc);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();

        let cpu1 = Cpu.Ops.Add_16.add_hl_bc({cpu, mmu, gpu}).cpu;
        expect(Cpu.rHl(cpu1)) |> toEqual(0x3dc);
      });
      test("sets the half-carry flag", () => {
        let cpu =
          Cpu.make()
          |> Cpu.writeRegister16(HL, 0b00001110)
          |> Cpu.writeRegister16(BC, 0b00000011);

        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();

        let cpu1 = Cpu.Ops.Add_16.add_hl_bc({cpu, mmu, gpu}).cpu;
        expect(Cpu.getFlag(H, cpu1)) |> toEqual(0b00100000);
      });
      test("sets the carry flag", () => {
        let cpu =
          Cpu.make()
          |> Cpu.writeRegister16(HL, 0xffff)
          |> Cpu.writeRegister16(BC, 120);

        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();

        let cpu1 = Cpu.Ops.Add_16.add_hl_bc({cpu, mmu, gpu}).cpu;
        expect(Cpu.getFlag(C, cpu1)) |> toEqual(0b00010000);
      });
    });

    describe("Push/Pop", () => {
      let initialValues = () => {
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        (mmu, gpu);
      };

      let generateTest =
          (description, register, push: Cpu.Ops.op, pop: Cpu.Ops.op, read) => {
        let addr = 0x8500;

        describe(
          description,
          () => {
            test("Push decrements the stack pointer", () => {
              let (mmu, gpu) = initialValues();
              let cpu =
                Cpu.make()
                |> Cpu.writeRegister16(register, 10)
                |> Cpu.setSp(addr);

              let s = push({cpu, mmu, gpu});
              expect(s.cpu.registers.sp) |> toBe(addr - 2);
            });
            test("Pop increments the stack pointer", () => {
              let (mmu, gpu) = initialValues();
              let cpu =
                Cpu.make()
                |> Cpu.writeRegister16(register, 10)
                |> Cpu.setSp(addr);

              let s = pop({cpu, mmu, gpu});
              expect(s.cpu.registers.sp) |> toBe(addr + 2);
            });
            test("Pushes and pops the value", () => {
              let (mmu, gpu) = initialValues();
              let cpu =
                Cpu.make()
                |> Cpu.writeRegister16(register, 10)
                |> Cpu.setSp(addr);

              let s = push({cpu, mmu, gpu});
              let cpu = s.cpu |> Cpu.writeRegister16(register, 0xffff);
              let cpu = pop({...s, cpu}).cpu;

              let (value, sp) = (read(cpu), cpu.registers.sp);

              expect((value, sp)) |> toEqual((10, addr));
            });
          },
        );
      };

      generateTest(
        "AF",
        Cpu.AF,
        Cpu.Ops.Push.push_af,
        Cpu.Ops.Pop.pop_af,
        Cpu.rAf,
      );
      generateTest(
        "BC",
        Cpu.BC,
        Cpu.Ops.Push.push_bc,
        Cpu.Ops.Pop.pop_bc,
        Cpu.rBc,
      );
      generateTest(
        "DE",
        Cpu.DE,
        Cpu.Ops.Push.push_de,
        Cpu.Ops.Pop.pop_de,
        Cpu.rDe,
      );
      generateTest(
        "HL",
        Cpu.HL,
        Cpu.Ops.Push.push_hl,
        Cpu.Ops.Pop.pop_hl,
        Cpu.rHl,
      );
    });

    describe("AND", () => {
      let initialValues = () => (Mmu.load(Array.make(4096, 0)), Gpu.make());
      let a = 0b01010101;
      let b = 0b10001001;

      test("A", () => {
        let (mmu, gpu) = initialValues();
        let cpu = Cpu.make() |> Cpu.setRegisters(~a);
        let cpu1 = Cpu.Ops.And.and_a({cpu, mmu, gpu}).cpu;

        expect(cpu1.registers.a) |> toBe(a land a);
      });
      test("B", () => {
        let (mmu, gpu) = initialValues();
        let cpu = Cpu.make() |> Cpu.setRegisters(~a, ~b);
        let cpu1 = Cpu.Ops.And.and_b({cpu, mmu, gpu}).cpu;

        expect(cpu1.registers.a) |> toBe(a land b);
      });
      test("C", () => {
        let (mmu, gpu) = initialValues();
        let cpu = Cpu.make() |> Cpu.setRegisters(~a, ~c=b);
        let cpu1 = Cpu.Ops.And.and_c({cpu, mmu, gpu}).cpu;

        expect(cpu1.registers.a) |> toBe(a land b);
      });
    });

    describe("LD_B_N", () => {
      let initialPc = 0xE000;

      test("B", () => {
        let (mmu, gpu) = initialValues();
        let cpu = Cpu.make() |> Cpu.setPc(initialPc);
        let (mmu, gpu) = Mmu.write8(initialPc, 64, {mmu, gpu});

        let cpu1 = Cpu.Ops.Load_nn_8.ld_b_n({cpu, mmu, gpu}).cpu;
        let cpu2 = Cpu.Ops.Load_nn_8.load_nn_8(C, {cpu, mmu, gpu}).cpu;

        expect(cpu1) |> toEqual(cpu2);
      });
      test("B", () => {
        let (mmu, gpu) = initialValues();
        let cpu = Cpu.make() |> Cpu.setPc(initialPc);
        let (mmu, gpu) = Mmu.write8(initialPc, 64, {mmu, gpu});

        let cpu1 = Cpu.Ops.Load_nn_8.ld_b_n({cpu, mmu, gpu}).cpu;

        expect(cpu1.registers.b) |> toEqual(64);
      });
      test("PC", () => {
        let (mmu, gpu) = initialValues();
        let cpu = Cpu.make() |> Cpu.setPc(initialPc);

        let cpu1 = Cpu.Ops.Load_nn_8.load_nn_8(C, {cpu, mmu, gpu}).cpu;

        expect(cpu1.registers.pc) |> toEqual(initialPc + 1);
      });
    });

    testAll(
      "RLCA",
      [
        (0b10001000, 0b00010001, 0b00010000),
        (0b11111111, 0b11111111, 0b00010000),
        (0b00000001, 0b00000010, 0b00000000),
      ],
      ((input, expected, carry)) => {
        let cpu = Cpu.make() |> Cpu.setRegisters(~a=input);
        let mmu = Mmu.load(Array.make(4096, 0));
        let gpu = Gpu.make();
        let cpu1 = Cpu.Ops.Rotation.rlca({cpu, mmu, gpu}).cpu;
        expect((cpu1.registers.a, Cpu.getFlag(Cpu.Flags.C, cpu1)))
        |> toEqual((expected, carry));
      },
    );
  });
});
