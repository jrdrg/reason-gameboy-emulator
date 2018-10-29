open Jest;

describe("Cpu", () =>
  Expect.(
    describe("Flags", () => {
      open Cpu.Flags;
      testAll(
        "Setting flags",
        [
          (Z, 0b10000000),
          (N, 0b01000000),
          (H, 0b00100000),
          (C, 0b00010000),
        ],
        ((flag, expected)) => {
          let cpu = Cpu.make() |> Cpu.setFlag(flag, 1);
          expect(cpu |> Cpu.getFlag(flag)) |> toBe(expected);
        },
      );
      test("Setting a flag with an initial value", () => {
        let initialValue = 0b00010000;
        let cpu = Cpu.make() |> Cpu.setFlag(Z, 1, ~initialValue);
        expect(cpu.registers.f) |> toBe(0b10010000);
      });
    })
  )
);
