open Jest;

describe("Cpu Flags", () => {
  open Expect;
  open Cpu.Flags;

  testAll(
    "Setting flags",
    [(Z, 0b10000000), (N, 0b01000000), (H, 0b00100000), (C, 0b00010000)],
    ((flag, expected)) => {
      let cpu = Cpu.make() |> Cpu.setFlag(flag, 1);
      expect(cpu |> Cpu.getFlag(flag)) |> toBe(expected);
    },
  );
  testAll(
    "Setting a flag with an initial value",
    [
      (Z, 0b00010000, 0b10010000),
      (N, 0b00110000, 0b01110000),
      (H, 0b11010000, 0b11110000),
      (C, 0b11000000, 0b11010000),
    ],
    ((flag, initialValue, expected)) => {
      // let initialValue = 0b00010000;
      let cpu = Cpu.make() |> Cpu.setFlag(flag, 1, ~initialValue);
      expect(cpu.registers.f) |> toBe(expected);
    },
  );
  testAll(
    "Getting a flag's value",
    [
      (Z, 0b10010000, 1),
      (Z, 0b01100000, 0),
      (C, 0b11110000, 1),
      (C, 0b10100000, 0),
    ],
    ((flag, value, expected)) => {
      let cpu = Cpu.make() |> Cpu.setRegisters(~f=value);
      expect(cpu.registers.f |> getFlag(flag)) |> toBe(expected);
    },
  );
});
