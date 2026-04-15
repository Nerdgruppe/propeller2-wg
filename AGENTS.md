# Propeller 2 Working Group

This repository contains several projects related to the Parallax Propeller 2.

It's a unique microcontroller with a one-of-a-kind architecture:

- 8 CPU cores (called "Cogs")
    - Eight powerful 32-bit processors:
    - Access to all I/O pins, plus four fast DAC output channels and four fast ADC input channels
    - 512 longs of dual-port register RAM for code and fast variables
    - 512 longs of dual-port lookup RAM for code, streamer lookup, and variables
    - Ability to execute code directly from register RAM, lookup RAM, and hub RAM
    - ~350 unique instructions for math, logic, timing, and control operations
    - 2-clock execution for all math and logic instructions, including 16 x 16 multiply
    - 6-clock custom-bytecode executor for interpreted languages
    - Ability to stream hub RAM and/or lookup RAM to DACs and pins or HDMI modulator
    - Ability to stream pins and/or ADCs to hub RAM
    - Live colorspace conversion using a 3 x 3 matrix with 8-bit signed/unsigned coefficients
    - Pixel blending instructions for 8:8:8:8 data
    - 16 unique event trackers that can be polled and waited upon
    - 3 prioritized interrupts that trigger on selectable events
    - Hidden debug interrupt for single-stepping, breakpoint, and polling
- The shared memory and data fabric called "Hub"
  - 512 KB of contiguous RAM in a 20-bit address space
    - 32-bits-per-clock sequential read/write for all cogs, simultaneously readable and writable as bytes, words, or longs in little-endian format
    - last 16KB of RAM is write-protectable
  - 32-bit, pipelined CORDIC solver with scale-factor correction
    - 32-bit x 32-bit unsigned multiply with 64-bit result
    - 64-bit / 32-bit unsigned divide with 32-bit quotient and 32-bit remainder
    - 64-bit → 32-bit square root
    - Rotate (X32,Y32) by Theta32 → (X32,Y32)
    - (Rho32,Theta32) → (X32,Y32) polar-to-cartesian
    - (X32,Y32) → (Rho32,Theta32) cartesian-to-polar
    - 32 → 5.27 unsigned-to-logarithm
    - 5.27 → 32 logarithm-to-unsigned
    - Cogs can start CORDIC operations every 8 clocks and get results 55 clocks later
  - 16 semaphore bits with atomic read-modify-write operations
  - 64-bit free-running counter which increments every clock, cleared on reset
  - High-quality pseudo-random number generator (Xoroshiro128**), true-random seeded at start-up, updates every clock, provides unique data to each cog and pin
- 64 identical Smart I/O pins, externally powered in blocks of 4
  - 8-bit, 120-ohm (3 ns) and 1 k-ohm DACs with 16-bit oversampling, noise, and high/low digital modes
  - Delta-sigma ADC with 5 ranges, 2 sources, and VIO/GIO calibration
  - Several ADC sampling modes: automatic 2n SINC2, adjustable SINC2/SINC3, oscilloscope
  - Logic, Schmitt, pin-to-pin-comparator, and 8-bit-level-comparator input modes
  - 2/3/5/8-bit-unanimous input filtering with selectable sample rate
  - Incorporation of inputs from relative pins, -3 to +3
  - Negative or positive local feedback, with or without clocking
  - Separate drive modes for high and low output: logic / 1.5 k  / 15 k / 150 k / 1 mA / 100 µA / 10 µA / float
  - Programmable 32-bit clock output, transition output, NCO/duty output
  - Triangle/sawtooth/SMPS PWM output, 16-bit frame with 16-bit prescaler
  - Quadrature decoding with 32-bit counter, both position and velocity modes
  - 16 different 32-bit measurements involving one or two signals
  - USB full-speed and low-speed (via odd/even pin pairs)
  - Synchronous serial transmit and receive, 1 to 32 bits, up to clock/2 baud rate
  - Asynchronous serial transmit and receive, 1 to 32 bits, up to clock/3 baud rate

## Projects

The following projects are managed in this repository:

- [P2DB](data/p2db/) - A formalized semantic instruction database
- [Propan](src/propan/) - A new, modern assembler for the Propeller 2
- [Turboprop](src/turboprop/) - A tiny application loader that allows launching programs on the Propeller 2 over serial port
- [Windtunnel](src/windtunnel/) - A clock cycle-exact simulator for the Propeller 2
- [VsCode Extension](vscode-extension/) - A Visual Studio Code extension meant to support the other projects

## Propeller 2 Documentation

The following official documentation is shipped inside the repository:

- [Datasheet](docs/p2/P2X8C4M64P_datasheet.pdf)
- [Instruction Set Summary](docs/p2/Instructions.xlsx)
- [Hardware Manual](docs/p2/Hardware_Manual.docx)
- [PASM2 Manual](docs/p2/PASM2_Manual.docx)
- [Silicon Documentation](docs/p2/Silicon_Documentation.docx)
- [SPIN2 Documentation](docs/p2/Spin2_Documentation.docx)
- [SPIN2 Manual](docs/p2/Spin2_Manual.docx)
- [Questions & Answers](docs/p2/Questions_and_Answers.xlsx)

The following community documentation is available as well:

- [p2docs](docs/p2docs.github.io/)
- [Smart Pins Documentation](docs/community/Smart_Pins.docx)

## Working in the Repository

### General Rules

There's a `justfile` in the workspace root which has these recipies:

- `just validate`: Executes all validation tasks.
- `just build`: Builds the Zig based software
- `just validate-p2db`: Validates the P2DB schema and database
- `just update-windtunnel`: Regenerates
  - `src/windtunnel/sim/encoding.zig` with `utility/gen_windtunnel.py encoding ...`.
  - `src/windtunnel/sim/decode.zig` with `utility/gen_windtunnel.py decoder ...`.
- `regenerate-windtunnel-executor`: Regenerates the contents of `src/windtunnel/sim/execute.zig` with `utility/gen_windtunnel.py executor ...`. Use with care!
- `just setup-venv`: Sets up a virtual environment in the repository. Do not use unless explicitly asked to!

### Zig-based Projects

Propan, Turboprop and Windtunnel are implemented in Zig (see `build.zig.zon` for required version).

Typical workflows (started from the workspace root):

- `${zig} build install test`: Build the projects and run their test suites.
- `${zig} build install -Dno-emit-bin`: Compile the projects, but don't write the output. This is a quick syntax check.

### P2DB

The P2DB is written in CUE and located in `data/p2db`.

Typical workflows in that folder:

- `cue vet isa.cue`: Check the syntax of the schema for errors.
- `cue vet -c isa.cue examples.yml`: Check the example file for errors.

