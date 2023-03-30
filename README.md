# sz81_2_1_8
sz81 is an excellent zx81 emulator, which supports multiple OS. Currently it is at version 2.3.12. The latest version can be found [here](https://github.com/SegHaxx/sz81)

This repository is based on an obsolete version of the sz81 ZX81 emulator (version 2.1.8). Version 2.1.8 was the first version to support Hires graphics, and the last version (excluding 2.1.9 which did not support hires) before sz81 was moved to use the Z80 core and pixel generation from [EightyOne](https://github.com/charlierobson/EightyOne)

The EightyOne core has delivered excellent Hires graphics support into sz81. However, this comes at the cost of higher CPU load. This is not an issue for most systems, the lastest sz81 will run perfectly on a Raspberry Pi.

The core code in sz81 version 2.3.x is too CPU intensive to run at 100% emulation speed on a very low specification MCU, such as the ARM M0+ based Raspberry Pi Pico.

The core in version 2.1.8 will run at 100% speed on an overclocked Pi Pico. This repository is used to experiment with improving the Hires graphics support in the 2.1.8 core, whilst still being able to run at 100% emulation speed on a single core of an overclocked Raspberry Pi Pico.

Note that the experiments are on branches, not main.

