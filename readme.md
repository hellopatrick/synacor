## Hello, Synacor Challenge.

My attempt at solving the [Synacor Challenge](https://challenge.synacor.com) in OCaml.

Instructions:
- Build with `jbuilder build`.
- Run with `jbuilder exec synacor`.

Custom Commands:
- `debug`: prints out all vm instructions.
- `save_check`: saves current memory to `./dump.txt`.
- `load_check`: loads `./dump.txt` as current memory state.

TODO:
- Add support for named save/load checkpoints.
- Command for modifying registers.
- Locate the teleporter's confirmation code.
- Optimize the teleporter's confirmation code.
- Bypass confirmation code.
